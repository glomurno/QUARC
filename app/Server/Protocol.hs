{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Server.Protocol
Description : JSON-RPC 2.0 request\/response types for the lqCCS sidecar.

The wire protocol is newline-delimited JSON-RPC 2.0 (one object per line; aeson
emits no embedded newlines). Requests name a @method@ and carry @params@;
responses echo the request @id@ with a @result@ or an @error@. The payloads reuse
the @quarc.lts\/1@ contract ('Api.Contract'), so the transport is the only thing
specific to this module — the handler ('Server.Session.handle') is transport-free.
-}
module Server.Protocol (
    Progress (..),
    ReqMethod (..),
    Request (..),
    Response (..),
    Result (..),
    RpcError (..),
    Source (..),
    SourceInput (..),
    appErrorCode,
    cancelledErrorCode,
    parseErrorCode,
) where

import Control.Applicative ((<|>))
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (Null),
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, unpack)

import Api.Contract (JsonDiagnostic, LtsDocument, NodeDetail)
import LqCCS.Bisimulation (Equivalence, equivalenceFromName)
import LqCCS.Tagging (TagPolicy, defaultTagPolicy, tagPolicyFromName, tagPolicyName)
import LqCCS.Types (Silence (..), Tag (..))
import Options (Flag (..))

{- | Where to load a program from, plus how its untagged actions are tagged
(optional @tagging@: @branch@, the default, or @linear@).
-}
data Source = Source {sourceInput :: SourceInput, sourceTagging :: TagPolicy}

data SourceInput
    = SourceFile FilePath
    | SourceText String FilePath

instance FromJSON Source where
    parseJSON = withObject "source" $ \o -> do
        mfile <- o .:? "file"
        input <- case mfile of
            Just f -> pure (SourceFile f)
            Nothing -> SourceText <$> o .: "text" <*> o .: "path"
        name <- o .:? "tagging" .!= tagPolicyName defaultTagPolicy
        policy <- maybe (fail ("unknown tagging: " ++ name)) pure (tagPolicyFromName name)
        pure (Source input policy)

-- | The decoded method + params of a request.
data ReqMethod
    = ReqEval Source (Maybe String) [Flag]
    | ReqMinimize Source (Maybe String) [Flag] Equivalence (Maybe Silence)
    | ReqEquiv Source String String [Flag] (Maybe String) (Maybe Silence) Bool
    | ReqGetState Int Int
    | ReqCheck Source
    | ReqOutline Source
    | {- | Render to an image; with an output path the engine writes the file
      itself (binary-safe, no base64 round trip) instead of returning it.
      -}
      ReqExport Source (Maybe String) [Flag] String String Equivalence (Maybe Silence) (Maybe FilePath)
    | ReqSave FilePath String
    | ReqClose Int
    | {- | Cancel the in-flight request with this id. Handled by the transport
      ('Command.Serve'), which owns the worker threads.
      -}
      ReqCancel Value

-- | A JSON-RPC request: the echoed @id@ plus the decoded method.
data Request = Request {reqId :: Value, reqMethod :: ReqMethod}

instance FromJSON Request where
    parseJSON = withObject "Request" $ \o -> do
        i <- o .:? "id" .!= Null
        method <- o .: "method"
        params <- o .:? "params" .!= object []
        Request i <$> parseMethod method params

parseMethod :: Text -> Value -> Parser ReqMethod
parseMethod method = withObject "params" $ \o -> case method of
    "eval" -> ReqEval <$> o .: "source" <*> o .:? "config" <*> flags o
    "minimize" -> ReqMinimize <$> o .: "source" <*> o .:? "config" <*> flags o <*> equivalence o <*> silence o
    "equiv" -> ReqEquiv <$> o .: "source" <*> o .: "config1" <*> o .: "config2" <*> flags o <*> o .:? "bisimType" <*> silence o <*> (fromMaybe False <$> o .:? "minimized")
    "getState" -> ReqGetState <$> o .: "handle" <*> o .: "id"
    "check" -> ReqCheck <$> o .: "source"
    "outline" -> ReqOutline <$> o .: "source"
    "export" -> ReqExport <$> o .: "source" <*> o .:? "config" <*> flags o <*> o .:? "kind" .!= "eval" <*> o .:? "format" .!= "svg" <*> equivalence o <*> silence o <*> o .:? "path"
    "save" -> ReqSave <$> o .: "path" <*> o .: "text"
    "close" -> ReqClose <$> o .: "handle"
    "cancel" -> ReqCancel <$> o .: "id"
    other -> fail ("unknown method: " ++ unpack other)
  where
    flags o = parseFlags <$> (o .:? "flags" .!= [])
    -- Optional; defaults to strong bisimilarity.
    equivalence o = do
        name <- o .:? "equivalence" .!= "strong"
        maybe (fail ("unknown equivalence: " ++ name)) pure (equivalenceFromName name)
    -- Optional @{silent: [tag], visible: [tag]}@; replaces the file's declarations.
    silence o = o .:? "silence" >>= traverse (withObject "silence" (\s -> Silence <$> tags s "silent" <*> tags s "visible"))
    tags s key = s .:? key .!= [] >>= traverse parseTagJ

-- | A tag on the wire: @n@ for a single prefix, @[n, m]@ for a synchronisation.
parseTagJ :: Value -> Parser Tag
parseTagJ v = (ActionTag <$> parseJSON v) <|> (pair =<< parseJSON v)
  where
    pair [a, b] = pure (SynchTag (min a b) (max a b))
    pair _ = fail "expected a tag n or [n, m]"

-- | Map render-flag strings to 'Flag's, ignoring unknown entries.
parseFlags :: [Text] -> [Flag]
parseFlags = mapMaybe flagFromText
  where
    flagFromText t = case t of
        "simplify" -> Just Simplify
        "simplify_lts" -> Just Simplify
        "sugar" -> Just Sugar
        "no_names" -> Just NoNames
        "only_dots" -> Just OnlyDots
        "print_lts" -> Just PrintLts
        _ -> Nothing

-- | The successful payload of a response.
data Result
    = -- | An evaluated\/minimized\/compared graph: cache handle + document.
      RDocument Int LtsDocument
    | -- | Lazily fetched per-node detail (matrices).
      RDetail NodeDetail
    | -- | Editor diagnostics from @check@.
      RDiagnostics [JsonDiagnostic]
    | {- | File outline from @outline@: config names (unnamed block is 'Nothing'),
      the raw source text, and the file's tag equivalences (whether it has
      global ones, and the config pairs its scoped ones apply to).
      -}
      ROutline [Maybe String] String Bool [(String, String)]
    | -- | A rendered export from @export@: format tag + payload (e.g. SVG text).
      RExport String String
    | -- | An export written to disk by the engine: format tag + output path.
      RExportFile String FilePath
    | -- | Acknowledgement (e.g. @close@).
      ROk

resultObject :: Result -> Value
resultObject (RDocument h doc) = object ["handle" .= h, "document" .= doc]
resultObject (RDetail d) = toJSON d
resultObject (RDiagnostics ds) = object ["diagnostics" .= ds]
resultObject (ROutline cfgs src global scopes) =
    object
        [ "configs" .= cfgs
        , "source" .= src
        , "tagEquiv" .= object ["global" .= global, "scopes" .= [[c1, c2] | (c1, c2) <- scopes]]
        ]
resultObject (RExport fmt payload) = object ["format" .= fmt, "data" .= payload]
resultObject (RExportFile fmt path) = object ["format" .= fmt, "path" .= path]
resultObject ROk = object ["ok" .= True]

-- | A JSON-RPC error: code, message, and any structured diagnostics.
data RpcError = RpcError {errCode :: Int, errMessage :: String, errDiagnostics :: [JsonDiagnostic]}

-- | JSON-RPC reserved code for malformed JSON.
parseErrorCode :: Int
parseErrorCode = -32700

-- | Implementation-defined server error (parse\/typecheck\/eval failures).
appErrorCode :: Int
appErrorCode = -32000

-- | The request was cancelled by a @cancel@ (same code as LSP's RequestCancelled).
cancelledErrorCode :: Int
cancelledErrorCode = -32800

{- | A progress notification for an in-flight request (JSON-RPC notification, no
@id@ of its own): which phase it entered and, once known, the number of states.
-}
data Progress = Progress {progId :: Value, progPhase :: String, progStates :: Maybe Int}

instance ToJSON Progress where
    toJSON (Progress i phase states) =
        object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("progress" :: Text)
            , "params" .= object (["id" .= i, "phase" .= phase] ++ ["states" .= n | Just n <- [states]])
            ]

-- | A JSON-RPC response: the echoed @id@ and either a result or an error.
data Response = Response {resId :: Value, resPayload :: Either RpcError Result}

instance ToJSON Response where
    toJSON (Response i payload) =
        object $ ["jsonrpc" .= ("2.0" :: Text), "id" .= i] ++ body
      where
        body = case payload of
            Right r -> ["result" .= resultObject r]
            Left (RpcError code msg ds) ->
                [ "error"
                    .= object
                        ( ["code" .= code, "message" .= msg]
                            ++ ["data" .= object ["diagnostics" .= ds] | not (null ds)]
                        )
                ]
