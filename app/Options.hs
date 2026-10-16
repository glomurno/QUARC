{-# LANGUAGE OverloadedStrings #-}

module Options where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.FilePath (takeBaseName)

import LqCCS.Bisimulation (Equivalence (..), Rooting (..), TagMode (..), equivalenceFromName, equivalenceName, tagModeFromName, tagModeName)
import LqCCS.Tagging (TagPolicy (..), defaultTagPolicy, tagPolicyFromName, tagPolicyName)
import LqCCS.Types (Silence (..), Tag (..))

data ProcInput = StdIn | InputFile !FilePath deriving (Eq, Show)

data ProcOutput = StdOut | OutputFile !FilePath deriving (Eq, Show)

data Flag = Simplify | NoNames | OnlyDots | Sugar | PrintLts deriving (Eq, Show)

data Format = Png | Svg | Json deriving (Eq, Show)

-- | Which parts of the input file @check@ reports on.
data CheckTarget
    = -- | Every process declaration and every start block.
      CheckAll
    | -- | Only the process declarations.
      CheckProcesses
    | -- | Only the named start block.
      CheckStart !String
    deriving (Eq, Show)

data CheckOptions = CheckOptions
    { checkInput :: !ProcInput
    , checkJson :: !Bool
    , checkTarget :: !CheckTarget
    }
    deriving (Eq, Show)

data EvalOptions = EvalOptions
    { evalInput :: !ProcInput
    , evalOutput :: !ProcOutput
    , evalFlags :: ![Flag]
    , evalFormat :: !Format
    , evalConfig :: !(Maybe String)
    , evalTagging :: !TagPolicy
    }
    deriving (Eq, Show)

data MinimizeOptions = MinimizeOptions
    { minimizeInput :: !ProcInput
    , minimizeOutput :: !ProcOutput
    , minimizeFlags :: ![Flag]
    , minimizeFormat :: !Format
    , minimizeTagEquiv :: ![(Tag, Tag)]
    , minimizeTagMode :: !TagMode
    , minimizeEquivalence :: !Equivalence
    , minimizeSilence :: !Silence
    , minimizeConfig :: !(Maybe String)
    , minimizeTagging :: !TagPolicy
    }
    deriving (Eq, Show)

-- | How to pick the two processes for the equivalence check.
data EquivSource
    = -- | Two separate input files; each file's default start block is used.
      TwoFiles !FilePath !FilePath
    | -- | A single file with two named start blocks.
      NamedConfigs !FilePath !String !String
    deriving (Eq, Show)

data EquivOptions = EquivOptions
    { equivSource :: !EquivSource
    , equivOutput :: !ProcOutput
    , equivFlags :: ![Flag]
    , equivTagEquiv :: ![(Tag, Tag)]
    , equivTagMode :: !TagMode
    , equivEquivalence :: !Equivalence
    , equivRooting :: !Rooting
    , equivSilence :: !Silence
    , equivFormat :: !Format
    , equivTagging :: !TagPolicy
    }
    deriving (Eq, Show)

{- | Options for the JSON-RPC. Currently parameterless; reserved for
future transport options.
-}
data ServeOptions = ServeOptions
    deriving (Eq, Show)

data Command
    = Check CheckOptions
    | Eval EvalOptions
    | Minimize MinimizeOptions
    | Equiv EquivOptions
    | Serve ServeOptions
    deriving (Eq, Show)

-- | Global options shared by every subcommand, controlling logging output.
data GlobalOpts = GlobalOpts
    { goVerbose :: !Int
    -- ^ Number of @-v@ flags supplied (raises verbosity).
    , goQuiet :: !Bool
    -- ^ Suppress everything but errors.
    , goLogFile :: !(Maybe FilePath)
    -- ^ Write logs here instead of stderr.
    , goTiming :: !Bool
    -- ^ Emit per-phase wall-clock timings.
    }
    deriving (Eq, Show)

-- | A full CLI invocation: global logging options plus the chosen command.
data Invocation = Invocation !GlobalOpts !Command
    deriving (Eq, Show)

globalOpts :: Parser GlobalOpts
globalOpts =
    ( GlobalOpts . length
        <$> many
            ( flag'
                ()
                ( mconcat
                    [ short 'v'
                    , long "verbose"
                    , help "Increase log verbosity (repeatable: -v info, -vv debug, -vvv trace)"
                    ]
                )
            )
    )
        <*> switch
            ( mconcat
                [ short 'q'
                , long "quiet"
                , help "Only report errors"
                ]
            )
        <*> optional
            ( strOption
                ( mconcat
                    [ long "log-file"
                    , metavar "PATH"
                    , help "Append logs to PATH instead of stderr"
                    , action "file"
                    ]
                )
            )
        <*> switch
            ( mconcat
                [ long "timing"
                , help "Print per-phase wall-clock timings"
                ]
            )

inputOpt' :: Parser ProcInput
inputOpt' =
    InputFile
        <$> strArgument
            ( mconcat
                [ metavar "INPUT_FILE"
                , help "Input file or stdin if empty"
                , action "file"
                ]
            )
            <|> pure StdIn

inputOpt :: Parser FilePath
inputOpt =
    strArgument $
        mconcat
            [ metavar "INPUT_FILE"
            , help "Input file"
            , action "file"
            ]

outputOpt :: Parser (Maybe ProcOutput)
outputOpt =
    optional $
        OutputFile
            <$> strOption
                ( mconcat
                    [ short 'o'
                    , long "output"
                    , metavar "OUTPUT_NAME"
                    , help "Output file name without extension (default: <input>_<command>, in the current directory)"
                    ]
                )
                <|> flag'
                    StdOut
                    ( mconcat
                        [ long "stdout"
                        , help "Write the output to stdout instead of a file"
                        ]
                    )

defaultOutput :: String -> [FilePath] -> Maybe ProcOutput -> ProcOutput
defaultOutput cmd inputs = fromMaybe (OutputFile (intercalate "_" (map takeBaseName inputs ++ [cmd])))

inputFiles :: ProcInput -> [FilePath]
inputFiles StdIn = []
inputFiles (InputFile path) = [path]

flagsOpt :: Parser [Flag]
flagsOpt =
    many $
        option
            (maybeReader flagStr)
            ( mconcat
                [ short 'f'
                , long "flag"
                , metavar "OPTION_FLAG"
                , help "Possible values: simplify_lts, no_names, only_dots, sugar, print_lts"
                , completeWith ["simplify_lts", "no_names", "only_dots", "sugar", "print_lts"]
                ]
            )
  where
    flagStr :: String -> Maybe Flag
    flagStr "simplify_lts" = Just Simplify
    flagStr "no_names" = Just NoNames
    flagStr "only_dots" = Just OnlyDots
    flagStr "sugar" = Just Sugar
    flagStr "print_lts" = Just PrintLts
    flagStr _ = Nothing

formatOpt :: Parser Format
formatOpt =
    option
        (maybeReader formatStr)
        ( mconcat
            [ long "format"
            , metavar "EXT"
            , value Png
            , help "Possible values: png, svg, json"
            , completeWith ["png", "svg", "json"]
            ]
        )
  where
    formatStr :: String -> Maybe Format
    formatStr "png" = Just Png
    formatStr "svg" = Just Svg
    formatStr "json" = Just Json
    formatStr _ = Nothing

checkOpts :: Parser Command
checkOpts =
    Check
        <$> ( CheckOptions
                <$> inputOpt'
                <*> switch
                    ( mconcat
                        [ long "json"
                        , help "Emit diagnostics as quarc.lts/1 JSON instead of human-readable text"
                        ]
                    )
                <*> checkTargetOpt
            )

checkTargetOpt :: Parser CheckTarget
checkTargetOpt =
    flag'
        CheckProcesses
        ( mconcat
            [ long "processes"
            , help "Only check process declarations, skipping start blocks"
            ]
        )
        <|> ( CheckStart
                <$> strOption
                    ( mconcat
                        [ long "config"
                        , metavar "CONFIG"
                        , help "Only check the start block with this name"
                        ]
                    )
            )
        <|> pure CheckAll

configOpt :: Parser (Maybe String)
configOpt =
    optional $
        strOption $
            mconcat
                [ long "config"
                , metavar "CONFIG"
                , help "Name of start block to evaluate"
                ]

taggingOpt :: Parser TagPolicy
taggingOpt =
    option
        (maybeReader tagPolicyFromName)
        ( mconcat
            [ long "tagging"
            , metavar "branch|linear"
            , value defaultTagPolicy
            , showDefaultWith tagPolicyName
            , help "How untagged actions are tagged: one tag per linear sequence, fresh in each branch of || and + (branch), or a fresh tag for every action (linear)"
            , completeWith (tagPolicyName <$> [minBound .. maxBound :: TagPolicy])
            ]
        )

evalOpts :: Parser Command
evalOpts = Eval <$> (withOutput <$> inputOpt' <*> outputOpt <*> flagsOpt <*> formatOpt <*> configOpt <*> taggingOpt)
  where
    withOutput i o = EvalOptions i (defaultOutput "eval" (inputFiles i) o)

parseTag :: String -> Either String Tag
parseTag s = case reads s of
    [(n, "")] -> Right (ActionTag n)
    _ -> case span (/= ',') s of
        (a, ',' : b) -> case (reads a, reads b) of
            ([(x, "")], [(y, "")]) -> Right (SynchTag (min x y) (max x y))
            _ -> Left ("not a valid tag: " ++ s)
        _ -> Left ("not a valid tag: " ++ s)

parseTagPair :: String -> Either String (Tag, Tag)
parseTagPair s = case break (== '=') s of
    (a, '=' : b) -> (,) <$> parseTag a <*> parseTag b
    _ -> Left ("expected A=B, got: " ++ s)

parseTagPairs :: String -> Either String [(Tag, Tag)]
parseTagPairs s = mapM parseTagPair (words s)

tagEquivOpt :: Parser [(Tag, Tag)]
tagEquivOpt =
    fmap concat $
        many $
            option
                (eitherReader parseTagPairs)
                ( mconcat
                    [ long "tag-equiv"
                    , metavar "A=B [A=B ...]"
                    , help "Declare tags A and B equivalent for bisimilarity (repeatable). A is an integer n or a synch pair n,m."
                    ]
                )

tagModeOpt :: Parser TagMode
tagModeOpt =
    option
        (maybeReader tagModeFromName)
        ( mconcat
            [ long "tags"
            , metavar "file|demonic|angelic"
            , value FileTags
            , showDefaultWith tagModeName
            , help "How tags are identified: the tag equivalences of the file and --tag-equiv (file), none (demonic), or all tags equal (angelic)"
            ]
        )

equivalenceOpt :: Parser Equivalence
equivalenceOpt =
    option
        (maybeReader equivalenceFromName)
        ( mconcat
            [ long "equivalence"
            , metavar "strong|branching"
            , value Strong
            , showDefaultWith equivalenceName
            , help "Behavioural equivalence to check: strong bisimilarity, or branching bisimilarity (tau steps are silent)"
            ]
        )

parseTags :: String -> Either String [Tag]
parseTags = mapM (parseTag . filter (`notElem` ("()" :: String))) . words

tagListOpt :: String -> String -> Parser [Tag]
tagListOpt name desc =
    fmap concat $
        many $
            option
                (eitherReader parseTags)
                ( mconcat
                    [ long name
                    , metavar "TAG [TAG ...]"
                    , help desc
                    ]
                )

silenceOpt :: Parser Silence
silenceOpt =
    Silence
        <$> tagListOpt "silent" "Only these tau tags are silent under branching bisimilarity (repeatable; default: every tau tag). A tag is n or a synch pair n,m."
        <*> tagListOpt "visible" "Keep these tau tags observable under branching bisimilarity (repeatable)."

minimizeOpts :: Parser Command
minimizeOpts = Minimize <$> (withOutput <$> inputOpt' <*> outputOpt <*> flagsOpt <*> formatOpt <*> tagEquivOpt <*> tagModeOpt <*> equivalenceOpt <*> silenceOpt <*> configOpt <*> taggingOpt)
  where
    withOutput i o = MinimizeOptions i (defaultOutput "minimize" (inputFiles i) o)

equivSourceOpt :: Parser EquivSource
equivSourceOpt = source <$> inputOpt <*> (Left <$> configs <|> Right <$> inputOpt)
  where
    source f (Left (c1, c2)) = NamedConfigs f c1 c2
    source f (Right f2) = TwoFiles f f2
    configs =
        (,)
            <$> strOption
                ( mconcat
                    [ long "config1"
                    , metavar "CONFIG1"
                    , help "Name of first start block (single-file mode)"
                    ]
                )
            <*> strOption
                ( mconcat
                    [ long "config2"
                    , metavar "CONFIG2"
                    , help "Name of second start block (single-file mode)"
                    ]
                )

rootedOpt :: Parser Rooting
rootedOpt =
    flag
        Unrooted
        Rooted
        ( mconcat
            [ long "rooted"
            , help "Also require initial steps, tau included, to be matched (rooted branching bisimilarity, a congruence for choice); no effect on strong bisimilarity"
            ]
        )

equivOpts :: Parser Command
equivOpts = Equiv <$> (withOutput <$> equivSourceOpt <*> outputOpt <*> flagsOpt <*> tagEquivOpt <*> tagModeOpt <*> equivalenceOpt <*> rootedOpt <*> silenceOpt <*> formatOpt <*> taggingOpt)
  where
    withOutput src o = EquivOptions src (defaultOutput "equiv" (sourceFiles src) o)
    sourceFiles (TwoFiles f1 f2) = [f1, f2]
    sourceFiles (NamedConfigs f _ _) = [f]

serveOpts :: Parser Command
serveOpts = pure (Serve ServeOptions)

commands :: ParserInfo Invocation
commands =
    info
        ( ( Invocation
                <$> globalOpts
                <*> hsubparser
                    ( mconcat
                        [ command "check" (info checkOpts (progDesc "Typecheck every process and start block of a file"))
                        , command "eval" (info evalOpts (progDesc "Evaluate a process"))
                        , command "minimize" (info minimizeOpts (progDesc "Minimize a process semantics via strong or branching bisimilarity"))
                        , command "equiv" (info equivOpts (progDesc "Check equivalence between two processes via strong or branching bisimilarity"))
                        , command "serve" (info serveOpts (progDesc "Run the JSON-RPC sidecar over stdio"))
                        ]
                    )
          )
            <**> helper
            <**> simpleVersioner "v0.1.0.0"
        )
        idm
