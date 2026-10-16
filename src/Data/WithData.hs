{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Data.WithData
Description : Type-level heterogeneous records and metadata-annotated values.

Provides a heterogeneous record type 'Record' indexed by a type-level list.
Fields are accessed, updated, and removed by label using the 'Member' class
and related helper functions.

The 'WithData' wrapper attaches a 'Record' as metadata to any value. Equality,
ordering, hashing, and 'Show' all delegate to the wrapped value, ignoring
metadata, so 'WithData' fields survive transformations transparently.

Example defining a two-field record:

> type MyFields = '[ 'KeyVal "count" Int, 'KeyVal "name" String ]
> myRecord :: Record MyFields
> myRecord = Cons 0 (Cons "hello" Nil)
> myCount = select @"count" myRecord  -- 0
> myName = select @"name" myRecord  -- "hello"
-}
module Data.WithData (
    HasField,
    KeyVal (..),
    Lookup,
    Record (..),
    WithData (..),
    getMeta,
    removeMeta,
    select,
    setMeta,
    updateMeta,
) where

import Data.Kind (Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.TypeLits

import Data.Function (on)
import Data.Hashable (Hashable (..))

-- | A pairing of a type-level string label with a 'Type'.
data KeyVal = KeyVal !Symbol !Type

-- | A type-indexed heterogeneous record.
data Record (fields :: [KeyVal]) where
    Nil :: Record '[]
    Cons :: !ty -> Record fields -> Record ('KeyVal label ty : fields)

{- | Returns 'True if the symbol @s@ matches the label of the first field in
@fields@, 'False otherwise (including on an empty list).
-}
type family IsHead (s :: Symbol) (fields :: [KeyVal]) :: Bool where
    IsHead s ('KeyVal s' _ ': _) = s == s'
    IsHead s _ = 'False

{- | Class providing field manipulation on a 'Record' by label.

The @isHead@ boolean parameter drives instance selection without overlap:
Use the 'HasField' constraint alias rather than this class directly.
-}
class Member (s :: Symbol) fields t (isHead :: Bool) where
    -- | The field list after removing the field labeled @s@.
    type Without s fields isHead :: [KeyVal]

    -- | Extract the value at label @s@.
    member :: Record fields -> t

    -- | Apply a function to the value at label @s@.
    update :: (t -> t) -> Record fields -> Record fields

    -- | Remove the field labeled @s@.
    remove :: Record fields -> Record (Without s fields isHead)

instance Member s ('KeyVal s ty ': fields) ty 'True where
    type Without s ('KeyVal s ty ': fields) 'True = fields
    member (Cons x _) = x
    update f (Cons x r) = Cons (f x) r
    remove (Cons _ r) = r

instance (Member s fields ty (IsHead s fields)) => Member s ('KeyVal s' ty' ': fields) ty 'False where
    type Without s ('KeyVal s' ty' ': fields) 'False = 'KeyVal s' ty' ': Without s fields (IsHead s fields)
    member (Cons _ r) = member @s @fields @ty @(IsHead s fields) r
    update f (Cons x r) = Cons x (update @s @fields @ty @(IsHead s fields) f r)
    remove (Cons x r) = Cons x (remove @s @fields @ty @(IsHead s fields) r)

{- | Look up the 'Type' associated with label @s@ in a list of 'KeyVal' entries.
Produces a 'TypeError' at compile time if @s@ is not present.
-}
type family Lookup (s :: Symbol) (xs :: [KeyVal]) :: Type where
    Lookup s '[] = TypeError ('Text "Cannot find the label: " ':<>: 'Text s)
    Lookup s ('KeyVal s' t ': tail) = If (s == s') t (Lookup s tail)
    Lookup s (badType ': tail) =
        TypeError
            ( 'Text "Expecting a KV in the type list, instead have: "
                ':<>: ShowType badType
            )

type LookupField name fields = Member name fields (Lookup name fields) (IsHead name fields)

-- | Constraint asserting that @fields@ contains a field labeled @name@ with type @ty@.
type HasField name ty fields = (Member name fields (Lookup name fields) (IsHead name fields), Lookup name fields ~ ty)

{- | Extract the value at label @s@ from a 'Record'.
>>> select @"myField" (Cons 42 Nil :: Record '[ 'KeyVal "myField" Int])
42
-}
select :: forall s fields. (LookupField s fields) => Record fields -> Lookup s fields
select = member @s @fields @(Lookup s fields) @(IsHead s fields)

{- | A value of type @a@ annotated with a 'Record' of metadata fields.

'Eq', 'Show', and 'Hashable' instances ignore the metadata and operate
solely on the wrapped value, so 'WithData' values compare and hash like
plain @a@ values.
-}
data WithData (fields :: [KeyVal]) a = WD {metadata :: !(Record fields), val :: !a}

instance (Eq a) => Eq (WithData fields a) where
    (==) = (==) `on` val

instance (Show a) => Show (WithData fields a) where
    show (WD _ v) = show v

instance (Hashable a) => Hashable (WithData fields a) where
    hashWithSalt salt (WD _ v) = hashWithSalt salt v

instance Functor (WithData fields) where
    fmap f (WD meta v) = WD meta (f v)

{- | Extract the metadata field labeled @s@ from a 'WithData' value.
>>> getMeta @"myField" (WD (Cons 42 Nil) () :: WithData '[ 'KeyVal "myField" Int] ())
42
-}
getMeta :: forall s fields a. (LookupField s fields) => WithData fields a -> Lookup s fields
getMeta = select @s . metadata

{- | Apply a function to the metadata field labeled @s@.
'WithData' with the same wrapped value.
>>> wd = updateMeta @"myField" (* 2) (WD (Cons 42 Nil) () :: WithData '[ 'KeyVal "myField" Int] ())
>>> getMeta @"myField" wd
84
-}
updateMeta :: forall s fields a. (LookupField s fields) => (Lookup s fields -> Lookup s fields) -> WithData fields a -> WithData fields a
updateMeta f (WD metadata val) = WD (update @s @fields @(Lookup s fields) @(IsHead s fields) f metadata) val

{- | Set the metadata field labeled @s@ to a new value.
>>> wd = setMeta @"myField" 18 (WD (Cons 42 Nil) () :: WithData '[ 'KeyVal "myField" Int] ())
>>> getMeta @"myField" wd
18
-}
setMeta :: forall s fields a. (LookupField s fields) => Lookup s fields -> WithData fields a -> WithData fields a
setMeta v = updateMeta @s (const v)

{- | Remove the metadata field labeled @s@.
>>> wd = removeMeta @"myField" (WD (Cons 42 Nil) () :: WithData '[ 'KeyVal "myField" Int] ())
>>> getMeta @"myField" wd
Cannot find the label: myField
-}
removeMeta :: forall s fields fields' a. (LookupField s fields, Without s fields (IsHead s fields) ~ fields') => WithData fields a -> WithData fields' a
removeMeta (WD meta v) = WD (remove @s @fields @(Lookup s fields) @(IsHead s fields) meta) v
