
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed products.
-}

module Data.HList.TIP where


import Data.HList.HListPrelude
import Data.HList.HList
import Data.HList.HArray ()
import Data.HList.HTypeIndexed

import Data.HList.TypeEqO () -- for doctest

-- --------------------------------------------------------------------------
-- * The newtype for type-indexed products

newtype TIP (l :: [*]) = TIP{unTIP:: HList l}

instance Show (HList l) => Show (TIP l) where
  show (TIP l) = "TIP" ++ show l

mkTIP :: HTypeIndexed l => HList l -> TIP l
mkTIP = TIP

emptyTIP :: TIP '[]
emptyTIP = mkTIP HNil

-- --------------------------------------------------------------------------
-- * Type-indexed type sequences

class HTypeIndexed (l :: [*])
instance HTypeIndexed '[]
instance (HOccursNot e l,HTypeIndexed l) => HTypeIndexed (e ': l)

-- --------------------------------------------------------------------------
-- Implementing the HListPrelude interface

instance (HOccursNot e l, HTypeIndexed l) => HExtend e (TIP l) 
 where
  type HExtendR e (TIP l) = TIP (e ': l)
  e .*. TIP l = mkTIP (HCons e l)


-- | One occurrence and nothing is left
--
-- This variation provides an extra feature for singleton lists.
-- That is, the result type is unified with the element in the list.
-- Hence the explicit provision of a result type can be omitted.
--

instance e' ~ e => HOccurs e' (TIP '[e]) where
  hOccurs (TIP (HCons e' _)) = e'

instance HOccurs e (HList (x ': y ': l))
      => HOccurs e (TIP (x ': y ': l)) where
  hOccurs (TIP l) = hOccurs l


instance HOccursNot e l => HOccursNot e (TIP l)


instance (HAppend (HList l) (HList l'), HTypeIndexed (HAppendList l l'))
           => HAppend (TIP l) (TIP l')
 where
  hAppend (TIP l) (TIP l') = mkTIP (hAppend l l')

type instance HAppendR (TIP l) (TIP l') = TIP (HAppendList l l')

-- instance HOccurrence e l l' => HOccurrence e (TIP l) l'
--  where
--   hOccurrence e = hOccurrence e . unTIP

-- --------------------------------------------------------------------------
-- * Shielding type-indexed operations
-- $note The absence of signatures is deliberate! They all must be inferred.

onTIP f (TIP l) = mkTIP (f l)

tipyDelete  p t  = onTIP (hDeleteAt p) t
tipyUpdate  e t  = onTIP (hUpdateAt e) t
tipyProject ps t = onTIP (hProjectBy ps) t


-- | Split produces two TIPs
tipySplit ps (TIP l) = (mkTIP l',mkTIP l'')
 where
  (l',l'') = hSplitBy ps l


-- --------------------------------------------------------------------------

-- | Subtyping for TIPs

instance SubType (TIP l) (TIP '[])
instance (HOccurs e (TIP l1), SubType (TIP l1) (TIP l2))
      =>  SubType (TIP l1) (TIP (e ': l2))


-- --------------------------------------------------------------------------

-- * Sample code

{- $setup

[@Assume@]

>>> import Data.HList.FakePrelude

>>> :{
newtype Key    = Key Integer deriving (Show,Eq,Ord)
newtype Name   = Name String deriving (Show,Eq)
data Breed     = Cow | Sheep deriving (Show,Eq)
newtype Price  = Price Float deriving (Show,Eq,Ord)
data Disease   = BSE | FM deriving (Show,Eq)
type Animal =  '[Key,Name,Breed,Price]
:}

>>> :{
let myAnimal :: HList Animal
    myAnimal = hBuild (Key 42) (Name "Angus") Cow (Price 75.5)
    myTipyCow = TIP myAnimal
    animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
    animalKey = hOccurs
:}

-}

{- $sessionlog
[@Session log@]

>>> :t myTipyCow
myTipyCow :: TIP Animal

>>> hOccurs myTipyCow :: Breed
Cow

>>> BSE .*. myTipyCow
TIPH[BSE, Key 42, Name "Angus", Cow, Price 75.5]



>>> Sheep .*. tipyDelete (proxy::Proxy Breed) myTipyCow
TIPH[Sheep, Key 42, Name "Angus", Price 75.5]

>>> tipyUpdate Sheep myTipyCow
TIPH[Key 42, Name "Angus", Sheep, Price 75.5]

-}


{- $sessionlog2

Don't bother repeating the type error:

>>> let doctestEq x y = x == y || "No instance for" `Data.List.isInfixOf` x

>>> Sheep .*. myTipyCow
-- type error --

-}
