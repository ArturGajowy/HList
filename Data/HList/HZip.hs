
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Zipping and unzipping for (conceptually) lists of pairs.

   Provides two alternative implementations
 -}

module Data.HList.HZip where

import Data.HList.HList
import Data.HList.FakePrelude

-- * zip
-- ** functional dependency

class HZip x y l | x y -> l, l -> x y where
  hZip   :: HList x -> HList y -> HList l
  hUnzip :: HList l -> (HList x, HList y)

instance HZip '[] '[] '[] where
  hZip _ _ = HNil
  hUnzip _ = (HNil, HNil)

instance ((x,y)~z, HZip xs ys zs) => HZip (x ': xs) (y ': ys) (z ': zs) where
  hZip (HCons x xs) (HCons y ys) = (x,y) `HCons` hZip xs ys
  hUnzip (HCons ~(x,y) zs) = let ~(xs,ys) = hUnzip zs in (x `HCons` xs, y `HCons` ys)

-- ** type family
-- $note 'hZip2' can be written as a standalone function, with an appropriate
-- type family to calculate the result type. However, that does not seem to
-- be the case for 'hUnzip2', so to re-use some type functions the two are
-- in the same class.

-- | HZipR in the superclass constraint doesn't hurt, but it doesn't seem to be
-- necessary
class HZipR (MapFst z) (MapSnd z) ~ z => HUnZip z where
  type MapFst z :: [*]
  type MapSnd z :: [*]
  hZip2 :: HList (MapFst z) -> HList (MapSnd z) -> HList z
  hUnzip2 :: HList z -> (HList (MapFst z), HList (MapSnd z))

instance HUnZip '[] where
  type MapFst '[] = '[]
  type MapSnd '[] = '[]
  hZip2 _ _ = HNil
  hUnzip2 _ = (HNil, HNil)

instance (z ~ (x,y), HUnZip zs) => HUnZip (z ': zs) where
  type MapFst (z ': zs) = ( Fst z ': MapFst zs )
  type MapSnd (z ': zs) = ( Snd z ': MapSnd zs )
  hZip2 (HCons x xs) (HCons y ys) = HCons (x,y) (hZip2 xs ys)
  hZip2 _ _ = error "Data.HList.HZip.hZip2: impossible case"
  hUnzip2 (HCons ~(x,y) zs) = let ~(xs,ys) = hUnzip2 zs in (x `HCons` xs, y `HCons` ys)


-- | calculates something like:
--
-- > [a] -> [b] -> [(a,b)]
--
-- can be used to give another type for 'hZip2'
--
-- > hZip2 :: HList a -> HList b -> HList (HZipR a b)
type family HZipR (x::[*]) (y::[*]) :: [*]
type instance HZipR '[] '[] = '[]
type instance HZipR (x ': xs) (y ': ys) = (x,y) ': HZipR xs ys


-- ** utility type functions
-- $note do they belong somewhere else?
type family Fst a
type instance Fst (a,b) = a
type family Snd a
type instance Snd (a,b) = b


-- * transpose

{- | this transpose requires equal-length HLists inside a HList:

>>> let ex = (1 .*. 2 .*. HNil) .*. ('a' .*. 'b' .*. HNil) .*. ( 3 .*. 5 .*. HNil) .*. HNil
>>> ex
H[H["b", "a"], H['a', 'b'], H[(), 5]]


>>> hTranspose samp
H[H["b", 'a', ()], H["a", 'b', 5]]

-}
hTranspose x = hFoldr HZipF (hReplicate (hLength (hHead x)) HNil) x


-- ** helpers

-- | same as 'HZip' but HCons the elements of x onto y. This might be doable as
-- a @hMap f (hZip x y)@, but that one doesn't propagate types as easily it
-- seems.
class HZip3 x y l | x y -> l, l -> x y where
  hZip3   :: HList x -> HList y -> HList l

instance HZip3 '[] '[] '[] where
  hZip3 _ _ = HNil

instance (HList (x ': y) ~z, HZip3 xs ys zs) => HZip3 (x ': xs) (HList y ': ys) (z ': zs) where
  hZip3 (HCons x xs) (HCons y ys) = HCons x y  `HCons` hZip3 xs ys

data HZipF = HZipF
instance (
    HZip3 a b c,
    x ~ (HList a, HList b),
    y ~ HList c) => ApplyAB HZipF x y
    where applyAB _ (x,y) = hZip3 x y
