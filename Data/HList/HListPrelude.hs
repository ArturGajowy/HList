
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Declarations for various classes and functions that
   apply for the whole range of heterogeneous collections
   (HList, TIP, records, etc).
 -}

module Data.HList.HListPrelude where

import Data.HList.FakePrelude
import GHC.TypeLits (Symbol)


class HExtend e l where
  type HExtendR e l
  (.*.) :: e -> l -> HExtendR e l

infixr 2 .*.


-- | to keep types shorter, '.*.' used with Proxy avoids
-- producing a @Proxy :: Proxy '[Label x,Label y,Label z]@
-- if @Proxy :: Proxy '[x,y,z]@ is not a kind error (as it
-- is when mixing Label6 and Label3 labels).
--
-- ghc-7.6 does not accept @Proxy ('[] :: [k])@ so for now
-- require @k ~ *@
instance HExtend (Label x) (Proxy ('[] :: [*])) where
    type HExtendR (Label x) (Proxy ('[] :: [*])) = Proxy '[x]
    (.*.) _ _ = Proxy


-- | similar to 'emptyRecord', 'emptyTIP', emptyHList (actually called 'HNil'),
-- except emptyProxy is the rightmost argument to '.*.'
emptyProxy = Proxy :: Proxy ('[] :: [*])

-- Poly-kinded
class SubType l l'

-- subType :: SubType l l' => l -> l' -> ()
-- subType _ _ = ()

class HAppend l1 l2 where
  hAppend :: l1 -> l2 -> HAppendR l1 l2

-- | poly-kinded, but 'hAppend' only works in cases where the kind variable
-- `k` is `*`
type family HAppendR (l1::k) (l2::k) :: k
-- class HMember e1 l (b :: Bool) | e1 l -> b

-- One occurrence and nothing is left
class HOccurs e l where
  hOccurs :: l -> e

-- Class to test that a type is "free" in a type sequence
-- polykinded
class HOccursNot (e :: k) (l :: [k])

class HProject l l' where
  hProject :: l -> l'

-- | Map a type (key) to a natural (index) within the collection
-- This is a purely type-level computation
class HType2HNat (e :: k) (l :: [k]) (n :: HNat) | e l -> n

--  | and lift to the list of types
class HTypes2HNats es l (ns :: [HNat]) | es l -> ns

-- | Delete all elements with the type-level key e from the
-- collection l. Since the key is type-level, it is represented
-- by a Proxy.
-- (polykinded)

class HDeleteMany e l l' | e l -> l' where
  hDeleteMany :: Proxy e -> l -> l'


class HDeleteAtLabel (r :: [*] -> *) (l :: k) v v' | l v -> v' where
    hDeleteAtLabel :: Label l -> r v -> r v'




-- | 'unzip'
class SameLengths [x,y,xy] => HUnzip (r :: [*] -> *) x y xy
        | x y -> xy, xy -> x y where
  hUnzip :: r xy -> (r x, r y)

-- | 'zip'. Variant supports hUnzip, but not hZip ('hZipVariant' returns a Maybe)
class HUnzip r x y xy => HZip (r :: [*] -> *) x y xy where
  hZip :: r x -> r y -> r xy


