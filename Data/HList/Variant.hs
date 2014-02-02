{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}


{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Variants, i.e., labelled sums.

   One approach to their implementation would be to consider both
   the favoured label and the corresponding value as dynamics upon
   variant construction. Since we are too lazy to programme some
   Typeable instances for non-ghc systems (NB: in GHC, Typeable
   is derivable), we rather model variants as (opaque) records
   with maybies for the values. Only one value will actually hold
   non-Nothing, as guaranteed by the constructor.

   See VariantP.hs for a different approach to open sums.
-}

module Data.HList.Variant where

import Data.HList.FakePrelude
import Data.HList.Record
import Data.HList.HList


-- --------------------------------------------------------------------------
-- | Variant types on the basis of label-maybe pairs.

newtype Variant mr = Variant mr


-- --------------------------------------------------------------------------
-- | Turn proxy sequence into sequence of Nothings

data HMaybeF = HMaybeF
instance ((Tagged l (Proxy t) ~ a, b ~ Tagged l (Maybe t))) =>  ApplyAB HMaybeF a b   where
    applyAB _ _ = Tagged Nothing

hMaybied x = hMap HMaybeF x


-- --------------------------------------------------------------------------
-- | Public constructor
-- it seems we can blame 'hUpdateAtLabel' (not 'HMap') for needing the asTypeOf?
mkVariant x y (Record v) = let r1 = Record (hMaybied v) in
    case hUpdateAtLabel x (Just y) r1 `asTypeOf` r1 of
    Record t -> Variant t

-- --------------------------------------------------------------------------
-- | Public destructor

unVariant x (Variant v) = hLookupByLabel x (Record v)


-- --------------------------------------------------------------------------
-- | Variants are opaque

instance Show (Variant v)
 where
  show _ = "<Cannot show Variant content!>"


