{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records -- advanced operations
   These operations that (may) require GHC

   See "Data.HList.Record" for the base module.
-}

module Data.HList.RecordAdv where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HArray
import Data.HList.Record
import Data.HList.HList
import Data.Typeable

-- --------------------------------------------------------------------------

-- | A variation on update.
--
-- Replace a proxy by a value of the proxied type.
-- The signature is inferred
hUnproxyLabel :: (HFind l (RecordLabels r) n,HUpdateAtHNat n (LVPair l v) r,HasField l (Record r) (Proxy v)) =>
    Label l-> v -> Record r -> Record (HUpdateAtHNatR n (LVPair l v) r)
hUnproxyLabel l v r = hUpdateAtLabel l v r
 where
  tpe :: a -> Proxy a -> ()
  tpe _ _ = ()
  _ = tpe v (hLookupByLabel l r)


infixr 2 .^.
{-|
  This is a variation on updating, so use the same fixity as (.\@.).
-}
f@(LVPair v) .^. r = hUnproxyLabel (labelLVPair f) v r


-- --------------------------------------------------------------------------

-- | Test for values; refuse proxies

hasNoProxies :: HasNoProxies r
             => Record r -> ()
hasNoProxies = const ()


data ProxyFound x
class HasNoProxies (l :: [*])
instance HasNoProxies '[]
instance Fail (ProxyFound x) => HasNoProxies (Proxy x ': l)
instance Fail (ProxyFound x) => HasNoProxies (LVPair lab (Proxy x) ': l)
instance HasNoProxies l => HasNoProxies (e ': l)


-- --------------------------------------------------------------------------

-- | Narrow a record to a different record type
--
-- First is the `monadic' version, which returns the `failure indictator'
-- (HNothing) if the narrowing fails because the source does not have
-- all the fields for the target.
class  NarrowM a b res | a b -> res where
    narrowM :: Record a -> Record b -> res

instance NarrowM a '[] (HJust (Record '[])) where

    narrowM _ _ = HJust emptyRecord

instance (H2ProjectByLabels (l ': '[]) a rin rout,
          NarrowM' rin rout b res)
    => NarrowM a (LVPair l v ': b) res where
    narrowM (Record a) _ = narrowM' (Record rin) (Record rout) (undefined:: Record b)
     where
        (rin,rout) = h2projectByLabels (proxy :: Proxy (l ': '[])) a

-- | could be changed to type family
class  NarrowM' rin rout b res | rin rout b -> res where
    narrowM' :: Record rin -> Record rout -> Record b -> res

instance NarrowM' '[] rout b HNothing where
    narrowM' _ _ _ = HNothing

instance (NarrowM rout b res',
          NarrowM'' f res' res)
    => NarrowM' (f ': '[]) rout b res where
    narrowM' (Record (f `HCons` HNil)) rout b =
        narrowM'' f (narrowM rout b)

class  NarrowM'' f r r' where
    narrowM'' :: f -> r -> r'

instance (hNothing ~ HNothing) => NarrowM'' f HNothing hNothing where
    narrowM'' _ _ = HNothing

instance NarrowM'' f (HJust (Record r)) (HJust (Record (f ': r))) where
    narrowM'' f (HJust (Record r)) = HJust (Record (HCons f r))


class  Narrow a b
 where narrow :: Record a -> Record b

instance Narrow a '[]
 where   narrow _ = emptyRecord

instance ( Narrow rout r'
         , H2ProjectByLabels (l ': '[]) r (LVPair l v ': '[]) rout
         ) => Narrow r ( LVPair l v ': r' )
  where
    narrow (Record r) = case h2projectByLabels (proxy::Proxy (l ': '[])) r of
        (HCons f HNil,rout) -> let (Record r')    = narrow (Record rout)
            in Record (HCons f r')


-- --------------------------------------------------------------------------

-- | Narrow two records to their least-upper bound

class LubNarrow (a :: [*]) (b :: [*]) (c :: [*]) | a b -> c
 where
  lubNarrow :: Record a -> Record b -> (Record c, Record c)

instance ( HTIntersect (RecordLabels a) (RecordLabels b) lc
         , H2ProjectByLabels lc a c aout
         , H2ProjectByLabels lc b c bout
         , HRLabelSet c
         )
      => LubNarrow a b c
 where

 lubNarrow ra@(Record _) rb@(Record _) =
     ( hProjectByLabels (proxy::Proxy lc) ra
     , hProjectByLabels (proxy::Proxy lc) rb
     )


{-
-- --------------------------------------------------------------------------

-- | List constructors that also LUB together

data NilLub
nilLub :: NilLub
nilLub = undefined

class ConsLub h t l | h t -> l
 where
  consLub :: h -> t -> l

instance ConsLub e  NilLub [e]
 where
  consLub h _ = [h]

instance LubNarrow e0 e1 e2 => ConsLub e0 [e1] [e2]
 where
  consLub h t = fst (head z) : map snd (tail z)
   where
    z = map (lubNarrow h) (undefined:t)


-- --------------------------------------------------------------------------

-- | Extension of lubNarrow to a heterogeneous list

class HLub l e | l -> e
 where
  hLub :: l -> [e]

instance ( LubNarrow h h' e
         )
      => HLub (HCons h (HCons h' HNil)) e
 where
  hLub (HCons h (HCons h' _)) = [fst ee, snd ee]
   where
    ee = lubNarrow h h'

instance ( HLub (HCons h (HCons h'' t)) e'
         , HLub (HCons h' (HCons h'' t)) e''
         , LubNarrow e' e'' e
         , HLub (HCons e (HCons h'' t)) e
         )
      => HLub (HCons h (HCons h' (HCons h'' t))) e
 where
  hLub (HCons h (HCons h' t)) = fst e : ( snd e : tail r )
   where
    e' = hLub (HCons h t)
    e'' = hLub (HCons h' t)
    e = lubNarrow (head e') (head e'')
    r = hLub (HCons (fst e) t)


-}

-- --------------------------------------------------------------------------
-- | Record equivalence modulo field order
--
-- Decide if two records r1 and r2 are identical or differ only in the order
-- of their fields.
--
-- If the two record types are indeed equivalent, return the witness of
-- their equivalence, (HJust (r1->r2,r2->r1)). If they are not equivalent,
-- return HNothing
--
-- The function equivR does not examine the values of its arguments:
-- it needs only their types.
--
--
-- The algorithm is simple: two records are equivalent if one can be narrowed
-- to the other, and vice versa. The narrowing coercions are the desired
-- witnesses.
--
-- The obvious optimization is to check first if two records are of the same
-- type. That requires TypeEq however. Perhaps we shouldn't use it here.
-- Use of the record narrowing tacitly assumes that the label of a record
-- field uniquely determines the type of the field value. Therefore, we
-- should not use equivR on two records with inconsistent labeling...

class RecordEquiv r1 r2 res | r1 r2 -> res where
    equivR :: r1 -> r2 -> res


instance (HEq r1 r2 b, RecordEquiv' b r1 r2 res)
    => RecordEquiv r1 r2 res where
    equivR _ _ = equivR' (undefined::Proxy b) (undefined::r1) (undefined::r2)
-- Two records have the same type: the fast path
instance RecordEquiv' True r r
                      (HJust (r->r,r->r)) where
    equivR' _ _ _ = HJust (id,id)

instance (NarrowM r1 r2 r12, NarrowM r2 r1 r21,
          RecordEquiv' False (Record r1->r12) (Record r2->r21) res)
    => RecordEquiv (Record r1) (Record r2) res where
    equivR r1 r2 = equivR' (undefined :: Proxy False) r1p r2p
     where r1p r1 = narrowM (r1 :: Record r1) r2
           r2p r2 = narrowM (r2 :: Record r2) r1

class RecordEquiv' (b :: Bool) pj1 pj2 res | b pj1 pj2 -> res where
    equivR' :: Proxy b -> pj1 -> pj2 -> res

instance RecordEquiv' False (r1->HJust r2) (r2->HJust r1) (HJust (r1->r2,r2->r1))
    where
    equivR' _ r12 r21 = HJust (unj.r12,unj.r21)
     where unj (HJust x) = x

-- r2 has something that r1 doesn't
instance RecordEquiv' False (r1->HNothing) pj2 HNothing where
    equivR' _ _ _ = HNothing

-- r1 is a strict superset of r2
instance RecordEquiv' False (r1->HJust r2) (r2->HNothing) HNothing where
    equivR' _ _ _ = HNothing

{-

-- --------------------------------------------------------------------------
-- Typeable instances

hNilTcName :: TyCon
hNilTcName = mkTyCon "HList.HNil"
instance Typeable HNil
 where
  typeOf _ = mkTyConApp hNilTcName []

hConsTcName :: TyCon
hConsTcName = mkTyCon "HList.HCons"
instance (Typeable x, Typeable y) => Typeable (HCons x y)
 where
  typeOf ~(HCons x y)
   = mkTyConApp hConsTcName [ typeOf x, typeOf y ]

recordTcName :: TyCon
recordTcName = mkTyCon "HList.Record"
instance Typeable x => Typeable (Record x)
 where
  typeOf ~(Record x)
   = mkTyConApp recordTcName [ typeOf x ]

hFieldTcName :: TyCon
hFieldTcName = mkTyCon "HList.F"
instance (Typeable x, Typeable y) => Typeable (LVPair x y)
 where
  typeOf _
   = mkTyConApp hFieldTcName [ typeOf (undefined::x), typeOf (undefined::y)  ]

proxyTcName :: TyCon
proxyTcName = mkTyCon "HList.Proxy"
instance Typeable x => Typeable (Proxy x)
 where
  typeOf _
   = mkTyConApp proxyTcName [ typeOf (undefined::x) ]


-}

