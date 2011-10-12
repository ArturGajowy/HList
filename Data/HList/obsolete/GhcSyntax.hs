{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Some dedicated infix operators at the type and the value level.
-}

module Data.HList.GhcSyntax where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.Record
import Data.HList.GhcRecord
import Data.HList.HArray


{-----------------------------------------------------------------------------}

-- * For types

infixr 2 :*:
infixr 2 .*.

type e :*: l = HCons e l


infixr 4 :=:
type l :=: v = LVPair l v

infixr 4 .=.


{-----------------------------------------------------------------------------}

-- * For records

{-|

  [@(.*.)@]
           Add a field to a record. Analagous to (++) for
           lists.

  > record .*. field1
  >        .*. field2

-}
(.*.) :: HExtend e l l' => e -> l -> l'
(.*.) =  hExtend

{-|

  Create a value with the given label. Analagous to a data
  constructor such as 'Just', 'Left', or 'Right'. Higher fixity
  than record-modification operations like (.*.), (.-.), etc. to
  support expression like the below w/o parentheses:

  > label1 .=. value1 .*.
  > label2 .=. value2 .*.
  > emptyRecord

-}
(.=.) :: l -> v -> LVPair l v
l .=. v = newLVPair l v

infixr 9 .!.
{-|
  Lookup a value in a record, by its label. Analagous to (!!), the
  list indexing operation. Highest fixity, like (!!).

  > record1 .*. label1 .=. record2 .!. label1
  >         .*. label2 .=. record2 .!. label2

-}
(.!.) :: (HasField l r v) => r -> l -> v
r .!. l =  hLookupByLabel l r

infixl 2 .-.
{-|
  Remove a field from a record. At the same
  level as other record modification options (.*.). Analagous
  to (\\) in lists.

  > record1 .-. label1

  > label1 .=. value1 .*.
  > label2 .=. value2 .-.
  > label2 .*.
  > emptyRecord

  > label1 .=. value1 .-.
  > label1 .*.
  > label2 .=. value2 .*.
  > emptyRecord

  > record1 .*. label1 .=. record2 .!. label1
  >         .*. label2 .=. record2 .!. label2
  >         .-. label1

-}
(.-.) :: (H2ProjectByLabels (HCons e HNil) r _r' r') =>
    Record r -> e -> Record r'
r .-. l =  hDeleteAtLabel l r

infixr 2 .@.
{-|

  Update a field with a particular value.
  Same fixity as (.*.) so that extensions and updates can be chained.
  There is no real list analogue, since there is no Prelude defined
  update.

  > label1 .=. value1 .@. record1

-}
(.@.) :: (HUpdateAtHNat n (LVPair t t1) t2 l',HFind t ls n,RecordLabels t2 ls) =>LVPair t t1 -> Record t2 -> Record l'
f@(LVPair v) .@. r  =  hUpdateAtLabel (labelLVPair f) v r

infixr 2 .^.
{-|
  This is a variation on updating (according to GhcRecord.hs),
  so use the same fixity as (.\@.).
-}
(.^.) :: (HUpdateAtHNat n (LVPair t t1) t2 l',HFind t ls n,RecordLabels t2 ls,HasField t t2 (Proxy t1)) =>LVPair t t1 -> Record t2 -> Record l'
f@(LVPair v) .^. r = hUnproxyLabel (labelLVPair f) v r

infixr 2 .<.
{-|
  Another variation on update, so give it the same fixity as (.\@.).

-}
(.<.) :: (HasField t t2 t1,HUpdateAtHNat n (LVPair t t1) t2 l',HFind t ls n,RecordLabels t2 ls) =>LVPair t t1 -> Record t2 -> Record l'
f@(LVPair v) .<. r = hTPupdateAtLabel (labelLVPair f) v r

infixl 1 .<++.
{-|
  Similar to list append, so give this slightly lower fixity than
  (.*.), so we can write:

   > field1 .=. value .*. record1 .<++. record2

-}
(.<++.) ::  (HLeftUnion r r' r'') => r -> r' -> r''
r .<++. r' = hLeftUnion r r'


{-----------------------------------------------------------------------------}

-- Convenience notation for TIRs

infixr 2 :+:
infixr 2 .+.

type e :+: l = HCons (Proxy e) l

{-|
  Type-indexed rows append. Very similar to (.*.), so
  keep the same fixity.
-}
(.+.) ::  (HExtend (Proxy e) l l') => e -> l -> l'
e .+. r = hExtend (toProxy e) r


{-----------------------------------------------------------------------------}
