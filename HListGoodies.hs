{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A wave of type-level functions involving HLists.

 -}

 
module HListGoodies where

import FakePrelude
import HListPrelude


{-----------------------------------------------------------------------------}

-- Staged equality for lists

instance HStagedEq HNil HNil
 where
  hStagedEq _ _ = True
 
instance HStagedEq HNil (HCons e l)
 where
  hStagedEq _ _ = False
 
instance HStagedEq (HCons e l) HNil
 where
  hStagedEq _ _ = False

instance ( TypeEqBool e e' b
         , HStagedEq l l'
         , HStagedEq' b e e'
         )
      =>   HStagedEq (HCons e l) (HCons e' l')
 where
  hStagedEq (HCons e l) (HCons e' l') = (hStagedEq' b e e') && b'
   where
    b  = typeEqBool e e'
    b' = hStagedEq l l'

class HStagedEq' b e e'
 where
  hStagedEq' :: b -> e -> e' -> Bool

instance HStagedEq' HFalse e e'
 where
  hStagedEq' _ _ _ = False

instance Eq e => HStagedEq' HTrue e e
 where
  hStagedEq' _ = (==)


{-----------------------------------------------------------------------------}


-- Ensure a list to contain HNats only

class HList l => HNats l
instance HNats HNil
instance (HNat n, HNats ns) => HNats (HCons n ns)


-- Static set property based on HEq

class HSet l
instance HSet HNil
instance (HNotMember e l, HSet l) => HSet (HCons e l)


-- Find an element in a set based on HEq
class HNat n => HFind e l n | e l -> n
 where
  hFind :: e -> l -> n

instance ( HEq e e' b
         , HFind' b e l n
         )
      =>   HFind e (HCons e' l) n
 where
  hFind e (HCons e' l) = n
   where
    b  = hEq e e'
    n  = hFind' b e l

class HNat n => HFind' b e l n | b e l -> n
 where
  hFind' :: b -> e -> l -> n

instance HFind' HTrue e l HZero
 where
  hFind' _ _ _ = HZero

instance HFind e l n
      => HFind' HFalse e l (HSucc n)
 where
  hFind' _ e l = HSucc (hFind e l)


-- Negated membership test; could be written as predicate

class HNotMember e l
instance HNotMember e HNil
instance (HEq e e' HFalse, HNotMember e l) => HNotMember e (HCons e' l)


-- Turn a heterogeneous list into a homogeneous one

class HList2List l e
 where
  hList2List :: l -> [e]

instance HList2List HNil e
 where
  hList2List HNil = []

instance HList2List l e
      => HList2List (HCons e l) e
 where
  hList2List (HCons e l) = e:hList2List l


{-----------------------------------------------------------------------------}
