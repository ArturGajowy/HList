{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic cast
   and TTypeable-based type equality. This module is prepared for use
   with Hugs. This model works in principle also for GHC (see module
   GHCTTypeable) perhaps modulo some slight differences in test cases.

-}


import HugsDatatypes
import CommonMain
import TTypeableTypeEq
import TTypeableTypeEqBool


{-----------------------------------------------------------------------------}

type Animal =  HCons Key
              (HCons Name
              (HCons Breed
              (HCons Price
               HNil)))

myAnimal :: Animal
myAnimal =  HCons (Key 42)
           (HCons (Name "Angus")
           (HCons  Cow
           (HCons (Price 75.5)
            HNil)))

{-

HList> hFoldr (HSeq HShow) (return () :: IO ()) myAnimal
Key 42
Name "Angus"
Cow
Price 75.5

HList> hAppend myAnimal myAnimal
HCons (Key 42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) (HCons (Key
42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil)))))))

-}

testHArray = (myProj1,myProj2,myProj3,myProj4)
 where
  myProj1 = hProjectByHNats myAnimal (HCons HZero (HCons HZero HNil))
  myProj2 = hProjectByHNats myAnimal (HCons HZero (HCons (HSucc HZero) HNil))
  myProj3 = hProjectAwayByHNats myAnimal (HCons HZero HNil)
  myProj4 = hSplitByHNats myAnimal (HCons HZero (HCons (HSucc HZero) HNil))

{-

*HArray> myProj1
HCons (Key 42) (HCons (Key 42) HNil)

*HArray> myProj2
HCons (Key 42) (HCons Cow HNil)

*HArray> myProj3
HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil))

*HArray> myProj4
(HCons (Key 42) (HCons (Name "Angus") HNil),HCons Cow (HCons (Price 75.5) HNil)

-}

testHOccurs = (testHOccurs1,testHOccurs2)
 where
  testHOccurs1 = hOccurs myAnimal :: Breed
  testHOccurs2 = hOccursGrounded (HCons 1 HNil)

testTypeIndexed = (typeIdx1,typeIdx2,typeIdx3,typeIdx4,typeIdx5)
 where
   typeIdx1 = hExtend BSE myAnimal
   typeIdx2 = hUpdateByType  typeIdx1 Sheep
   typeIdx3 = hDeleteByProxy myAnimal (Proxy::Proxy Breed)
   typeIdx4 = hProjectByProxies myAnimal (HCons (Proxy::Proxy Breed) HNil)
   typeIdx5 = fst$ hSplitByProxies myAnimal (HCons (Proxy::Proxy Breed) HNil)

testTuple = [testTuple1,testTuple2,testTuple3]
 where
  testTuple1 = let (a,b) = tuple oneTrue in (a+(1::Int), not b)
  testTuple2 = let (n,l,a,b) = tuple' oneTrue in (a+(1::Int), not b)
  testTuple3 = let b = not $ fst $ tuple oneTrue in (1::Int,b)

testTIP = [show testTIP1, show testTIP2, show testTIP3, show testTIP4]
 where
  myTipyCow = TIP myAnimal
  animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
  animalKey = hOccurs
  testTIP1 = hOccurs myTipyCow :: Breed
  testTIP2 = hExtend BSE myTipyCow
  testTIP3 = hExtend Sheep $ tipyDelete myTipyCow (Proxy::Proxy Breed)
  testTIP4 = tipyUpdate myTipyCow Sheep

testSimpleRecords = [ show test1 
                    , show test2
                    , show test3 
                    , show test4
                    , show test5
                    , show test6
                    ]
 where
  key   = HZero
  name  = HSucc key
  breed = HSucc name
  price = HSucc breed
  test1 = mkSimpleRecord $ HCons (key,42::Integer)
                         $ HCons (name,"Angus")
                         $ HCons (breed,Cow)
                         $ HNil 
  test2 = hLookupByLabel test1 breed
  test3 = hDeleteByLabel test1 breed
  test4 = hUpdateByLabel test1 breed Sheep
  test5 = hExtend (price,8.8) test1
  test6 = hProject test5 (HCons breed (HCons price HNil))


{-----------------------------------------------------------------------------}

main = print $   ( testHArray
               , ( testHOccurs
               , ( testTypeIndexed
               , ( testTuple
               , ( testTIP
               , ( testSimpleRecords
               ))))))


{-----------------------------------------------------------------------------}
