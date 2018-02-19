{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic type
   cast and generic type equality.

-}

module HListExample.MainGhcGeneric1 (mainGhcGeneric1) where

import HListExample.Datatypes2
import Data.HList.CommonMain
import Control.Lens

import Properties.Common
import Test.Hspec

import Control.Monad.Writer

-- --------------------------------------------------------------------------

type Animal =  '[Key,Name,Breed,Price]

angus :: HList Animal
angus =  HCons (Key 42)
           (HCons (Name "Angus")
           (HCons  Cow
           (HCons (Price 75.5)
            HNil)))

tlist1 = hFoldr (HSeq HPrint) (return () :: IO ()) angus

testBasic = do
  it "basic" $ do
    -- tlist1 does IO. The equivalent using Writer
    let f = HSeq $ HComp (tell . (:[])) HShow
        f2 = Fun (tell . (:[]) . show) :: Fun Show (Writer [String] ())

        angusStr = ["Key 42",  "Name \"Angus\"", "Cow", "Price 75.5" ]

    execWriter (hFoldr f (return ()) angus) `shouldBe`
        angusStr

    execWriter (hFoldr (HSeq f2) (return ()) angus) `shouldBe`
        angusStr


    hAppend angus angus `shouldShowTo` 
      "H[Key 42,Name \"Angus\",Cow,Price 75.5,Key 42,Name \"Angus\",Cow,Price 75.5]"


testHArray = do
  it "HArray" $ do
    hProjectByHNats (hNats (HCons hZero (HCons hZero HNil))) angus `shouldShowTo`
      "H[Key 42]"

      -- Before:
      -- H[Key 42, Key 42]
      -- XXX I don't duplicate at present!
    hProjectByHNats (hNats (HCons hZero (HCons (hSucc hZero) HNil))) angus `shouldShowTo`
      "H[Key 42,Name \"Angus\"]"

    hProjectByHNats (undefined::Proxy ['HZero, 'HSucc 'HZero]) angus `shouldShowTo`
      "H[Key 42,Name \"Angus\"]"

    hProjectAwayByHNats (hNats (HCons hZero HNil)) angus `shouldShowTo`
      "H[Name \"Angus\",Cow,Price 75.5]"

    hSplitByHNats 
	    (undefined::Proxy ['HZero, 'HSucc 'HZero])
	    angus
      `shouldShowTo`
      "(H[Key 42,Name \"Angus\"],H[Cow,Price 75.5])"


  it "HOccurs" $ do
    (hOccurs angus :: Breed) `shouldShowTo` "Cow"
    hOccurs angus `shouldBe` Cow

    hOccurs (hBuild 1 ^. from tipHList) `shouldShowTo` "1"

    (null $ hOccurs $ hBuild [] ^. from tipHList) `shouldBe` True
    (hProject angus :: HList '[Key, Name]) `shouldShowTo`
      "H[Key 42,Name \"Angus\"]"


  it "TypeIndexed" $ do
    let typeIdx1 = hDeleteMany (undefined::Proxy Name) angus
        typeIdx2 = BSE .*. angus
    typeIdx1 `shouldShowTo` "H[Key 42,Cow,Price 75.5]"
    typeIdx2 `shouldShowTo` "H[BSE,Key 42,Name \"Angus\",Cow,Price 75.5]"

    hUpdateAt Sheep typeIdx1 `shouldShowTo`
      "H[Key 42,Sheep,Price 75.5]"
  
    hDeleteAt (undefined::Proxy Breed) typeIdx2 `shouldShowTo`
      "H[BSE,Key 42,Name \"Angus\",Price 75.5]"

    hProjectBy (undefined::Proxy '[Breed]) angus `shouldShowTo` "H[Cow]"

    hProject angus `shouldBe` HCons Cow HNil

    -- doesn't work
    -- hProjectBy Proxy angus `shouldBe` HCons Cow HNil

    hSplitBy (undefined:: Proxy '[Breed]) angus `shouldShowTo`
      "(H[Cow],H[Key 42,Name \"Angus\",Price 75.5])"

testTIP = do
  -- |
  -- This example from the TIR paper challenges singleton lists.
  -- Thanks to the HW 2004 reviewer who pointed out the value of this example.
  -- We note that the explicit type below is richer than the inferred type.
  -- This richer type was needed for making this operation more polymorphic.
  -- That is, /a)/ would not work without the explicit type, 
  -- while /b/ would:
  --
  -- >  a)  ((+) (1::Int)) $ snd $ tuple oneTrue
  -- >  b)  ((+) (1::Int)) $ fst $ tuple oneTrue
  --
  -- As of 2014, type signatures are not needed to define tipyTuple.
  it "tipyTuple" $ do
    let tuple l = tipyTuple l

        -- oneTrue :: TIP (TagR [Int, Bool])		-- inferred
        -- oneTrue :: TIP '[Tagged Int Int, Tagged Bool Bool] -- expanded out
        oneTrue = (1::Int) .*. True .*. emptyTIP

    case tuple oneTrue of
      (a,b) -> (a+(1::Int), not b) `shouldShowTo` "(2,False)"

    not (fst (tuple oneTrue)) `shouldShowTo` "False"
    tuple oneTrue `shouldBe` (1::Int,True)

    (((+) (1::Int)) $ fst $ tuple oneTrue) `shouldBe` 2
    (((+) (1::Int)) $ snd $ tuple oneTrue) `shouldBe` 2


  it "tip" $ do
    hOccurs myTipyCow `shouldBe` Cow
    (BSE .*. myTipyCow) `shouldShowTo` "TIPH[BSE,Key 42,Name \"Angus\",Cow,Price 75.5]"
    -- (Sheep .*. myTipyCow) `shouldBe`  _
    {- if we uncomment the line above, we get the type error
       about the violation of the TIP condition: Breed type
       occurs twice.

      No instance for (Fail * (TypeFound Breed))
    -}

    (Sheep .*. hDeleteAtLabel (Label :: Label Breed) myTipyCow)
        `shouldShowTo` "TIPH[Sheep,Key 42,Name \"Angus\",Price 75.5]"

    (Sheep .*. (myTipyCow .-. (Label :: Label Breed)))
        `shouldShowTo` "TIPH[Sheep,Key 42,Name \"Angus\",Price 75.5]"

    tipyUpdate Sheep myTipyCow
        `shouldShowTo` "TIPH[Key 42,Name \"Angus\",Sheep,Price 75.5]"




myTipyCow = tipHList # angus -- lens #

animalKey :: ( SubType l (TIP Animal) -- explicit
             , HOccurs Key l          -- implicit
             ) => l -> Key
animalKey = hOccurs

animalish :: SubType l (TIP Animal) => l -> l
animalish = id
animalKey' l = hOccurs (animalish l) :: Key


makeLabels3 "MyNS" (words "key name breed price")
{- ^ makeLabels3 generates something like
data MyNS = MyNS -- a name space for record labels

key   = firstLabel MyNS  (undefined::DKey)
name  = nextLabel  key   (undefined::DName)
breed = nextLabel  name  (undefined::DBreed)
price = nextLabel  breed (undefined::DPrice)

data DKey;   instance Show DKey   where show _ = "key"
data DName;  instance Show DName  where show _ = "name"
data DBreed; instance Show DBreed where show _ = "breed"
data DPrice; instance Show DPrice where show _ = "price"

-}

unpricedAngus =  key    .=. (42::Integer)
             .*. name   .=. "Angus"
             .*. breed  .=. Cow
             .*. emptyRecord


testRecords = describe "testRecords" $ it "tests" $ do

  unpricedAngus `shouldShowTo` "Record{key=42,name=\"Angus\",breed=Cow}"
  unpricedAngus .!. breed `shouldShowTo` "Cow"

  let test3 = hDeleteAtLabel breed unpricedAngus

  test3
    `shouldShowTo` "Record{key=42,name=\"Angus\"}"

  (breed .=. Sheep .@. unpricedAngus)
    `shouldShowTo` "Record{key=42,name=\"Angus\",breed=Sheep}"

  let test4 = price .=. 8.8 .*. unpricedAngus

  test4
    `shouldShowTo` "Record{price=8.8,key=42,name=\"Angus\",breed=Cow}"

  hProjectByLabels (labelsOf (breed `HCons` price `HCons` HNil)) test4
    `shouldShowTo` "Record{price=8.8,breed=Cow}"

  -- XXX extra Label shouldn't be needed?
  -- alternatively it could be a compile-time error...
  -- hProjectByLabels (hEndP $ hBuild breed price) test4
  --  `shouldShowTo` "Record{price=8.8,breed=Cow}"

  -- test7 should be the same as test4 but
  -- with the different order of labels
  (newLVPair breed Sheep) .*. test3
    `shouldShowTo` "Record{breed=Sheep,key=42,name=\"Angus\"}"


type AnimalCol = TagR [Key,Name,Breed,Price]


testTIC = describe "TIC" $ do
  it "show" $
    (myCol :: TIC AnimalCol) `shouldShowTo` "TIC{breed=Cow}"
  it "hOccurs found" $
    (hOccurs myCol :: Maybe Breed) `shouldBe` Just Cow
  it "hOccurs absent" $
    (hOccurs myCol :: Maybe Price) `shouldBe` Nothing

myCol = mkTIC Cow :: TIC AnimalCol
{-

*TIC> mkTIC "42" :: TIC AnimalCol
Type error ...

*TIC> hOccurs myCol :: Maybe String
Type error ...

-- both of the these type errors could be better
-- (on ghc-7.10.3), Any is used to satisfy FD coverage condition, but the
-- TypeError context should be printed instead
<interactive>:170:1:
    Couldn't match type ‘Data.HList.CommonMain.Any’ with ‘[Char]’
    In the expression: mkTIC "42" :: TIC AnimalCol
    In an equation for ‘it’: it = mkTIC "42" :: TIC AnimalCol
-}

testVariant = describe "Variant" $ it "test" $ do
    testVar1 `shouldShowTo` "V{name=\"angus\"}"
    (testVar1 .!. key) `shouldBe` Nothing
    (testVar1 .!. name) `shouldBe` Just "angus"
 where
  testVar1 = mkVariant name "angus" animalVar

animalVar = asProxy $
               key   .=. (undefined :: Integer)
           .*. name  .=. (undefined :: String)
           .*. breed .=. (undefined :: Breed)
           .*. emptyRecord


asProxy :: proxy a -> Proxy a
asProxy _ = Proxy

mainGhcGeneric1 = describe "mainGhcGeneric1" $ do
  testBasic
  testHArray
  testTIP
  testRecords
  testTIC
  testVariant
