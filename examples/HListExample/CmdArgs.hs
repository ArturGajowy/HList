{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
module HListExample.CmdArgs where


import Data.Generics
import Control.Lens
import Test.Hspec
import Properties.Common

import Data.HList.CommonMain

import System.Console.CmdArgs (cmdArgs)
import System.Environment (withArgs)


{-

An example showing off the data instance for Record / Variant / TIP / TIC

Also a use of cmdArgs

Note that ghc-7.8.2 does not have (or can produce) instances of typeable
for types of kind Symbol (ie. promoted strings):
<https://ghc.haskell.org/trac/ghc/ticket/9111>, so for now use the Label3
style

-}

#define USE_LABEL3 __GLASGOW_HASKELL__ == 708

#if USE_LABEL3
makeLabels3 "examples_cmdargs" (words "x y z tic")
makeLabels3 "optV" (words "optA optB optC")
#else
makeLabels6 (words "x y z tic")
makeLabels6 (words "optA optB optC")
#endif

makeLabelable "abc df"

#if USE_LABEL3
-- XXX remove extra Label?
v = (optA .*. optB .*. optC .*. emptyProxy)
      `zipTagged` (Proxy :: Proxy '[Int,Char,Double])
#else
v = Proxy :: Proxy '[Tagged "optA" Int, Tagged "optB" Char, Tagged "optC" Double]
#endif

type Z' = TagR [Int, Char, Double]
-- type Z' = [Tagged Int Int, Tagged Char Char, Tagged Double Double]

d0 = x .=. (5 :: Int)
    .*. y .=. True
    .*. z .=. mkVariant optC (1 :: Double) v
    .*. tic .=. mkTIC' 'x' (Proxy :: Proxy Z')
    .*. emptyRecord

-- the equivalent ordinary record for reference
data E = E { a :: Int, b :: Bool }
    deriving (Show, Data, Typeable)

data Opt = OptA Int | OptB Char | OptC Double
    deriving (Show, Data, Typeable)

e0 = E 5 True

mainCmdargs = describe "cmdargs/Data" $ do
  it "variant show" $
    mkVariant optC 1 v `shouldShowTo` "V{optC=1.0}"

  -- increment V{optC=1.0} via data instance
  it "gmapT" $
    gmapT (mkT ((+1) :: Double -> Double)) (mkVariant optC 1 v)
     `shouldShowTo` "V{optC=2.0}"

  it "d0" $
    d0 `shouldShowTo`
      "Record{x=5,y=True,z=V{optC=1.0},tic=TIC{char='x'}}"

  it "modify d0's Bool children" $
    gmapT (mkT not) d0 `shouldShowTo`
      "Record{x=5,y=False,z=V{optC=1.0},tic=TIC{char='x'}}"

  it "modify d0's Int children" $
    gmapT (mkT (+(1::Int))) d0 `shouldShowTo`
      "Record{x=6,y=True,z=V{optC=1.0},tic=TIC{char='x'}}"

  it "modify d0's Char children (none)" $
    gmapT (mkT (succ :: Char -> Char)) d0 `shouldShowTo`
      "Record{x=5,y=True,z=V{optC=1.0},tic=TIC{char='x'}}"

  it "modify d0's Char grandchildren" $
    everywhere (mkT (succ :: Char -> Char)) d0 `shouldShowTo`
      "Record{x=5,y=True,z=V{optC=1.0},tic=TIC{char='y'}}"

#if !(USE_LABEL3)
  it "dredge optC" $
    d0 & dredge optC +~ 1 `shouldShowTo`
      "Record{x=5,y=True,z=V{optC=2.0},tic=TIC{char='x'}}"
#endif

  -- theB is like a TIP the unsafe lookup function applied
  let theB :: Typeable a => a
      theB = error "theB"
            `extB` (1::Int)
            `extB` True
            `extB` (2.5::Double)
            `extB` 'b'
            `extB` mkVariant optC theB v
            `extB` mkTIC' (theB :: Char) (Proxy :: Proxy Z')

  it "fromConstrB" $
    fromConstrB theB undefined `asTypeOf` d0 `shouldShowTo`
      "Record{x=1,y=True,z=V{optC=2.5},tic=TIC{char='b'}}"

  it "cmdargs built-in data" $
    withArgs ["-a=4", "-b=False" ] (cmdArgs e0) `shouldReturnShowTo`
        "E {a = 4, b = False}"

  -- drop the tic and variant-containing fields: cmdargs doesn't support
  -- it. Cmdargs doesn't support fields containing
  -- `data Opt = OptA Int | OptB Char` either
  let dRec = d0 & from hListRecord %~ (hInit . hInit)

  it "dRec" $
    dRec `shouldShowTo`
      "Record{x=5,y=True}"

  it "cmdargs Record" $
    withArgs ["-x=4", "-y=False"] (cmdArgs dRec) `shouldReturnShowTo`
      "Record{x=4,y=False}"

