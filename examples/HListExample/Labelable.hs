{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, TemplateHaskell, DataKinds, PolyKinds,
  GADTs, ConstraintKinds #-}
{- | Demonstrates @hLens'@

may be worthwhile to have a lens-free test suite, doing stuff like:

> case x (Identity  . (++"there")) r of Identity t -> t

-}
module HListExample.Labelable where
import Data.HList.CommonMain
import Control.Lens

import Text.Read

import Properties.Common
import Test.Hspec


makeLabelable "lbX lbY"

#if __GLASGOW_HASKELL__ < 707
#define INT_SIG_76 :: Int
#else
#define INT_SIG_76
#endif

r = lbX .==. "hi" .*.
    lbY .==. (lbY .==. 321 .*. lbX .==. 123 .*. emptyRecord) .*.
    emptyRecord

mainLabelable = describe "labelable" $ do
  it "lookup" $ do
    r ^. lbX      `shouldShowTo` "\"hi\""

    -- ghc-7.6 doesn't default when r is involved lower down,
    -- while 7.8.2 does
    (r ^. lbY . lbY  INT_SIG_76) `shouldShowTo` "321"
    (r ^. lbY . lbX  INT_SIG_76) `shouldShowTo` "123"

  it "modify" $ do
    r & lbX .~ () `shouldShowTo`
        "Record{lbX=(),lbY=Record{lbY=321,lbX=123}}"

    r & lbY . lbY .~ "xy" `shouldShowTo`
        "Record{lbX=\"hi\",lbY=Record{lbY=\"xy\",lbX=123}}"

  it "read/show" $ do
    let rString = "Record{lbX=\"hi\",lbY=Record{lbY=321,lbX=123}}"

    r `shouldShowTo` rString

    readMaybe rString `asTypeOf` Just r
        `shouldBe` Just r

    -- the read instance does not reorder labels
    let rStringPerm = "Record{lbY=Record{lbY=321,lbX=123},lbX=\"hi\"}"
    readMaybe rStringPerm `asTypeOf` Just r
        `shouldBe` Nothing

    -- but we can reorder this way
    (r ^. rearranged) `asTypeOf` (undefined :: Record '[Tagged "lbY" t, Tagged "lbX" s])
        `shouldShowTo` rStringPerm
