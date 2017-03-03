{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-} -- for pun
{-# LANGUAGE TemplateHaskell #-}
module HListExample.Prism where


import Test.Hspec
import Properties.Common

import Data.HList.CommonMain
import Data.HList.Labelable (hLens')
import Control.Lens

-- generate left = Label :: Label "left"
makeLabels6 (words "left right up down")

--- define the Labelable labels manually
left_ = hLens' left
right_ = hLens' right
up_ = hLens' up
down_ = hLens' down

-- this definition is needed to decide what order
-- to put the fields in, as well as their initial types
r = [pun|right left up|] where
  left = 'a'
  right = 2 :: Int
  up = 2.3 :: Double

r2 = down_ .==. v .*. r

v = mkVariant left 'x' r

mainPrism = do
  it "inspect v with hPrism" $ do
    v ^? hPrism left `shouldShowTo` "Just 'x'"
    v ^? hPrism right `shouldBe` Nothing
    v ^? hPrism up `shouldBe` Nothing
    v2 ^? hPrism left `shouldShowTo` "Just ()"

  it "inspect v with hPrism through Labelable" $ do
    v ^? left_ `shouldShowTo` "Just 'x'"
    v ^? right_ `shouldBe` Nothing
    v ^? up_ `shouldBe` Nothing
    v2 ^? left_ `shouldShowTo` "Just ()"

  it "Setting the missing tag does nothing" $ do
    set right_ () v `shouldShowTo` "V{left='x'}"

    set _Right () (Left 'x') -- prisms for Either do the same thing
      `shouldShowTo` "Left 'x'"

  it "compose prism" $ do
    v3 ^? up_.up_ `shouldBe` Nothing
    v3 ^? left_ `shouldShowTo` "Just 'x'"

    v4 ^? left_.left_ `shouldShowTo` "Just \"leftleft\""

  it "compose lens.prism" $ do
    r2 ^? down_.left_ `shouldShowTo` "Just 'x'"
    r2 ^? down_.right_ `shouldBe` Nothing

    let du = down_.up_
    r2 ^? du `shouldBe` Nothing

  it "extension" $ do
    v5 ^? down_ `shouldBe` Just "hi"
    v6 ^? down_ `shouldBe` Just "hi"
    v7 ^? down_ `shouldBe` Nothing
    v7 ^? left_ `shouldBe` Just 'x'

  it "show" $ do
    vs `shouldShowTo`
        "Record{v=V{left='x'},\
        \v2=V{left=()},\
        \v2'=V{left=()},\
        \v3=V{left='x'},\
        \v4=V{left=V{left=\"leftleft\"}},\
        \v5=V{down=\"hi\"},\
        \v6=V{down=\"hi\"},\
        \v7=V{left='x'}}"

    -- works in ghci. Probably need -XExtendedDefaultRules
    -- wX `shouldShowTo` "V{x='a'}"
    -- wY `shouldShowTo` "V{y=2.5}"
    [wX,wY] `shouldShowTo` "[V{x='a'},V{y=2.5}]"

  -- :t wX
  -- > wX :: Variant '[Tagged "x" Char, Tagged "y" y]
  --
  -- > :t wY
  -- > wY :: Variant '[Tagged "x" x, Tagged "y" Double]
  --
  -- ghc doesn't need to decide on a type for values that
  -- have no influence on the final result
  it "type partly defined" $ do
    wX ^? hLens' (Label :: Label "x")
        `shouldShowTo` "Just 'a'"
    wY ^? hLens' (Label :: Label "y")
        `shouldShowTo` "Just 2.5"
  

wX = mkVariant (Label :: Label "x") 'a' wProto
wY = mkVariant (Label :: Label "y") (2.5 :: Double) wProto

wProto = undefined :: Record
  '[Tagged "x" x, Tagged "y" y]

vs = [pun| v v2 v2' v3 v4 v5 v6 v7 |]

-- note that we can change the type of the 'x' field
-- from Char to ()
v2 = set (hPrism left) () v


-- or with the "better" label
v2' = set left_ () v


v3 = v & up_ .~ v & up_.up_ .~ "upup"
v4 = v & left_ .~ v & left_.left_ .~ "leftleft"
v5 = down .=. Just "hi" .*. v
v6 = down_ .==. Just "hi" .*. v
v7 = down .=. (Nothing :: Maybe String) .*. v
