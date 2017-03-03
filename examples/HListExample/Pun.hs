{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- more examples for record puns
module HListExample.Pun where
import Data.HList.CommonMain

import Test.Hspec
import Properties.Common

makeLabels6 (words "a b c")


r  = c .=. "c" .*. b .=. (a .=. 3 .*. emptyRecord) .*. emptyRecord
r2 = b .=. (a .=. 1 .*. emptyRecord) .*. emptyRecord


p1 ( (.!. b) -> (b @ ((.!. a) -> a))) = (a,b)

p2 [pun| b @ {a} |] = (a, b)

-- same as p2, but gives a warning
-- p3 [pun| b @ a |] = (a, b)

p4 [pun| b{a} |] = a -- b is not bound

-- adds `x' and `y' into a field called r
e1 = let x = 1; y = "hi" in [pun| r @ { x y } |]

-- updates the `c' field
e2 = let c = 1; y = "hi" in [pun| r @ { c y } |]

-- same as e1, but doesn't use a pre-existing r
e3 = let x = 1; y = "hi" in [pun| r { x y } |]


mainPun = describe "pun quasiquoter" $ do
  it "pattern" $ do
        p1 r `shouldShowTo` "(3,Record{a=3})"
        p2 r `shouldShowTo` "(3,Record{a=3})"
        p4 r `shouldBe` 3

  it "expression" $ do
        e1 `shouldShowTo` "Record{r=Record{x=1,y=\"hi\",c=\"c\",b=Record{a=3}}}"
        e2 `shouldShowTo` "Record{r=Record{c=1,y=\"hi\",b=Record{a=3}}}"
        e3 `shouldShowTo` "Record{r=Record{x=1,y=\"hi\"}}"

