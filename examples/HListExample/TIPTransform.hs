{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}

-- Transforming a TIP: applying to a TIP a (polyvariadic) function
-- that takes arguments from a TIP and updates the TIP with the result.
-- 
-- In more detail: we have a typed-indexed collection TIP and we
-- would like to apply a transformation function to it, whose argument
-- types and the result type are all in the TIP. The function should locate
-- its arguments based on their types, and update the TIP
-- with the result. The function may have any number of arguments,
-- including zero; the order of arguments should not matter.

-- The problem was posed by Andrew U. Frank on Haskell-Cafe, Sep 10, 2009.
-- http://www.haskell.org/pipermail/haskell-cafe/2009-September/066217.html
-- The problem is an interesting variation of the keyword argument problem.

module HListExample.TIPTransform where

import Data.HList.CommonMain
import Data.Typeable

import Properties.Common
import Test.Hspec

-- We start with the examples

newtype MyVal = MyVal Int deriving (Show, Typeable)

-- or if no typeable, use
-- instance ShowLabel MyVal where showLabel _ = "MyVal"

tip1 = MyVal 20 .*. (1::Int) .*. True .*. emptyTIP


mainTIPTransform = describe "tipTransform" $ it "all" $ do
  tip1 `shouldShowTo` "TIPH[MyVal 20,1,True]"

  -- Update the Int component of tip1 to 2. The Int component must
  -- exist. Otherwise, it is a type error
  ttip (2::Int) tip1 `shouldShowTo`
        "TIPH[MyVal 20,2,True]"

  -- Negate the boolean component of tip1
  ttip not tip1 `shouldShowTo`
        "TIPH[MyVal 20,1,False]"

  -- Update the Int component from the values of two other components
  ttip (\(MyVal x) y -> x+y) tip1 `shouldShowTo`
    "TIPH[MyVal 20,21,True]"

  -- Update the MyVal component from the values of three other components
  ttip (\b (MyVal x) y -> MyVal $ if b then x+y else 0) tip1
        `shouldShowTo`
        "TIPH[MyVal 21,1,True]"

  -- The same but with the permuted argument order.
  -- The order of arguments is immaterial: the values will be looked up using
  -- their types
  ttip (\b y (MyVal x)-> MyVal $ if b then x+y else 0) tip1
        `shouldShowTo`
        "TIPH[MyVal 21,1,True]"

-- The implementation
-- part of HList proper now
