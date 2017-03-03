{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
module Properties.KW where


import Properties.Common
import Test.QuickCheck
import Data.HList.CommonMain
import Test.Hspec


kwSpecs = describe "kw" $ do

#if (__GLASGOW_HASKELL__ > 799)
    return ()
    {- with NoMonoLocalBinds
     - /home/aavogt/wip/HList/HList/examples/Properties/KW.hs:59:15: error:ghc: panic! (the 'impossible' happened)
  (GHC version 8.0.2 for x86_64-unknown-linux):
	No skolem info: k_aqoh[sk]

        with MonoLocalBinds, I think the error is the same as in earlier versions

        /home/aavogt/wip/HList/HList/examples/Properties/KW.hs:62:15: error:
            • Couldn't match type ‘'[Tagged "x" (BoolN "x")]’ with ‘'[]’
              Expected type: Record '[]
                Actual type: HExtendR (Tagged "x" (BoolN "x")) (Record '[])
            • In the first argument of ‘f2’, namely
                ‘(lx .=. x2 .*. emptyRecord)’
              In the first argument of ‘eq’, namely
                ‘f2 (lx .=. x2 .*. emptyRecord)’
              In the expression: f2 (lx .=. x2 .*. emptyRecord) `eq` f1 x2 y
                     
        /home/aavogt/wip/HList/HList/examples/Properties/KW.hs:63:15: error:
            • Couldn't match type ‘'[Tagged "y" (BoolN "y")]’ with ‘'[]’
              Expected type: Record '[]
                Actual type: HExtendR (Tagged "y" (BoolN "y")) (Record '[])
            • In the first argument of ‘f2’, namely
                ‘(ly .=. y2 .*. emptyRecord)’
              In the first argument of ‘eq’, namely
                ‘f2 (ly .=. y2 .*. emptyRecord)’
              In the expression: f2 (ly .=. y2 .*. emptyRecord) `eq` f1 x y2
                     
        /home/aavogt/wip/HList/HList/examples/Properties/KW.hs:64:15: error:
            • Couldn't match type ‘'[Tagged "x" (BoolN "x"),
                                     Tagged "y" (BoolN "y")]’
                             with ‘'[]’
              Expected type: Record '[]
                Actual type: HExtendR
                               (Tagged "x" (BoolN "x")) (Record '[Tagged "y" (BoolN "y")])
            • In the first argument of ‘f2’, namely
                ‘(lx .=. x2 .*. ly .=. y2 .*. emptyRecord)’
              In the first argument of ‘eq’, namely
                ‘f2 (lx .=. x2 .*. ly .=. y2 .*. emptyRecord)’
              In the expression:
                f2 (lx .=. x2 .*. ly .=. y2 .*. emptyRecord) `eq` f1 x2 y2
                     
        /home/aavogt/wip/HList/HList/examples/Properties/KW.hs:65:15: error:
            • Couldn't match type ‘'[Tagged "y" (BoolN "y"),
                                     Tagged "x" (BoolN "x")]’
                             with ‘'[]’
              Expected type: Record '[]
                Actual type: HExtendR
                               (Tagged "y" (BoolN "y")) (Record '[Tagged "x" (BoolN "x")])
            • In the first argument of ‘f2’, namely
                ‘(ly .=. y2 .*. lx .=. x2 .*. emptyRecord)’
              In the first argument of ‘eq’, namely
                ‘f2 (ly .=. y2 .*. lx .=. x2 .*. emptyRecord)’
              In the expression:
                f2 (ly .=. y2 .*. lx .=. x2 .*. emptyRecord) `eq` f1 x2 y2

-}
#else
    it "f1" $ property $ do
      (f1 :: BoolN "x" -> BoolN "y") <- arbitrary
      x :: BoolN "x" <- arbitrary
      x2 :: BoolN "x" <- arbitrary
      let f2 (Label :: Label "x") x () = f1 x
          f = f2 .*. recToKW [pun| x |]
      return $ conjoin
        [ kw f lx x2 () `eq` f1 x2,
          kw f () `eq` f1 x ]

    -- a function of two arguments can be made into a keyword function
    it "f2" $ property $ do
      (f1 :: BoolN "x" -> BoolN "y" -> BoolN "z") <- arbitrary
      x :: BoolN "x" <- arbitrary
      x2 :: BoolN "x" <- arbitrary
      y :: BoolN "y" <- arbitrary
      y2 :: BoolN "y" <- arbitrary

      let f2 (_ :: Label "x") x (_ :: Label "y") y () = f1 x y
          f = f2 .*. recToKW [pun| x y |]

      return $ conjoin
        [ kw f lx x2 ly y2 () `eq` f1 x2 y2,
          kw f ly y2 lx x2 () `eq` f1 x2 y2,
          kw f ly y2 () `eq` f1 x y2,
          kw f lx x2 () `eq` f1 x2 y,
          kw f () `eq` f1 x y ]

    -- alternatively, a function taking a record is pretty much
    -- a keyword argument. Error messages for missing keywords
    -- are a bit worse (blame hRearrange')
    it "f2Alt" $ property $ do
      (f1 :: BoolN "x" -> BoolN "y" -> BoolN "z") <- arbitrary
      x :: BoolN "x" <- arbitrary
      x2 :: BoolN "x" <- arbitrary
      y :: BoolN "y" <- arbitrary
      y2 :: BoolN "y" <- arbitrary

      let addDef new = hRearrange (Proxy :: Proxy [Label "x", Label "y"]) (new .<++. [pun| x y |])
          f2 (addDef  -> [pun| (x y) |]) = f1 x y
      return $ conjoin
        [ f2 emptyRecord `eq` f1 x y,
          f2 (lx .=. x2 .*. emptyRecord) `eq` f1 x2 y,
          f2 (ly .=. y2 .*. emptyRecord) `eq` f1 x y2,
          f2 (lx .=. x2 .*. ly .=. y2 .*. emptyRecord) `eq` f1 x2 y2,
          f2 (ly .=. y2 .*. lx .=. x2 .*. emptyRecord) `eq` f1 x2 y2
        ]

#endif
