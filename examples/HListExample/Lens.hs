{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds #-}
{- | Demonstrates @hLens@. See also labelable.hs which is more "convenient"

-}
module HListExample.Lens where
import Data.HList.CommonMain
import Control.Lens
import Hspec

makeLabels6 (words "x y z")


#if __GLASGOW_HASKELL__ > 707
yRec = y .=. 321 .*. x .=. 123 .*. emptyRecord
#else
-- defaulting doesn't work in ghc-7.6.3
yRec = y .=. (321 :: Integer) .*. x .=. (123 :: Integer) .*. emptyRecord
#endif

r = x .=. "hi" .*.
    y .=. yRec .*.
    emptyRecord


_ = (r^.y') `asTypeOf` hRearrange'
    (x .=. 1 .*. y .=. 1 .*. emptyRecord)

rSmall = x .=. "" .*. emptyRecord

x' a = hLens x a
y' a = hLens y a

main = do
    view (hLens x) r `shouldBe` "\"hi\""
    set (hLens x) () r `shouldBe` "Record{x=(),y=Record{y=321,x=123}}"

    r ^. hLens y . hLens x `shouldBe` "123"
    r & hLens y . hLens y .~ "xy" `shouldBe` "Record{x=\"hi\",y=Record{y=\"xy\",x=123}}"


    -- and now for with hLens applied second
    print (view x' r)
    print (set x' () r)

    print (r ^. y' . y')
    print (r & y' . y' .~ "xy")

    putStrLn "\n\nIsos"
    print (r & sameLength . unlabeled . hTuple . _1 .~ ())
    print (r & sameLength . unlabeled . hTuple . _2 .~ ())
    print (z .=. () .*. r
              & unlabeled' . from tipHList %~ ttip (\x z -> x ++ show (z :: ())))

    r ^. unlabeled . from tipHList & tipPutStrLn
    return ()


tipPutStrLn tip = ttipM ?? tip $ \x -> do
  putStrLn x
  return x
