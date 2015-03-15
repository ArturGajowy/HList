import Data.List
import System.Process
import System.Exit
import System.IO
import Data.Char
import System.Environment

-- | tests that all the >>> comments are followed by correct output. Easiest is to
--
-- > cabal test
--
-- or
--
-- > runghc examples/rundoctests.hs
--
-- or
--
-- > runghc examples/rundoctests.hs Data/HList/File1.hs Data/HList/File2.hs
--
-- you need Cabal >= 1.18 since that's around when cabal repl got added.
main = do
    as <- getArgs
    o <- readProcess
        "cabal" ["repl","--ghc-options","-v0 -w"]
        ":show packages\n:show language"
    let flags = words $ unlines
                    $ filter (\f -> not $ "template-haskell" `isInfixOf` f)
                    $ filter ((=="-") . take 1 . dropWhile isSpace)
                    $ lines o

    let files = case as of
            [] -> ["Data/HList/CommonMain.hs",
                   "Data/HList/HList.hs",
                   "Data/HList/Record.hs",
                   "Data/HList/Labelable.hs",
                   "Data/HList/HSort.hs"]
            _ -> as

    let args = "-i.": "-idist/build/autogen":
             "-optP-include":
             "-optPdist/build/autogen/cabal_macros.h" :
             "-Idist/build/autogen" : "-w":
             files ++ flags
    readProcess "doctest" args ""

