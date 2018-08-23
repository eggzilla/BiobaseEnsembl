-- | GFF3 test script
--runghc -package-db --ghc-arg=.cabal-sandbox/x86_64-linux-ghc-8.0.1-packages.conf.d/ GFF3Test.hs tab.out

module Main where

import System.Environment (getArgs)
import System.Process
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import Data.List
import System.Directory
import System.Process
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import Biobase.GFF3.Import
import Biobase.GFF3.Types

main = do
  args <- getArgs
  let input_file = (head args)
  putStrLn "Test:"
  inputBlast <- L8.readFile input_file
  let gffOutput = parseGFF3 (L8.drop 0 inputBlast)
  print gffOutput
