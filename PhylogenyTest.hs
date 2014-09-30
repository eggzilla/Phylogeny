-- | Parser test script
-- /scr/kronos/egg/ghc/ghc/bin/runghc PhylogenyTest.hs query2.dnd

module Main where
    
import System.Environment (getArgs)
import System.Process 
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import Data.List
import Bio.Phylogeny
import System.Directory
import System.Process
import Control.Monad    
import Data.Either.Unwrap
import Data.Graph.Inductive

    
main = do
  args <- getArgs
  let input_file = (head args)
  parsedPhylogeny <- readGraphNewick input_file
  let treedrawing = drawPhylogeneticGraph (fromRight parsedPhylogeny)
  putStr (concat treedrawing)
  --putStrLn "-----"
  --print (fromRight parsedPhylogeny)
