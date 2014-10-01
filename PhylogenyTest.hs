-- | Parser test script
-- /scr/kronos/egg/ghc/ghc/bin/runghc PhylogenyTest.hs query2.dnd

module Main where
    
import System.Environment (getArgs)
import System.Process 
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import Data.List
import Bio.PhylogenyTools
import Bio.PhylogenyParser
import System.Directory
import System.Process
import Control.Monad    
import Data.Either.Unwrap
import Data.Graph.Inductive

main = do
  args <- getArgs
  let input_file = (head args)
  -- read as phylogeny graph
  parsedPhylogeny <- readGraphNewick input_file
  -- Output graph as test.dot file, can be rendered as svg with 
  -- cat test.dot | dot -Tsvg > phylo.svg
  putStrLn "-----"
  print (fromRight parsedPhylogeny)
  putStrLn "-----"
  let (currentPathLengths,pairs)= unzip (pathLengthsIndexed (fromRight parsedPhylogeny))
  print currentPathLengths
  print pairs
  let treedrawing = drawPhylogeneticGraph (fromRight parsedPhylogeny)
  writeFile "test.dot" treedrawing
