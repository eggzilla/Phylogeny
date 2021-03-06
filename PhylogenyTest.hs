-- | Parser test script
-- runghc -package-db --ghc-arg=/home/egg/current/Projects/Haskell/Phylogeny/.cabal-sandbox/x86_64-linux-ghc-8.0.1-packages.conf.d/ /home/egg/current/Projects/Haskell/Phylogeny/PhylogenyTest.hs tree.dnd

module Main where
    
import System.Environment (getArgs)
import System.Process 
--import Text.ParserCombinators.Parsec
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
  --putStrLn "-----"
  --print (fromRight parsedPhylogeny)
  --putStrLn "-----"
  --let indexedPathLengths = pathLengthsIndexed (fromRight parsedPhylogeny)
  --let (currentPathLengths,pairs)= unzip indexedPathLengths
  --print currentPathLengths
  --print pairs
  --let averagePathLengths = averagePathLengthperNodes indexedPathLengths
  --putStrLn "Average path lengths per node:"
  --print averagePathLengths
  let treedrawing = drawPhylogeneticGraph (fromRight parsedPhylogeny)
  writeFile "test.dot" treedrawing
