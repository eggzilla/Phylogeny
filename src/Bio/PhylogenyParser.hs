{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Parse and process phylogeny data

module Bio.PhylogenyParser (                      
                       module Bio.PhylogenyData,
                       parseGraphNewick,
                       readGraphNewick
                      ) where
import Prelude 
import System.IO 
import Bio.PhylogenyData
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad
import Data.Tree
import Data.List
import Data.Either
import Data.Tuple
import qualified Data.Text.Lazy as TL
import Data.Graph.Inductive
import qualified Data.Either.Unwrap as E
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Printing as GVP
--------------------------------------------------------

-- | Parse newick tree format from input string
parseGraphNewick input = runParser genParserGraphNewickFormat 1 "parseGraphNewick:" 

-- | Parse from input filePath                        
readGraphNewick filePath = do
  phylogenyRaw <- readFile filePath
  let phylogenyParsed = parseGraphNewick (filter (/= '\n') phylogenyRaw)
  return phylogenyParsed

--Graph representation
genParserGraphNewickFormat :: GenParser Char Int (Gr String Double)
genParserGraphNewickFormat = do
  char '('
  currentIndex <- getState
  setState (currentIndex + 1)
  children <- many1 (choice [try (genParserGraphNode currentIndex), try (genParserGraphInternal currentIndex), try (genParserGraphLeaf currentIndex)])
  char ')'
  maybeNodeId <- optionMaybe (try (many1 (choice [alphaNum, oneOf ".\\/:|_-"])))
  let nodeId = fromMaybe "internal" maybeNodeId
  char ';'
  optional eof
  let currentNode = (currentIndex,nodeId)
  let (otherNodes,otherEdges) =  unzip children
  let nodes = currentNode: concat otherNodes
  let edges = concat otherEdges
  return (mkGraph nodes edges)

genParserGraphNode :: Int -> GenParser Char Int ([(Int, String)],[(Int,Int,Double)])
genParserGraphNode parentNodeIndex = do
  optional (try (char ','))
  try (char '(')
  currentIndex <- getState
  setState (currentIndex + 1)
  children <- many1 (choice [try (genParserGraphLeaf currentIndex), try (genParserGraphInternal currentIndex),try (genParserGraphNode currentIndex)])
  char ')'
  nodeId <- try (many1 (choice [alphaNum, oneOf ".\\/|_-"]))
  edgeDistance <- genParserEdgeDistance
  choice [try (lookAhead (char ',')), try (lookAhead (char ')')), try (lookAhead (char '(')), try (lookAhead (char ';'))]
  let currentNode = (currentIndex,nodeId)
  let currentEdges = [(parentNodeIndex,currentIndex,edgeDistance),(currentIndex,parentNodeIndex,edgeDistance)]
  let (otherNodes,otherEdges) =  unzip children
  let nodes = currentNode: concat otherNodes
  let edges = currentEdges ++ concat otherEdges
  return (nodes,edges)

genParserGraphInternal :: Int -> GenParser Char Int ([(Int, String)],[(Int,Int,Double)])
genParserGraphInternal parentNodeIndex = do
  optional (try (char ','))
  try (char '(')
  currentIndex <- getState
  setState (currentIndex + 1)
  children <- many1 (choice [try (genParserGraphLeaf currentIndex), try (genParserGraphInternal currentIndex), try (genParserGraphNode currentIndex)])
  char ')'
  edgeDistance <- genParserEdgeDistance
  choice [try (lookAhead (char ',')), try (lookAhead (char ')')), try (lookAhead (char '(')), try (lookAhead (char ';'))]
  let currentNode = (currentIndex, "internal")
  let currentEdges = [(parentNodeIndex,currentIndex,edgeDistance),(currentIndex,parentNodeIndex,edgeDistance)]
  let (otherNodes,otherEdges) =  unzip children
  let nodes = currentNode: concat otherNodes
  let edges = currentEdges ++ concat otherEdges
  return (nodes,edges)

genParserGraphLeaf :: Int -> GenParser Char Int ([(Int, String)],[(Int,Int,Double)])
genParserGraphLeaf parentNodeIndex = do
  optional (try (char ','))
  nodeId <- try (many1 (choice [alphaNum, oneOf ".\\/|_-"]))
  edgeDistance <- try genParserEdgeDistance
  choice [try (lookAhead (char ',')), try (lookAhead (char ')')), try (lookAhead (char '(')), try (lookAhead (char ';'))]
  currentIndex <- getState
  setState (currentIndex + 1)
  let currentNode = [(currentIndex,nodeId)]
  let currentEdge = [(parentNodeIndex,currentIndex,edgeDistance),(currentIndex,parentNodeIndex,edgeDistance)]
  return (currentNode,currentEdge)

genParserEdgeDistance :: GenParser Char st Double
genParserEdgeDistance = do
  try (char ':')
  nodeDistance <- many1 (choice [try digit, try (char '.'), try (char '-')])
  return (readDouble nodeDistance)

---------------------------
--Auxiliary functions:
readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read
