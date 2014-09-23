{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Parse and process phylogeny data

module Bio.Phylogeny (                      
                       module Bio.PhylogenyData,
                       parseNewick,
                       readNewick,
                       drawPylogeneticTree
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
--import qualified Data.Tree.Zipper as TZ
import Data.List
import Data.Either
import qualified Data.Either.Unwrap as E

--------------------------------------------------------

-- | Parse newick tree format from input string
parseNewick input = parse genParserNewickFormat "parseNewickFormat" input

-- | Parse  from input filePath                        
readNewick filePath = parseFromFile genParserNewickFormat filePath

--draw Tree
drawPylogeneticTree :: [Tree PhylogenyNode] -> String
drawPylogeneticTree inputTree = output
 where stringTree = map showPhylogenyNode inputTree
       output = drawForest stringTree

showPhylogenyNode :: Tree PhylogenyNode -> Tree String
showPhylogenyNode (Node node children) = Node ((phyloIdfromMaybe (phylogenyId node)) ++ " " ++ (phyloDistancefromMaybe (distance node))) (map showPhylogenyNode children)

phyloIdfromMaybe :: Maybe String -> String
phyloIdfromMaybe id = fromMaybe "NA" id

phyloDistancefromMaybe :: Maybe Double -> String
phyloDistancefromMaybe distance 
  | isJust distance = show (fromJust distance)
  | otherwise = "noDist"

-- parse Tree
genParserNewickFormat :: GenParser Char st [Tree PhylogenyNode]
genParserNewickFormat = do
  tree <- genParserNewickTree 
  char ';'
  optional eof
  return tree

genParserNewickTree :: GenParser Char st [Tree PhylogenyNode]
genParserNewickTree = do
  subtrees <- many (choice [(try genParserNewickLeaf),(try genParserNewickSubTreeRight), (try genParserNewickSubTreeLeft), (try genParserNewickBranch)])
  return subtrees

--Tree Leaf [x,..]
genParserNewickSubTreeLeft :: GenParser Char st (Tree PhylogenyNode)
genParserNewickSubTreeLeft = do 
  leaf <- choice [try genParserPhylogenyFullNode, try genParserPhylogenyIdNode, try genParserPhylogenyDistanceNode]
  char '('
  optional (char '\n')
  subtree <- try genParserNewickTree
  char ')'
  optional (char ',')
  optional (char '\n')
  return $ Node leaf subtree

--Tree Leaf [x,..]
genParserNewickSubTreeRight :: GenParser Char st (Tree PhylogenyNode)
genParserNewickSubTreeRight = do
  char '('
  optional (char '\n')
  subtree <- try genParserNewickTree
  char ')'
  optional (char '\n')
  leaf <- try genParserNewickNode
  optional (char ',')
  optional (char '\n')
  return $ Node leaf subtree

--Tree Nothing [x,..]
genParserNewickBranch :: GenParser Char st (Tree PhylogenyNode)
genParserNewickBranch = do
  char '('
  optional (char '\n')
  subtree <- try genParserNewickTree
  char ')'
  optional (char '\n')
  optional (char ',')
  optional (char '\n')
  return $ Node (PhylogenyNode Nothing Nothing) subtree

--Tree Leaf []
genParserNewickLeaf :: GenParser Char st (Tree PhylogenyNode)
genParserNewickLeaf = do
  node <- try genParserNewickNode
  return $ Node node [] 

-- Node
genParserNewickNode :: GenParser Char st PhylogenyNode
genParserNewickNode = do
  leaf1 <- choice [try genParserPhylogenyFullNode, try genParserPhylogenyDistanceNode, try genParserPhylogenyIdNode]
  optional (char ',')
  --choice [try (char ','), try (char ')'), try (char ';')]
  optional (char '\n')
  return leaf1

--Phylogeny Id Node
genParserPhylogenyIdNode:: GenParser Char st PhylogenyNode
genParserPhylogenyIdNode = do
  nodeId <- (try (many1 (choice [alphaNum,(oneOf ".\\/:|_-")])))
  choice [try (lookAhead (char ',')), try (lookAhead (char ')')), try (lookAhead (char '(')), try (lookAhead (char ';'))]
  return $ PhylogenyNode (Just nodeId) Nothing

--Phylogeny Distance Node
genParserPhylogenyDistanceNode:: GenParser Char st PhylogenyNode
genParserPhylogenyDistanceNode = do
  (char ':')
  nodeDistance <- many1 (choice [try digit, try (char '.')])
  choice [try (lookAhead (char ',')), try (lookAhead (char ')')), try (lookAhead (char '(')), try (lookAhead (char ';'))]
  return $ PhylogenyNode Nothing (Just (readDouble nodeDistance))

--Phylogeny Full Node
genParserPhylogenyFullNode:: GenParser Char st PhylogenyNode
genParserPhylogenyFullNode = do
  nodeId <- (try (many1 (choice [alphaNum,(oneOf ".\\/:|_-")])))
  (char ':')
  nodeDistance <- many1 (choice [try digit, try (char '.')])
  choice [try (lookAhead (char ',')), try (lookAhead (char ')')), try (lookAhead (char '(')), try (lookAhead (char ';'))]
  return $ PhylogenyNode (Just nodeId) (Just (readDouble nodeDistance))

---------------------------
--Auxiliary functions:
readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read
