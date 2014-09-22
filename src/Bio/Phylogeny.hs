-- | Parse and process phylogeny data

module Bio.Phylogeny (                      
                       module Bio.PhylogenyData,
                       parseNewick,
                       readNewick,
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
import qualified Data.Either.Unwrap as E

--------------------------------------------------------

-- | Parse newick tree format from input string
parseNewick input = parse genParserNewickFormat "parseNewickFormat" input

-- | Parse  from input filePath                        
readNewick filePath = parseFromFile genParserNewickFormat filePath

--genParserNewickFormat :: GenParser Char st Tree PhylogenyNode
genParserNewickFormat = do
  tree <- genParserNewickTree 
  char ';'
  optional eof
  return tree

--genParserNewickTree :: GenParser Char st Tree PhylogenyNode
genParserNewickTree = do
  subtrees <- many (choice [(try genParserNewickSubTreeLeft), (try genParserNewickSubTreeRight), (try genParserNewickLeaf), (try genParserNewickBranch)])
  return subtrees

--Tree Leaf [x,..]
--genParserNewickSubTreeLeft :: GenParser Char st Tree PhylogenyNode []
genParserNewickSubTreeLeft = do 
  leaf1 <- try genParserPhylogenyNode
  char '('
  optional (char '\n')
  subtree <- try genParserNewickTree
  char ')'
  optional (char ',')
  optional (char '\n')
  return $ Node (Just leaf1) subtree

--Tree Leaf [x,..]
--genParserNewickSubTreeRight :: GenParser Char st Tree PhylogenyNode []
genParserNewickSubTreeRight = do
  char '('
  optional (char '\n')
  subtree <- try genParserNewickTree
  char ')'
  optional (char '\n')
  leaf1 <- try genParserPhylogenyNode
  optional (char ',')
  optional (char '\n')
  return $ Node (Just leaf1) subtree

genParserNewickBranch = do
  char '('
  optional (char '\n')
  subtree <- try genParserNewickTree
  char ')'
  optional (char '\n')
  optional (char ',')
  optional (char '\n')
  return $ Node Nothing subtree

--Tree Leaf []
--genParserNewickLeaf :: GenParser Char st Tree PhylogenyNode []
genParserNewickLeaf = do
  leaf1 <- try genParserPhylogenyNode
  optional (char ',')
  optional (char '\n')
  return $ Node (Just leaf1) []

--Tree Leaf []
--genParserPhylogenyNode:: GenParser Char st PhylogenyNode
genParserPhylogenyNode = do
  nodeId <- optionMaybe (try (many (choice [alphaNum,(oneOf "./|_-")])))
  (char ':')
  nodeDistance <- optionMaybe (many (choice [digit, char '.']))
  choice [try (lookAhead (char ',')), try (lookAhead (char ')')), try (lookAhead (char '(')), try (lookAhead (char ';'))]
  return $ PhylogenyNode nodeId (liftM readDouble nodeDistance)

---------------------------
--Auxiliary functions:
readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read
