{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Parse and process phylogeny data

module Bio.PhylogenyTools (                      
                       module Bio.PhylogenyData,
                       drawPylogeneticTree,
                       drawPhylogeneticGraph
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

--draw Graph
drawPhylogeneticGraph :: (Gr String Double) -> String
drawPhylogeneticGraph inputGraph = do
  let dotFormat = GV.graphToDot GV.nonClusteredParams inputGraph
  let text = GVP.renderDot $ GVP.toDot dotFormat
  TL.unpack text
