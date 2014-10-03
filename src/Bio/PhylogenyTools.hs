{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Parse and process phylogeny data

module Bio.PhylogenyTools (                      
                       module Bio.PhylogenyData,
                       drawPhylogeneticGraph,
                       pathLengths,
                       pathLengthsIndexed,
                       averagePathLengthperNodes,
                       compareAveragePathLengths,
                       minimumAveragePathLength,
                       maximumAveragePathLength,
                       getLabel
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

--draw Graph
drawPhylogeneticGraph :: (Gr String Double) -> String
drawPhylogeneticGraph inputGraph = do
  let dotFormat = GV.graphToDot GV.nonClusteredParams inputGraph
  let text = GVP.renderDot $ GVP.toDot dotFormat
  TL.unpack text

--Paths
-- | Computes distance between all nodes in the graph
pathLengths :: (Gr String Double) -> [Double]
pathLengths inputGraph = pathLengths
  where nonInternalLabeledNodes = filter (\(_,label) -> label /= "internal") (labNodes inputGraph)
        nonInternalNodes = map (\(node,_) -> node) nonInternalLabeledNodes
        pairs = map toPair (sequence [(nonInternalNodes),(nonInternalNodes)])
        --we are not considering distance to self and the upper triangular part of the distance matrix
        nonselfPairs = filter (\pair -> (fst pair) /= (snd pair)) pairs 
        upperTriangularNonselfPairs = take ((length nonselfPairs) `div` 2) nonselfPairs
        pathLengths = map (\pair -> spLength (fst pair) (snd pair) inputGraph) upperTriangularNonselfPairs

-- | Computes distance between all nodes in the graph including the corresponding node indices
pathLengthsIndexed :: (Gr String Double) -> [(Double,(Node,Node))]
pathLengthsIndexed inputGraph = pathLengthsIndexed
  where nonInternalLabeledNodes = filter (\(_,label) -> label /= "internal") (labNodes inputGraph)
        nonInternalNodes = map (\(node,_) -> node) nonInternalLabeledNodes
        pairs = map toPair (sequence [(nonInternalNodes),(nonInternalNodes)])
        --we are not considering distance to self and the upper triangular part of the distance matrix
        nonselfPairs = filter (\pair -> (fst pair) /= (snd pair)) pairs 
        upperTriangularNonselfPairs = take ((length nonselfPairs) `div` 2) nonselfPairs
        pathLengths = map (\pair -> spLength (fst pair) (snd pair) inputGraph) upperTriangularNonselfPairs
        pathLengthsIndexed = zip pathLengths upperTriangularNonselfPairs

averagePathLengthperNodes :: [(Double,(Node,Node))] -> [(Node,Double)]
averagePathLengthperNodes indexedPathLengths = averagePathLengthperNodes
  where nodeIndices = nub (concatMap (\(_,(nodeIndex,nodeIndex2)) -> [nodeIndex,nodeIndex2]) indexedPathLengths)
        averagePathLengthperNodes = map (averagePathLengthperNode indexedPathLengths) nodeIndices
        
                     
averagePathLengthperNode :: [(Double,(Node,Node))] -> Node -> (Node,Double)
averagePathLengthperNode indexedPathLengths nodeIndex = (nodeIndex,averagePathLength)
  where pathLengthsperNode = filter (\(_,(a,b)) -> a == nodeIndex || b == nodeIndex) indexedPathLengths
        sumPathLengths = sum (map (\(pathLength,(_,_)) -> pathLength) pathLengthsperNode)
        averagePathLength = sumPathLengths / fromIntegral (length pathLengthsperNode)

minimumAveragePathLength :: [(Node,Double)] -> (Node,Double)
minimumAveragePathLength averagePathLengthperNode = minimumBy compareAveragePathLengths averagePathLengthperNode

maximumAveragePathLength :: [(Node,Double)] -> (Node,Double)
maximumAveragePathLength averagePathLengthperNode = maximumBy compareAveragePathLengths averagePathLengthperNode

getLabel :: (Gr String Double) -> Node -> String
getLabel parsedNewick inputNode = nodeLabel
  where nodeLabels = labNodes parsedNewick
        labeledNode = fromJust (find (\(index,label) -> index == inputNode) nodeLabels)
        nodeLabel = snd labeledNode

--auxiliary functions
toPair [a,b] = (a,b)

compareAveragePathLengths :: (Node,Double) -> (Node,Double) -> Ordering
compareAveragePathLengths (_,length1) (_,length2)
  | length1 > length2 = LT
  | length1 < length2 = GT
  -- in case of equal evalues the first hit is selected
  | length1 == length2 = EQ

