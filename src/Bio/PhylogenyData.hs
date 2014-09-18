-- | This module contains data structures for phylogeny data

module Bio.PhylogenyData where
import Data.Tree

data PhylogenyNode = SimpleTaxDumpNode
  {
   -- node id in GenBank
   phylogenyId :: String,
   -- parent node id in GenBank taxonomy database
   distance :: Double
  }
  deriving (Show, Read, Eq)
