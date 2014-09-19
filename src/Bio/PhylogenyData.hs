-- | This module contains data structures for phylogeny data

module Bio.PhylogenyData where
import Data.Tree

data PhylogenyNode = PhylogenyNode
  {
   -- node id in GenBank
   phylogenyId :: Maybe String,
   -- parent node id in GenBank taxonomy database
   distance :: Maybe Double
  }
  deriving (Show, Read, Eq)
