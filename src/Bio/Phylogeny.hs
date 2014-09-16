-- | Parse and process phylogeny data

module Bio.Phylogeny (                      
                       module Bio.PhylogenyData,
                       parseNewick,
                       readNewick,
                      ) where
import Prelude 
import System.IO 
import Text.Parsec.Prim
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
parseNewick input = parse genParserNewick "parseNewick" input

-- | Parse  from input filePath                        
readNewick filePath = parseFromFile genParserNewick filePath
