
-- | Encoding of GTF

module Biobase.GTF.Types where

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8 hiding (isSpace)
import qualified Data.Attoparsec.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import System.Directory
import Data.Char
import Control.Monad
import Debug.Trace
import Text.Printf

  -- | Datastructure for GTF http://gmod.org/wiki/GTF
data GTF = GTF
    { gtfEntries :: !(V.Vector GTFEntry),
      gtfSequence :: !B.ByteString
    }
    deriving (Eq)

-- | Datastructure for data lines of GTF http://gmod.org/wiki/GTF
data GTFEntry = GTFEntry
    { gtfSeqid :: !B.ByteString,
      gtfSource :: !B.ByteString,
      gtfType :: !B.ByteString,
      gtfStart :: Int,
      gtfEnd :: Int,
      gtfScore :: !B.ByteString,
      gtfStrand :: Char,
      gtfPhase :: B.ByteString,
      gtfAttributes :: !(V.Vector B.ByteString)
    }
    deriving (Eq)
