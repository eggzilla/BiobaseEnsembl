
-- |

module Biobase.GTF.Export where
import Biobase.GTF.Types
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import Data.List

instance Show GTF where
  show (GTF _Entries _Sequence)
    | not (null _Entries) = entriesString
    | otherwise = ""
    where entriesString =  concatMap show _Entries

instance Show GTFEntry where
  show (GTFEntry _gtfSeqid _gtfSource _gtfType _gtfStart _gtfEnd _gtfScore _gtfStrand _gtfPhase _gtfAttributes) =
    (B.unpack _gtfSeqid) ++ "\t" ++ (B.unpack _gtfSource) ++ "\t" ++ (B.unpack _gtfType) ++ "\t" ++ show _gtfStart ++ "\t" ++ show _gtfEnd ++ "\t" ++ (B.unpack _gtfScore) ++ "\t" ++ [_gtfStrand] ++  "\t" ++ (B.unpack _gtfPhase) ++ "\t" ++ (intercalate ";" (V.toList (V.map B.unpack _gtfAttributes))) ++ "\n"
