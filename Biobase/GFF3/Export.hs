
-- |

module Biobase.GFF3.Export where
import Biobase.GFF3.Types
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import Data.List

instance Show GFF3 where
  show (GFF3 _Entries _Sequence)
    | not (null _Entries) = header ++ entriesString
    | otherwise = header
    where header = "##gff-version 3\n"
          entriesString =  concatMap show _Entries

instance Show GFF3Entry where
  show (GFF3Entry _gff3Seqid _gff3Source _gff3Type _gff3Start _gff3End _gff3Score _gff3Strand _gff3Phase _gff3Attributes) =
    (B.unpack _gff3Seqid) ++ "\t" ++ (B.unpack _gff3Source) ++ "\t" ++ (B.unpack _gff3Type) ++ "\t" ++ show _gff3Start ++ "\t" ++ show _gff3End ++ "\t" ++ (B.unpack _gff3Score) ++ "\t" ++ [_gff3Strand] ++  "\t" ++ (B.unpack _gff3Phase) ++ "\t" ++ (intercalate ";" (V.toList (V.map B.unpack _gff3Attributes))) ++ "\n"
