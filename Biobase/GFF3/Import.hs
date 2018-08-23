{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Parses NCBI BLAST+ tabular output

module Biobase.BLAST.Import (gff3FromFile,
                             parseGFF3,
                            ) where

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
import Biobase.GFF3.Types

-- | reads and parses GFF3 from provided filePath
gff3FromFile :: String -> IO [GFF3]
gff3FromFile filePath = do
  printf "# reading GFF3 from file %s\n" filePath
  fileExists <- doesFileExist filePath
  if fileExists
     then parseGFF3s <$> B.readFile filePath
     else fail "# GFF3 file \"%s\" does not exist\n" filePath

-- | Read a lazy bytestring and stream out a list of @GFF3s@'s.
-- In case, there is a parse error "late" in the file, we might have
-- already streamed out some (or many!) of these results.

parseGFF3s :: B.ByteString -> [GFF3]
parseGFF3s = go
  where go xs = case L.parse genParseGFF3 xs of
          L.Fail remainingInput ctxts err  -> error $ "parseGFF3s failed! " ++ err ++ " ctxt: " ++ show ctxts ++ " head of remaining input: " ++ B.unpack (B.take 1000 remainingInput)
          L.Done remainingInput btr
            | B.null remainingInput  -> [btr]
            | otherwise              -> btr : go remainingInput

genParseGFF3 :: Parser GFF3
genParseGFF3 = do
  string "##gff-version 3"
  endOfLine
  _entry <- many (try genParseGFF3Entry) <?> "GFF3 entry"
  return $ GFF3 (V.fromList _entry) B.empty

genParseGFF3Entry :: Parser GFF3Entry
genParseGFF3Entry = do
  _gff3Seqid <- takeWhile1 (/=\t) <?> "seqid"
  char '\t'
  _gff3Source <- takeWhile1 (/=\t) <?> "source"
  char '\t'
  _gff3Type <- takeWhile1 (/=\t) <?> "type"
  char '\t'
  _gff3Start <- decimal  <?> "start"
  char '\t'
  _gff3End <- decimal <?> "end"
  char '\t'
  _gff3Score <- decimal <?> "score"
  char '\t'
  _gff3Strand <- (choice [char '+', char '-', char '.']) <?> "strand"
  char '\t'
  _gff3Phase <- decimal  <?> "phase"
  char '\t'
  _gff3Attributes <- many (try genParseGFF3Attribute) <?> "GFF3 entry" "attributes"
  endOfLine
  return $ GFF3Entry (B.fromStrict _gff3Seqid) (B.fromStrict _gff3Source) (B.fromStrict _gff3Type) _gff3Start _gff3End _gff3Score _gff3Strand _gff3Phase _gff3Attributes

genParseGFF3Attribute :: Parser L.ByteString
genParseGFF3Attribute = do
  _gff3Atribute <- takeWhile1 (/=;) <?> "seqid"
  _ <- option char ';'
  return $ (B.fromStrict _gff3Atribute)

--IUPAC amino acid with gap
--aminoacidLetters :: Char -> Bool
aminoacidLetters = inClass "ARNDCQEGHILMFPSTWYVBZX-"

--IUPAC nucleic acid characters with gap
--nucleotideLetters :: Char -> Bool
nucleotideLetters = inClass "AGTCURYSWKMBDHVN-."

--IUPAC nucleic acid characters with gap
--bioLetters :: Char -> Bool
bioLetters = inClass "ABCDEFGHIJKLMNOPQRSTUVWXYZ.-"


toLB :: C.ByteString -> B.ByteString
toLB = S.toLazyByteString . S.byteString
