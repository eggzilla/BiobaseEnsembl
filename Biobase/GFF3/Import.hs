{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Parses GFF3

module Biobase.GFF3.Import (gff3FromFile,
                             parseGFF3s,
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
import qualified Data.Word8 as W

-- | reads and parses GFF3 from provided filePath
gff3FromFile :: String -> IO [GFF3]
gff3FromFile filePath = do
  printf "# reading GFF3 from file %s\n" filePath
  fileExists <- doesFileExist filePath
  if fileExists
     then parseGFF3s <$> B.readFile filePath
     else fail ("# GFF3 file \"%s\" does not exist\n" ++ filePath)

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
  skipMany (try genParseGFF3Comment)
  _entry <- many1 (try genParseGFF3Entry) <?> "GFF3 entry"
  return $ GFF3 (V.fromList _entry) B.empty

genParseGFF3Comment :: Parser String
genParseGFF3Comment = do
  string "#"
  takeWhile1 (/= '\n')
  endOfLine
  return $ ""

genParseGFF3Entry :: Parser GFF3Entry
genParseGFF3Entry = do
  _gff3Seqid <- takeWhile1 (/= '\t') <?> "seqid"
  char '\t'
  _gff3Source <- takeWhile1 (/= '\t') <?> "source"
  char '\t'
  _gff3Type <- takeWhile1 (/= '\t') <?> "type"
  char '\t'
  _gff3Start <- decimal  <?> "start"
  char '\t'
  _gff3End <- decimal <?> "end"
  char '\t'
  _gff3Score <- takeWhile1 (/= '\t') <?> "score"
  char '\t'
  _gff3Strand <- (choice [char '+', char '-', char '.']) <?> "strand"
  char '\t'
  _gff3Phase <- takeWhile1 (/= '\t') <?> "phase"
  char '\t'
  _gff3Attributes <- genParseGFF3Attributes <?> "GFF3 attributes"
  endOfLine
  skipMany (try genParseGFF3Comment)
  return $ GFF3Entry (B.fromStrict _gff3Seqid) (B.fromStrict _gff3Source) (B.fromStrict _gff3Type) _gff3Start _gff3End (B.fromStrict _gff3Score) _gff3Strand (B.fromStrict _gff3Phase) (V.fromList _gff3Attributes)

genParseGFF3Attributes :: Parser [B.ByteString]
genParseGFF3Attributes = do
  _gff3AtributesString <- takeTill (\a -> a == '\n') <?> "attributes"
  let _gff3Atributes = map B.fromStrict (C.split ';' _gff3AtributesString)
  return $ _gff3Atributes

toLB :: C.ByteString -> B.ByteString
toLB = S.toLazyByteString . S.byteString
