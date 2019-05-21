{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Parses GTF

module Biobase.GTF.Import (gtfFromFile,
                             parseGTFs,
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
import Biobase.GTF.Types
import qualified Data.Word8 as W

-- | reads and parses GTF from provided filePath
gtfFromFile :: String -> IO [GTF]
gtfFromFile filePath = do
  printf "# reading GTF from file %s\n" filePath
  fileExists <- doesFileExist filePath
  if fileExists
     then parseGTFs <$> B.readFile filePath
     else fail "# GTF file \"%s\" does not exist\n" filePath

-- | Read a lazy bytestring and stream out a list of @GTFs@'s.
-- In case, there is a parse error "late" in the file, we might have
-- already streamed out some (or many!) of these results.

parseGTFs :: B.ByteString -> [GTF]
parseGTFs = go
  where go xs = case L.parse genParseGTF xs of
          L.Fail remainingInput ctxts err  -> error $ "parseGTFs failed! " ++ err ++ " ctxt: " ++ show ctxts ++ " head of remaining input: " ++ B.unpack (B.take 1000 remainingInput)
          L.Done remainingInput btr
            | B.null remainingInput  -> [btr]
            | otherwise              -> btr : go remainingInput

genParseGTF :: Parser GTF
genParseGTF = do
  skipMany (try genParseGTFComment)
  _entry <- many1 (try genParseGTFEntry) <?> "GTF entry"
  return $ GTF (V.fromList _entry) B.empty

genParseGTFComment :: Parser String
genParseGTFComment = do
  string "#"
  takeWhile1 (/= '\n')
  endOfLine
  return $ ""

genParseGTFEntry :: Parser GTFEntry
genParseGTFEntry = do
  _gtfSeqid <- takeWhile1 (/= '\t') <?> "seqid"
  char '\t'
  _gtfSource <- takeWhile1 (/= '\t') <?> "source"
  char '\t'
  _gtfType <- takeWhile1 (/= '\t') <?> "type"
  char '\t'
  _gtfStart <- decimal  <?> "start"
  char '\t'
  _gtfEnd <- decimal <?> "end"
  char '\t'
  _gtfScore <- takeWhile1 (/= '\t') <?> "score"
  char '\t'
  _gtfStrand <- (choice [char '+', char '-', char '.']) <?> "strand"
  char '\t'
  _gtfPhase <- takeWhile1 (/= '\t') <?> "phase"
  char '\t'
  _gtfAttributes <- genParseGTFAttributes <?> "GTF attributes"
  endOfLine
  skipMany (try genParseGTFComment)
  return $ GTFEntry (B.fromStrict _gtfSeqid) (B.fromStrict _gtfSource) (B.fromStrict _gtfType) _gtfStart _gtfEnd (B.fromStrict _gtfScore) _gtfStrand (B.fromStrict _gtfPhase) (V.fromList _gtfAttributes)

genParseGTFAttributes :: Parser [B.ByteString]
genParseGTFAttributes = do
  _gtfAtributesString <- takeTill (\a -> a == '\n') <?> "attributes"
  let _gtfAtributes = map B.fromStrict (C.split ';' _gtfAtributesString)
  return $ _gtfAtributes

toLB :: C.ByteString -> B.ByteString
toLB = S.toLazyByteString . S.byteString
