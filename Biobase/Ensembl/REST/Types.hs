module Biobase.Ensembl.REST.Types (
                       EnsemblEntry(..)
                      ) where

import Data.Aeson
import GHC.Generics
import Data.Maybe
import qualified Data.Text as T

--"[{\"display_id\":\"00300+2.7.2.4+1.1.1.3\",\"primary_id\":\"00300+2.7.2.4+1.1.1.3\",\"version\":\"0\",\"description\":\"\",\"dbname\":\"KEGG_Enzyme\",\"synonyms\":[],\"info_text\":\"\",\"info_type\":\"NONE\",\"db_display_name\":\"KEGG Pathway and Enzyme\"}]""

data EnsemblEntry = EnsemblEntry
      { display_id :: Maybe T.Text
      , primary_id :: Maybe T.Text
      , version :: Maybe T.Text
      , description :: Maybe T.Text
      , dbname :: Maybe T.Text
      , synonyms :: [T.Text]
      , info_text :: Maybe T.Text
      , info_type :: Maybe T.Text
      , db_display_name :: Maybe T.Text
      } deriving (Show, Generic, ToJSON, FromJSON)
