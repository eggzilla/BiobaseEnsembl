module Biobase.Ensembl.REST.Types (
                       EnsemblEntry(..)
                      ) where

import Data.Aeson
import GHC.Generics
import Data.Maybe

--"[{\"display_id\":\"00300+2.7.2.4+1.1.1.3\",\"primary_id\":\"00300+2.7.2.4+1.1.1.3\",\"version\":\"0\",\"description\":\"\",\"dbname\":\"KEGG_Enzyme\",\"synonyms\":[],\"info_text\":\"\",\"info_type\":\"NONE\",\"db_display_name\":\"KEGG Pathway and Enzyme\"}]""

data EnsemblEntry = EnsemblEntry
      { display_id :: Maybe String
      , primary_id :: Maybe String
      , version :: Maybe String
      , description :: Maybe String
      , dbname :: Maybe String
      , synonyms :: [String]
      , info_text :: Maybe String
      , info_type :: Maybe String
      , db_display_name :: Maybe String
      } deriving (Show, Generic, ToJSON, FromJSON)
