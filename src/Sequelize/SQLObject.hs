{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Sequelize.SQLObject where

import qualified Data.Aeson as A
import Data.Coerce (coerce)
import qualified Data.Vector as V
import Database.Beam.Backend
import qualified Database.Beam.Backend.SQL.AST as B
import Data.Text
import Data.Aeson
import qualified Data.Text as T

data SQLObject a = SQLObjectValue Text | SQLObjectList [SQLObject a]

instance ToJSON (SQLObject a) where
  toJSON (SQLObjectValue a) = A.String a
  toJSON (SQLObjectList as) = A.Array (V.fromList $ toJSON <$> as)

class ToSQLObject a where
  convertToSQLObject :: a -> SQLObject a

instance HasSqlValueSyntax B.Value A.Value where
  sqlValueSyntax = autoSqlValueSyntax

instance HasSqlValueSyntax B.Value a => ToSQLObject a where
  convertToSQLObject = SQLObjectValue . valueToText . sqlValueSyntax @B.Value

-- FIXME remove overlapping if possible
instance {-# OVERLAPPING #-} ToSQLObject a => ToSQLObject [a] where
  convertToSQLObject v = do
    let sqlObjectsList = convertToSQLObject <$> v
    SQLObjectList $ coerce @(SQLObject a) @(SQLObject [a]) <$> sqlObjectsList

instance {-# OVERLAPPING #-} ToSQLObject a => ToSQLObject (Maybe a) where
  convertToSQLObject mbA = case mbA of
    Just a -> coerce @(SQLObject a) @(SQLObject (Maybe a)) $ convertToSQLObject a
    Nothing -> coerce @(SQLObject SqlNull) @(SQLObject (Maybe a)) $ convertToSQLObject SqlNull

valueToText :: B.Value -> Text
valueToText (B.Value v) = T.pack $ show v
