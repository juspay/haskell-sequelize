-- {-# OPTIONS_GHC -Wno-orphans #-}
-- {-# LANGUAGE IncoherentInstances #-}

module Sequelize.SQLObject where

import qualified Data.Aeson as A
-- import Data.Coerce (coerce)
import qualified Data.Vector as V
-- import Database.Beam.Backend
-- import qualified Database.Beam.Backend.SQL.AST as B
import Data.Text
import Data.Aeson

data SQLObject a = SQLObjectValue Text | SQLObjectList [SQLObject a]

instance ToJSON (SQLObject a) where
  toJSON (SQLObjectValue a) = A.String a
  toJSON (SQLObjectList as) = A.Array (V.fromList $ toJSON <$> as)

class ToSQLObject a where
  convertToSQLObject :: a -> SQLObject a

-- instance HasSqlValueSyntax B.Value A.Value where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance HasSqlValueSyntax B.Value a => ToSQLObject a where
--   convertToSQLObject = SQLObjectValue . valueToText . sqlValueSyntax @B.Value

-- -- FIXME remove overlapping if possible
-- instance {-# OVERLAPPING #-} ToSQLObject a => ToSQLObject [a] where
--   convertToSQLObject v = do
--     let sqlObjectsList = convertToSQLObject <$> v
--     -- SQLObjectList $ coerce @(SQLObject a) @(SQLObject [a]) <$> sqlObjectsList
--     SQLObjectList $ coerce <$> sqlObjectsList

-- instance {-# OVERLAPPING #-} ToSQLObject a => ToSQLObject (Maybe a) where
--   convertToSQLObject mbA = case mbA of
--     -- Just a -> coerce @(SQLObject a) @(SQLObject (Maybe a)) $ convertToSQLObject a
--     -- Nothing -> coerce @(SQLObject SqlNull) @(SQLObject (Maybe a)) $ convertToSQLObject SqlNull
--     Just a -> coerce $ convertToSQLObject a
--     Nothing -> coerce $ convertToSQLObject SqlNull

-- valueToText :: B.Value -> Text
-- valueToText (B.Value v) = show v
