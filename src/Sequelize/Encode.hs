-- | Sequelize.js-compatible JSON encoder.
module Sequelize.Encode
  ( modelEncodeWhere,
    encodeWhere,
    encodeClause,
    encodeTerm,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Kind ()
import Data.Text (Text)
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import Sequelize

-- | Like 'encodeWhere', but takes the entity descriptor from 'Model'.
modelEncodeWhere ::
  forall be table.
  Model be table =>
  Where be table ->
  Aeson.Object
modelEncodeWhere w = encodeWhere modelTableEntityDescriptor w

encodeWhere ::
  forall be table.
  B.Beamable table =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) ->
  Where be table ->
  Aeson.Object
encodeWhere dt = encodeClause dt . And

encodeClause ::
  forall be table.
  B.Beamable table =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) ->
  Clause be table ->
  Aeson.Object
encodeClause dt w =
  let foldWhere' = \case
        And cs -> foldAnd cs
        Or cs -> foldOr cs
        Is column val -> foldIs column val
      foldAnd = \case
        [] -> HM.empty
        [x] -> foldWhere' x
        xs
          | Just maps <- mapM fromIs xs -> mconcat maps
          | otherwise -> HM.singleton "$and" (Aeson.toJSON $ map foldWhere' xs)
      foldOr = \case
        [] -> HM.empty
        [x] -> foldWhere' x
        xs -> HM.singleton "$or" (Aeson.toJSON $ map foldWhere' xs)
      foldIs :: Aeson.ToJSON a => Column table value -> Term be a -> Aeson.Object
      foldIs column val =
        let key =
              B._fieldName . fromColumnar' . column . columnize $
                B.dbTableSettings dt
         in HM.singleton key $ encodeTerm val
      fromIs :: Clause be table -> Maybe Aeson.Object
      fromIs = \case
        Is column val -> Just (foldIs column val)
        _ -> Nothing
   in foldWhere' w

-- Warning: the behavior for @Not (Like _)@, @Not (In _)@, @Not (Eq _)@ is
-- the same as in Sequelize.js, but for all other 'Not's it isn't.
encodeTerm :: Aeson.ToJSON a => Term be a -> Aeson.Value
encodeTerm = \case
  -- Contains vals -> array Aeson.toJSON "$contains" vals
  -- Contained vals -> array Aeson.toJSON "$contained" vals
  -- Any vals -> array Aeson.toJSON "$any" vals
  -- Between vals -> array Aeson.toJSON "$between" vals
  -- NotBetween vals -> array Aeson.toJSON "$notBetween" vals
  -- Overlap vals -> array Aeson.toJSON "$overlap" vals
  -- ILike val -> single Aeson.toJSON "$iLike" val
  -- NotILike val -> single Aeson.toJSON "$notILike" val
  -- RegExp val -> single Aeson.toJSON "$regexp" val
  -- NotRegExp val -> single Aeson.toJSON "$notRegexp" val
  -- IRegExp val -> single Aeson.toJSON "$iRegexp" val
  -- NotIRegExp val -> single Aeson.toJSON "$notIRegexp" val
  -- Col val -> single Aeson.toJSON "$col" val
  -- Not val -> single Aeson.toJSON "$not" val
  In vals -> array Aeson.toJSON "$in" vals
  Eq val -> Aeson.toJSON val
  Null -> Aeson.Null
  GreaterThan val -> single Aeson.toJSON "$gt" val
  GreaterThanOrEq val -> single Aeson.toJSON "$gte" val
  LessThan val -> single Aeson.toJSON "$lt" val
  LessThanOrEq val -> single Aeson.toJSON "$lte" val
  Like val -> single Aeson.toJSON "$like" val
  Not (Like val) -> single Aeson.toJSON "$notLike" val
  Not (In vals) -> array Aeson.toJSON "$notIn" vals
  Not (Eq val) -> single Aeson.toJSON "$ne" val
  Not Null -> single id "$ne" Aeson.Null
  Not term -> single encodeTerm "$not" term

array :: (a -> Aeson.Value) -> Text -> [a] -> Aeson.Value
array f k vs = Aeson.toJSON $ HM.singleton k $ map f vs

single :: (a -> Aeson.Value) -> Text -> a -> Aeson.Value
single f k v = Aeson.toJSON $ HM.singleton k $ f v