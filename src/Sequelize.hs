{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Sequelize
  ( -- * Types
    Column,
    Model,
    ModelMeta (..),

    -- * Statements
    sqlSelect,
    sqlSelectQ,
    sqlCount,
    sqlCountQ,
    sqlSelect',
    sqlDelete,
    sqlDeleteLimit,
    sqlUpdate,
    sqlUpdate',
    TableType(..),
    -- * WHERE
    Where,
    Clause (..),
    Term (..),
    WHERE,
    IS(..),
    isClausesToWhere,

    -- * ORDER BY
    OrderBy (..),

    -- * SET
    Set (..),

    -- * Internals
    whereQ,
    clauseQ,
    termQ,
    setQ,
    orderByQ,
    HasTableField (..),
    GModelToSets (..),
    modelToSets,
    ModelToSets,
    modelTableEntity,
    modelTableEntityDescriptor,
    DatabaseWith (..),
    columnize,
    fromColumnar',
    retypeQOrd,
    EqValue (..)
  )
where

import Control.Monad (join)
import Data.Aeson (ToJSON)
import Data.Functor.Identity (Identity)
import qualified Data.Generics.Product.Fields as L
import Data.Kind ()
import Data.Monoid (appEndo)
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Query.Internal as B
import qualified Database.Beam.Schema.Tables as B
import GHC.Generics (Generic)
import qualified GHC.Generics as G
import GHC.TypeLits (Symbol)
import Named ((:!), (:?), arg, argF)
import qualified Data.Maybe as DM
import Streamly.Data.MutByteArray (Serialize)

----------------------------------------------------------------------------
-- Column
----------------------------------------------------------------------------

-- | A table column identifier. Any field accessor is a 'Column'.
--
-- Sample: @lastUpdated :: Column UserT LocalTime@
--
-- 'Column's can be written in any way as long as they are field accessors.
-- E.g. you can write @Is (^. #email)@ instead of @Is email@.
type Column table value = forall f. table (B.Columnar' f) -> B.Columnar' f value

----------------------------------------------------------------------------
-- Where
----------------------------------------------------------------------------

-- | Sample sig: @Where MySQL UserT@
type Where be table = [Clause be table]

type WHERE be table = [IS be table]

isClausesToWhere :: WHERE be table -> Where be table
isClausesToWhere = fmap (\(IS c v) -> Is c (Eq v))

data IS be table where
  IS :: (Serialize value, ToJSON value, Ord value, EqValue be value, Show value) => Column table value -> value -> IS be table

data Clause be (table :: (* -> *) -> *) where
  And :: [Clause be table] -> Clause be table
  Or :: [Clause be table] -> Clause be table
  Is ::
    (Serialize value, ToJSON value, Ord value, Show value) =>
    Column table value ->
    Term be value ->
    Clause be table

-- sequelize.js operations that were not ported:
--
-- Contains :: [a] -> Term be a
-- Contained :: [a] -> Term be a
-- Any :: [a] -> Term be a
-- Between :: [Int] -> Term be Int (nb: not actually [Int])
-- NotBetween :: [Int] -> Term be Int
-- Overlap :: [Int] -> Term be Int
-- ILike :: Text -> Term be Text
-- NotILike :: Text -> Term be Text
-- RegExp :: Text -> Term be Text
-- NotRegExp :: Text -> Term be Text
-- IRegExp :: Text -> Term be Text
-- NotIRegExp :: Text -> Term be Text
-- Col :: Text -> Term be Text

data Term be a where
  In :: B.BeamSqlBackendCanSerialize be a => [a] -> Term be a
  Eq :: EqValue be a => a -> Term be a
  Null :: Term be (Maybe a)
  GreaterThan :: B.BeamSqlBackendCanSerialize be a => a -> Term be a
  GreaterThanOrEq :: B.BeamSqlBackendCanSerialize be a => a -> Term be a
  LessThan :: B.BeamSqlBackendCanSerialize be a => a -> Term be a
  LessThanOrEq :: B.BeamSqlBackendCanSerialize be a => a -> Term be a
  Like :: (B.BeamSqlBackendCanSerialize be Text, B.BeamSqlBackendIsString be Text) => Text -> Term be Text
  Not :: Term be a -> Term be a

whereQ ::
  forall be table s.
  (B.BeamSqlBackend be, B.Beamable table) =>
  Where be table ->
  (table (B.QExpr be s) -> B.QExpr be s B.SqlBool)
whereQ = clauseQ . And

clauseQ ::
  forall be table s.
  (B.BeamSqlBackend be, B.Beamable table) =>
  Clause be table ->
  (table (B.QExpr be s) -> B.QExpr be s B.SqlBool)
clauseQ p = \item -> case p of
  And [] -> B.sqlBool_ (B.val_ True)
  And xs -> foldr1 (B.&&?.) (map (flip clauseQ item) xs)
  Or [] -> B.sqlBool_ (B.val_ False)
  Or xs -> foldr1 (B.||?.) (map (flip clauseQ item) xs)
  Is column' term ->
    let column = fromColumnar' . column' . columnize
     in termQ (column item) term

termQ ::
  B.BeamSqlBackend be =>
  B.QExpr be s a ->
  Term be a ->
  B.QExpr be s B.SqlBool
termQ val = \case
  In lits -> B.sqlBool_ (val `B.in_` map B.val_ lits)
  Null -> B.sqlBool_ (B.isNothing_ val)
  Eq lit -> eqValue val lit
  GreaterThan lit -> B.sqlBool_ (val B.>. B.val_ lit)
  GreaterThanOrEq lit -> B.sqlBool_ (val B.>=. B.val_ lit)
  LessThan lit -> B.sqlBool_ (val B.<. B.val_ lit)
  LessThanOrEq lit -> B.sqlBool_ (val B.<=. B.val_ lit)
  Like s -> B.sqlBool_ (val `B.like_` B.val_ s)
  -- Nots
  Not Null -> B.sqlBool_ (B.isJust_ val)
  Not (Eq lit) -> neqValue val lit
  Not t -> B.sqlNot_ (termQ val t)

-- Needed because comparisons with Maybe are tricky
class EqValue be a where
  eqValue :: B.QExpr be s a -> a -> B.QExpr be s B.SqlBool
  neqValue :: B.QExpr be s a -> a -> B.QExpr be s B.SqlBool

instance
  (B.BeamSqlBackendCanSerialize be a, B.HasSqlEqualityCheck be a) =>
  EqValue be a
  where
  eqValue expr x = expr B.==?. B.val_ x
  neqValue expr x = expr B./=?. B.val_ x

instance
  {-# OVERLAPPING #-}
  (B.BeamSqlBackendCanSerialize be (Maybe a), B.HasSqlEqualityCheck be (Maybe a)) =>
  EqValue be (Maybe a)
  where
  eqValue expr = \case
    Nothing -> B.sqlBool_ (B.isNothing_ expr)
    Just x -> expr B.==?. B.val_ (Just x)
  neqValue expr = \case
    Nothing -> B.sqlBool_ (B.isJust_ expr)
    Just x -> expr B./=?. B.val_ (Just x)

----------------------------------------------------------------------------
-- Ordering
----------------------------------------------------------------------------

data OrderBy table
  = forall value. (Ord value) => Asc (Column table value)
  | forall value. (Ord value) => Desc (Column table value)

orderByQ ::
  (B.BeamSqlBackend be, B.Beamable table) =>
  OrderBy table ->
  table (B.QExpr be s) ->
  B.QOrd be s ()
orderByQ (Asc column) item =
  retypeQOrd $ B.asc_ (fromColumnar' . column . columnize $ item)
orderByQ (Desc column) item =
  retypeQOrd $ B.desc_ (fromColumnar' . column . columnize $ item)

----------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------

data Set be table
  = forall value.
    (B.BeamSqlBackendCanSerialize be value, ToJSON value) =>
    Set (Column table value) value
  | forall value.
    SetDefault (Column table value)

setQ ::
  (B.Beamable table, B.BeamSqlBackend be) =>
  table (B.QField s) ->
  Set be table ->
  B.QAssignment be s
setQ item = \case
  Set column' value ->
    let column = fromColumnar' . column' . columnize
     in column item B.<-. B.val_ value
  SetDefault column' ->
    let column = fromColumnar' . column' . columnize
     in column item B.<-. B.default_

----------------------------------------------------------------------------
-- Data to assignment
----------------------------------------------------------------------------

class HasTableField (name :: Symbol) table value where
  getTableField :: Column table value

-- TODO: can avoid generic-lens here
instance
  (forall f. L.HasField' name (table (B.Columnar' f)) (B.Columnar' f a)) =>
  HasTableField name table a
  where
  getTableField = L.getField @name

class GModelToSets be (table :: (* -> *) -> *) g where
  gModelToSets :: g x -> [Set be table]

instance
  GModelToSets be table g =>
  GModelToSets be table (G.D1 c g)
  where
  gModelToSets (G.M1 g) = gModelToSets g

instance
  GModelToSets be table g =>
  GModelToSets be table (G.C1 c g)
  where
  gModelToSets (G.M1 g) = gModelToSets g

instance
  (GModelToSets be table g1, GModelToSets be table g2) =>
  GModelToSets be table (g1 G.:*: g2)
  where
  gModelToSets (g1 G.:*: g2) = gModelToSets g1 ++ gModelToSets g2

-- If we have e.g. "id PRIMARY KEY AUTO_INCREMENT", we don't want to insert
-- a NULL into the table - we want to insert a default. Unfortunately, we
-- can't distinguish between NULL and DEFAULT at the model level.
--
-- So we just insert DEFAULT for all Nothings - in most cases it will be the
-- same thing, and when not, the column's default value is a better choice.
instance
  {-# OVERLAPPING #-}
  ( c ~ 'G.MetaSel ('Just name) _u _s _d,
    HasTableField name table (Maybe value),
    B.BeamSqlBackendCanSerialize be (Maybe value),
    ToJSON value
  ) =>
  GModelToSets be table (G.S1 c (G.Rec0 (Maybe value)))
  where
  gModelToSets (G.M1 (G.K1 mbValue)) =
    case mbValue of
      Nothing ->
        [SetDefault (getTableField @name @table @(Maybe value))]
      Just value ->
        [Set (getTableField @name @table @(Maybe value)) (Just value)]

instance
  {-# OVERLAPPABLE #-}
  ( c ~ 'G.MetaSel ('Just name) _u _s _d,
    HasTableField name table value,
    B.BeamSqlBackendCanSerialize be value,
    ToJSON value
  ) =>
  GModelToSets be table (G.S1 c (G.Rec0 value))
  where
  gModelToSets (G.M1 (G.K1 value)) =
    [Set (getTableField @name @table @value) value]

-- | Use this if you want to assign all fields.
modelToSets :: ModelToSets be table => table Identity -> [Set be table]
modelToSets = gModelToSets . G.from

-- | Use this constraint to indicate that a model supports 'modelToSets'.
type ModelToSets be table =
  ( Typeable table,
    B.Beamable table,
    Generic (table Identity),
    GModelToSets be table (G.Rep (table Identity))
  )

----------------------------------------------------------------------------
-- Options to Beam
----------------------------------------------------------------------------

applyLimit ::
  (B.Beamable table) =>
  Maybe Int ->
  (forall s. B.Q be db s (table (B.QExpr be s))) ->
  (forall s. B.Q be db s (table (B.QExpr be s)))
applyLimit mbLimit_ x = case mbLimit_ of
  Nothing -> x
  Just n -> B.limit_ (toInteger n) x

applyIndexHints ::
  (B.BeamSqlBackend be, B.Beamable table) =>
  Maybe Text ->
  (forall s. B.Q be db s (table (B.QExpr be s))) ->
  (forall s. B.Q be db s (table (B.QExpr be s)))
applyIndexHints mbLimit_ x = case mbLimit_ of
  Nothing -> x
  Just n -> B.forceIndex_ n x

applyOffset ::
  (B.Beamable table) =>
  Maybe Int ->
  (forall s. B.Q be db s (table (B.QExpr be s))) ->
  (forall s. B.Q be db s (table (B.QExpr be s)))
applyOffset mbOffset_ x = case mbOffset_ of
  Nothing -> x
  Just n -> B.offset_ (toInteger n) x

applyOrderBy ::
  (B.BeamSqlBackend be, B.Beamable table) =>
  Maybe [OrderBy table] ->
  (forall s. B.Q be db s (table (B.QExpr be s))) ->
  (forall s. B.Q be db s (table (B.QExpr be s)))
applyOrderBy mbOrderBy_ x = case mbOrderBy_ of
  Nothing -> x
  Just ords -> B.orderBy_ (mapM orderByQ ords) x

applyWhere :: (B.BeamSqlBackend be, B.Beamable table) 
           => Maybe (Where be table) 
           -> (forall s. B.Q be db s (table (B.QExpr be s)))
           -> B.Q be db s (table (B.QExpr be s))
applyWhere mbWhere_ q = maybe q (\w -> B.filter_' (whereQ w) q) mbWhere_
----------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------

class ModelMeta table where
  modelFieldModification :: table (B.FieldModification (B.TableField table))
  modelTableName :: Text
  modelTableType :: Maybe TableType
  modelTableType = Nothing
  modelSchemaName :: Maybe Text
  modelSchemaName = Nothing
  mkExprWithDefault :: forall be s.
    (B.BeamSqlBackend be, B.Beamable table,
     B.FieldsFulfillConstraint (B.BeamSqlBackendCanSerialize be) table) =>
    table Identity ->
    B.SqlInsertValues be (table (B.QExpr be s))
  mkExprWithDefault t = B.insertExpressions ( [B.val_ t] :: forall s'. [table (B.QExpr be s')])
  mkMultiExprWithDefault :: forall be s.
    (B.BeamSqlBackend be, B.Beamable table,
     B.FieldsFulfillConstraint (B.BeamSqlBackendCanSerialize be) table) =>
    [table Identity] ->
    B.SqlInsertValues be (table (B.QExpr be s))
  mkMultiExprWithDefault t = B.insertExpressions ( B.val_ <$> t :: forall s'. [table (B.QExpr be s')])
  mbSetAutoIncrementPrimaryId :: Maybe (Integer -> table Identity -> table Identity)
  mbSetAutoIncrementPrimaryId = Nothing

type Model be table =
  ( B.BeamSqlBackend be,
    Typeable table,
    Generic (table Identity),
    Generic (table B.Exposed),
    B.FieldsFulfillConstraint (B.BeamSqlBackendCanSerialize be) table,
    B.FromBackendRow be (table Identity),
    B.DatabaseEntityDefaultRequirements be (B.TableEntity table),
    B.Beamable table,
    ModelMeta table
  )

modelTableEntity ::
  forall table be db.
  Model be table =>
  Text ->
  B.DatabaseEntity be db (B.TableEntity table)
modelTableEntity schemaName =
  let B.EntityModification modification =
        B.modifyTableFields (modelFieldModification @table)
          <> B.setEntityName (modelTableName @table)
          <> B.setEntitySchema (Just schemaName)
   in appEndo modification $ B.DatabaseEntity $ B.dbEntityAuto ( modelTableName @table)

modelTableEntityDescriptor ::
  forall table be.
  Model be table =>
  Text ->
  B.DatabaseEntityDescriptor be (B.TableEntity table)
modelTableEntityDescriptor schemaName = let B.DatabaseEntity x = modelTableEntity @table schemaName in x

----------------------------------------------------------------------------
-- End-to-end
----------------------------------------------------------------------------

-- | You can use 'DatabaseWith' for operations like 'all_' that demand a
-- 'Database' instance but don't actually do anything with the instance.
data DatabaseWith table f = DatabaseWith
  { dwTable :: f (B.TableEntity table)
  }
  deriving (Generic, B.Database be)

sqlSelect ::
  forall be table.
  (B.HasQBuilder be, Model be table) =>
  -- Note: using 'where_' instead of 'where' because #where messes up indentation in Emacs
  "where_" :? Where be table ->
  "orderBy" :? [OrderBy table] ->
  "offset" :? Int ->
  "limit" :? Int ->
  "forceIndex" :? Text ->
  Text ->
  B.SqlSelect be (table Identity)
sqlSelect argWhere argOrder argOffset argLimit argForceIndex schemaName =
  B.select (sqlSelectQ @(DatabaseWith table) argWhere argOrder argOffset argLimit argForceIndex schemaName)

sqlCount ::
  forall be table.
  (B.HasQBuilder be, Model be table) =>
  -- Note: using 'where_' instead of 'where' because #where messes up indentation in Emacs
  "where_" :? Where be table ->
  "orderBy" :? [OrderBy table] ->
  "offset" :? Int ->
  "limit" :? Int ->
  "forceIndex" :? Text ->
  Text ->
  B.SqlSelect be Int
sqlCount argWhere argOrder argOffset argLimit argForceIndex schemaName =
  B.select (sqlCountQ @(DatabaseWith table) argWhere argOrder argOffset argLimit argForceIndex schemaName)

sqlCountQ ::
  forall db be table.
  (B.Database be db, B.HasQBuilder be, Model be table) =>
  "where_" :? Where be table ->
  "orderBy" :? [OrderBy table] ->
  "offset" :? Int ->
  "limit" :? Int ->
  "forceIndex" :? Text ->
  Text ->
  (forall s. B.Q be db s (B.WithRewrittenThread
                          (B.QNested s) s
                          (B.WithRewrittenContext
                             (B.QGenExpr B.QAggregateContext be (B.QNested s) Int)
                             B.QValueContext)))
sqlCountQ
  (argF #where_ -> mbWhere_)
  (argF #orderBy -> mbOrderBy_)
  (argF #offset -> mbOffset_)
  (argF #limit -> mbLimit_) 
  (argF #forceIndex -> mbForceIndex_ )
  schemaName =
    B.aggregate_ (\_ -> B.as_ @Int B.countAll_)
      $ applyLimit mbLimit_
      $ applyOffset mbOffset_
      $ applyOrderBy mbOrderBy_
      $ applyWhere mbWhere_
      $ applyIndexHints mbForceIndex_
      $ B.all_ (modelTableEntity @table @be @db schemaName)

-- | Like 'sqlSelect', but can be used as a part of a bigger SELECT.
sqlSelectQ ::
  forall db be table.
  (B.Database be db, B.HasQBuilder be, Model be table) =>
  "where_" :? Where be table ->
  "orderBy" :? [OrderBy table] ->
  "offset" :? Int ->
  "limit" :? Int ->
  "forceIndex" :? Text ->
  Text ->
  (forall s. B.Q be db s (table (B.QExpr be s)))
sqlSelectQ
  (argF #where_ -> mbWhere_)
  (argF #orderBy -> mbOrderBy_)
  (argF #offset -> mbOffset_)
  (argF #limit -> mbLimit_)
  (argF #forceIndex -> forceIndex_)
  schemaName =
    applyLimit mbLimit_
      $ applyOffset mbOffset_
      $ applyOrderBy mbOrderBy_
      $ applyWhere mbWhere_
      $ applyIndexHints forceIndex_
      $ B.all_ (modelTableEntity @table @be @db schemaName)

sqlSelect' ::
  forall be table.
  (B.HasQBuilder be, Model be table) =>
  "where_" :? Where be table ->
  "orderBy" :? Maybe [OrderBy table] ->
  "offset" :? Maybe Int ->
  "limit" :? Maybe Int ->
  "forceIndex" :? Maybe Text ->
  Text ->
  B.SqlSelect be (table Identity)
sqlSelect' argWhere argOrder argOffset argLimit argForceIndex schemaName =
  B.select (sqlSelectQ' @(DatabaseWith table) argWhere argOrder argOffset argLimit argForceIndex schemaName)

sqlSelectQ' ::
  forall db be table.
  (B.Database be db, B.HasQBuilder be, Model be table) =>
  "where_" :? Where be table ->
  "orderBy" :? Maybe [OrderBy table] ->
  "offset" :? Maybe Int ->
  "limit" :? Maybe Int ->
  "forceIndex" :? Maybe Text ->
  Text ->
  (forall s. B.Q be db s (table (B.QExpr be s)))
sqlSelectQ'
  (argF #where_ -> mbWhere_)
  (argF #orderBy -> mbOrderBy_)
  (argF #offset -> mbOffset_)
  (argF #limit -> mbLimit_)
  (argF #forceIndex -> forceIndex_)
  schemaName =
    applyLimit (join mbLimit_)
      $ applyOffset (join mbOffset_)
      $ applyOrderBy (join mbOrderBy_)
      $ applyWhere mbWhere_
      $ applyIndexHints (join forceIndex_)
      $ B.all_ (modelTableEntity @table @be @db schemaName)

sqlDelete ::
  forall be table.
  (B.HasQBuilder be, Model be table) =>
  "where_" :? Where be table ->
  Text ->
  B.SqlDelete be table
sqlDelete
  (argF #where_ -> mbWhere_)
  schemaName =
    B.delete'
      (modelTableEntity schemaName)
      (\item -> maybe (B.sqlBool_ (B.val_ True)) (flip whereQ item) mbWhere_)

sqlDeleteLimit ::
  forall be table.
  (B.HasQBuilder be, Model be table) =>
  "where_" :? Where be table ->
  "limit_" :? Maybe Int ->
  Text ->
  B.SqlDelete be table
sqlDeleteLimit
  (argF #where_ -> mbWhere_)
  (argF #limit_ -> mbLimit_)
  schemaName =
    B.deleteWLimit
      (DM.fromMaybe Nothing mbLimit_)
      (modelTableEntity schemaName)
      (\item -> maybe (B.sqlBool_ (B.val_ True)) (flip whereQ item) mbWhere_)

sqlUpdate ::
  forall be table.
  (B.HasQBuilder be, Model be table) =>
  "set" :! [Set be table] ->
  "where_" :? Where be table ->
  Text ->
  B.SqlUpdate be table
sqlUpdate
  (arg #set -> set_)
  (argF #where_ -> mbWhere_)
  schemaName =
    B.update'
      (modelTableEntity schemaName)
      (\item -> mconcat $ map (setQ item) set_)
      (\item -> maybe (B.sqlBool_ (B.val_ True)) (flip whereQ item) mbWhere_)

sqlUpdate' ::
  forall be table.
  (B.HasQBuilder be, Model be table, ModelToSets be table) =>
  "save" :! table Identity ->
  "where_" :? Where be table ->
  Text ->
  B.SqlUpdate be table
sqlUpdate'
  (arg #save -> save_)
  (argF #where_ -> mbWhere_)
  schemaName =
    B.update'
      (modelTableEntity schemaName)
      (\item -> mconcat $ map (setQ item) (modelToSets save_))
      (\item -> maybe (B.sqlBool_ (B.val_ True)) (flip whereQ item) mbWhere_)

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- TODO: this is probably expensive, and it doesn't have to be because table
-- f and table (Columnar' f) should have the same representation. Ideally
-- 'Beamable' should provide a way to columnize a table without doing any
-- work.
columnize :: B.Beamable table => table f -> table (B.Columnar' f)
columnize = B.changeBeamRep B.Columnar'

fromColumnar' :: B.Columnar' f value -> B.Columnar f value
fromColumnar' (B.Columnar' x) = x

retypeQOrd :: B.QOrd be s a -> B.QOrd be s b
retypeQOrd (B.QOrd x) = B.QOrd x

data TableType = DOMAIN | CONFIG | COMMON_CONFIG | TRACKER | COMMON_TRACKER
  deriving (Show, Eq)
