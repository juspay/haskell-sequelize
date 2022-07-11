{-# LANGUAGE DeriveAnyClass #-}

module Main where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Identity (Identity)
import Data.Kind ()
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Database.Beam as B
import qualified Database.Beam.MySQL as BM
import Database.Beam.MySQL (MySQL)
import GHC.Generics (Generic)
import Named ((!), defaults)
import Sequelize
import Test.Tasty
import Test.Tasty.HUnit

----------------------------------------------------------------------------
-- Setup
----------------------------------------------------------------------------

data TestDb f = TestDb
  { testTable :: f (B.TableEntity TestT)
  }
  deriving (Generic, B.Database be)

testDb :: B.DatabaseSettings be TestDb
testDb = B.defaultDbSettings

data TestT f = Test
  { email :: B.Columnar f Text,
    enabled :: B.Columnar (B.Nullable f) Bool
  }
  deriving (Generic)

type Test = TestT Identity

instance B.Beamable TestT

instance B.Table TestT where
  data PrimaryKey TestT f = TestId (B.Columnar f Text) deriving (Generic, B.Beamable)
  primaryKey = TestId . email

instance ModelMeta TestT where
  modelFieldModification = B.tableModification
  modelTableName = "test"

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

renderSelect :: (B.SqlSelect MySQL a) -> Text
renderSelect (B.SqlSelect a) =
  T.decodeUtf8
    $ BSL.toStrict
    $ Data.ByteString.Builder.toLazyByteString
    $ BM.unwrapInnerBuilder (BM.fromMysqlSelect a)

renderUpdate :: (B.SqlUpdate MySQL a) -> Text
renderUpdate (B.SqlUpdate _ a) =
  T.decodeUtf8
    $ BSL.toStrict
    $ Data.ByteString.Builder.toLazyByteString
    $ BM.unwrapInnerBuilder (BM.fromMysqlUpdate a)
renderUpdate _ = error "not implemented"

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ testCase "simple SELECT" unit_select_simple,
        testCase "Eq (x :: Maybe _)" unit_select_eq_maybe,
        testCase "full SELECT" unit_select_full,
        testCase "simple UPDATE" unit_update_simple,
        testCase "simple UPDATE setting all columns" unit_update'_simple,
        testCase "modelToSets" unit_modelToSets
      ]

unit_select_simple :: IO ()
unit_select_simple =
  renderSelect
    ( sqlSelect
        ! #where_ [Is enabled Null]
        ! defaults
    )
    @?= "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` \
        \WHERE (`t0`.`enabled`) IS NULL"

unit_select_eq_maybe :: IO ()
unit_select_eq_maybe = do
  renderSelect (sqlSelect ! #where_ [Is enabled (Eq Nothing)] ! defaults)
    @?= "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` WHERE (`t0`.`enabled`) IS NULL"
  renderSelect (sqlSelect ! #where_ [Is enabled (Eq (Just True))] ! defaults)
    @?= "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` WHERE (`t0`.`enabled`) = (TRUE)"
  renderSelect (sqlSelect ! #where_ [Is enabled (Not (Eq Nothing))] ! defaults)
    @?= "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` WHERE (`t0`.`enabled`) IS NOT NULL"
  renderSelect (sqlSelect ! #where_ [Is enabled (Not (Eq (Just True)))] ! defaults)
    @?= "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` WHERE (`t0`.`enabled`) <> (TRUE)"

unit_select_full :: IO ()
unit_select_full =
  renderSelect
    ( sqlSelect
        ! #where_
          [ Is enabled (Not Null),
            Or [Is email (Eq "a@example.com"), Is email (Eq "b@example.com")]
          ]
        ! #orderBy [Asc enabled, Desc email]
        ! #offset 8
        ! #limit 10
    )
    @?= "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` \
        \WHERE ((`t0`.`enabled`) IS NOT NULL) \
        \AND (((`t0`.`email`) = ('a@example.com')) OR ((`t0`.`email`) = ('b@example.com'))) \
        \ORDER BY `t0`.`enabled` ASC, `t0`.`email` DESC \
        \LIMIT 8, 10"

unit_update_simple :: IO ()
unit_update_simple =
  renderUpdate
    ( sqlUpdate
        ! #set [Set enabled Nothing]
        ! #where_ [Is enabled (Not Null)]
    )
    @?= "UPDATE `test` \
        \SET `enabled`=NULL \
        \WHERE (`enabled`) IS NOT NULL"

unit_update'_simple :: IO ()
unit_update'_simple =
  renderUpdate
    ( sqlUpdate'
        ! #save (Test "a@example.com" (Just False))
        ! #where_ [Is email (Eq "a@example.com")]
    )
    @?= "UPDATE `test` \
        \SET `email`='a@example.com', `enabled`=FALSE \
        \WHERE (`email`) = ('a@example.com')"

unit_modelToSets :: IO ()
unit_modelToSets = do
  renderUpdate
    ( sqlUpdate'
        ! #save (Test "a@example.com" Nothing)
        ! #where_ [Is email (Eq "a@example.com")]
    )
    @?= "UPDATE `test` \
        \SET `email`='a@example.com', `enabled`=DEFAULT \
        \WHERE (`email`) = ('a@example.com')"
