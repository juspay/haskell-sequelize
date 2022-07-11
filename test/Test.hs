{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Functor.Identity (Identity)
import Data.Kind ()
import Data.Text (Text)
import qualified Database.Beam as B
import GHC.Generics (Generic)
import Named ((!), defaults)
import Sequelize
import Test.Tasty
import Test.Tasty.HUnit
import Database.Beam.MySQL.Extra


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
  dumpSelectSQL
    (sqlSelect
        ! #where_ [Is enabled Null]
        ! defaults
    )
    @?= Just "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` \
        \WHERE (`t0`.`enabled`) IS NULL;"

unit_select_eq_maybe :: IO ()
unit_select_eq_maybe = do
  dumpSelectSQL (sqlSelect ! #where_ [Is enabled (Eq Nothing)] ! defaults)
    @?= Just "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` WHERE (`t0`.`enabled`) IS NULL;"
  dumpSelectSQL (sqlSelect ! #where_ [Is enabled (Eq (Just True))] ! defaults)
    @?= Just "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` WHERE (`t0`.`enabled`) = (TRUE);"
  dumpSelectSQL (sqlSelect ! #where_ [Is enabled (Not (Eq Nothing))] ! defaults)
    @?= Just "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` WHERE (`t0`.`enabled`) IS NOT NULL;"
  dumpSelectSQL (sqlSelect ! #where_ [Is enabled (Not (Eq (Just True)))] ! defaults)
    @?= Just "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` WHERE (`t0`.`enabled`) <> (TRUE);"

unit_select_full :: IO ()
unit_select_full =
  dumpSelectSQL
    ( sqlSelect
        ! #where_
          [ Is enabled (Not Null),
            Or [Is email (Eq "a@example.com"), Is email (Eq "b@example.com")]
          ]
        ! #orderBy [Asc enabled, Desc email]
        ! #offset 8
        ! #limit 10
    )
    @?= Just "SELECT `t0`.`email` AS `res0`, `t0`.`enabled` AS `res1` \
        \FROM `test` AS `t0` \
        \WHERE ((`t0`.`enabled`) IS NOT NULL) \
        \AND (((`t0`.`email`) = ('a@example.com')) OR ((`t0`.`email`) = ('b@example.com'))) \
        \ORDER BY `t0`.`enabled` ASC, `t0`.`email` DESC \
        \LIMIT 8, 10;"

unit_update_simple :: IO ()
unit_update_simple =
  dumpUpdateSQL
    ( sqlUpdate
        ! #set [Set enabled Nothing]
        ! #where_ [Is enabled (Not Null)]
    )
    @?= Just "UPDATE `test` \
        \SET `enabled`=NULL \
        \WHERE (`enabled`) IS NOT NULL;"

unit_update'_simple :: IO ()
unit_update'_simple =
  dumpUpdateSQL
    ( sqlUpdate'
        ! #save (Test "a@example.com" (Just False))
        ! #where_ [Is email (Eq "a@example.com")]
    )
    @?= Just "UPDATE `test` \
        \SET `email`='a@example.com', `enabled`=FALSE \
        \WHERE (`email`) = ('a@example.com');"

unit_modelToSets :: IO ()
unit_modelToSets = do
  dumpUpdateSQL
    ( sqlUpdate'
        ! #save (Test "a@example.com" Nothing)
        ! #where_ [Is email (Eq "a@example.com")]
    )
    @?= Just "UPDATE `test` \
        \SET `email`='a@example.com', `enabled`=DEFAULT \
        \WHERE (`email`) = ('a@example.com');"
