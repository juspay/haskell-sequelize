# haskell-sequelize

A port of
[juspay/purescript-sequelize](https://github.com/juspay/purescript-sequelize)
into Haskell, mimicking [Sequelize.js v4](https://sequelize.org/v4).

## Status

Has not been tried in production yet.

Does not provide INSERT or DELETE yet.

## Usage

### Table definition

Define a Beam table:

```haskell
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
```

Define a `ModelMeta` instance. You can modify table properties here.

```haskell
instance ModelMeta TestT where
  modelFieldModification = B.tableModification
  modelTableName = "test"
```

### SELECT

Simple SELECT:

```haskell
{-# LANGUAGE OverloadedLabels #-}

import Named

getDisabled :: SqlSelect MySQL Test
getDisabled =
  sqlSelect
    ! #where_ [Is enabled Null]
    ! defaults
```

A more complex SELECT:

```haskell
{-# LANGUAGE OverloadedLabels #-}

import Named

getSpecificPeople :: SqlSelect MySQL Test
getSpecificPeople =
  sqlSelect
    ! #where_
        [ Is enabled (Not Null),
          Or [Is email (Eq "a@example.com"), Is email (Eq "b@example.com")]
        ]
    ! #orderBy [Asc enabled, Desc email]
    ! #offset 8
    ! #limit 10
```

### UPDATE

Set some fields:

```haskell
{-# LANGUAGE OverloadedLabels #-}

import Named

busted :: SqlUpdate MySQL TestT
busted =
  sqlUpdate
    ! #set [Set enabled Nothing]
    ! #where_ [Is enabled (Not Null)]
```

Set all fields:

```haskell
{-# LANGUAGE OverloadedLabels #-}

import Named

disableSomeone :: SqlUpdate MySQL TestT
disableSomeone =
  sqlUpdate'
    ! #save (Test "a@example.com" (Just False))
    ! #where_ [Is email (Eq "a@example.com")]
```

We support `Set` and `SetDefault`.

## Developing

Nix:

```bash
$ nix-shell

(nix-shell) $ cabal build
(nix-shell) $ cabal test
```

Stack:

```bash
$ stack build
$ stack test
```
`stack test` doesn't work, because stack does not able to set `lenient` flag for cabal.
Use cabal tests instead.
Be sure the repo has `cabal.project.local` with `packages: ../beam-mysql` inside. Then run `euler dev` and then `cabal test -f lenient`
