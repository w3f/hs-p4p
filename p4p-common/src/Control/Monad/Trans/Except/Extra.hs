{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Trans.Except.Extra where

import           Control.Monad              (unless)
import           Control.Monad.Trans.Except (ExceptT (..), throwE)
import           Data.Type.Reflect          (HasConstructor, constrName)
import           GHC.Generics               (Generic, Rep)


type ErrMsg = String

checkConsistent
  :: (Eq a, Show a, Monad m)
  => String
  -> String
  -> a
  -> a
  -> ExceptT ErrMsg m ()
checkConsistent prefix name a b =
  unless (a == b)
    $  throwE
    $  prefix
    <> ": inconsistent "
    <> name
    <> ": "
    <> show a
    <> " vs "
    <> show b

checkConstrName
  :: (Show a, HasConstructor (Rep a), Generic a, Monad m)
  => String
  -> a
  -> String
  -> ExceptT ErrMsg m ()
checkConstrName prefix a c =
  unless (constrName a == c)
    $  throwE
    $  prefix
    <> ": bad constructor: "
    <> show a
    <> " vs "
    <> c

checkMaxSize :: (Monad m) => String -> Int -> Int -> ExceptT ErrMsg m ()
checkMaxSize prefix expected actual =
  unless (actual <= expected)
    $  throwE
    $  prefix
    <> ": too big: "
    <> show actual
    <> " vs "
    <> show expected
