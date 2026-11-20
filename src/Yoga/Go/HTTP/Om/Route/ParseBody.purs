module Yoga.Go.HTTP.Om.Route.ParseBody
  ( class ParseBody
  , parseBody
  , requestBodyKind
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Foreign (Foreign)
import Type.Proxy (Proxy)
import Yoga.HTTP.API.Route.Encoding (JSON, FormData, NoBody, PlainText)
import Yoga.JSON (class ReadForeign, read, readJSON)
import Yoga.Go.HTTP.Om.Server (BodyKind(..))

class ParseBody (encoding :: Type) (body :: Type) | encoding -> body where
  requestBodyKind :: Proxy encoding -> BodyKind
  parseBody :: Proxy encoding -> Boolean -> Foreign -> Either String body

instance ParseBody NoBody Unit where
  requestBodyKind _ = NoRequestBody
  parseBody _ _ _ = Right unit

instance ParseBody PlainText String where
  requestBodyKind _ = TextRequestBody
  parseBody _ present foreignBody =
    if present then lmap show (read foreignBody)
    else Left "Request body is required"

instance ReadForeign a => ParseBody (JSON a) a where
  requestBodyKind _ = JSONRequestBody
  parseBody _ present foreignBody =
    if present then do
      source <- lmap show (read foreignBody)
      lmap show (readJSON source)
    else Left "Request body is required"

instance ReadForeign a => ParseBody (FormData a) a where
  requestBodyKind _ = FormRequestBody
  parseBody _ present foreignBody =
    if present then lmap show (read foreignBody)
    else Left "Request body is required"

instance ReadForeign (Record row) => ParseBody (Record row) (Record row) where
  requestBodyKind _ = JSONRequestBody
  parseBody _ present foreignBody =
    if present then do
      source <- lmap show (read foreignBody)
      lmap show (readJSON source)
    else Left "Request body is required"
