module Yoga.Go.HTTP.OmIO
  ( OmIO(..)
  , runOmIO
  , liftIO
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Data.Variant (Variant)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Go.IO (IO)

newtype OmIO ctx err a = OmIO (ReaderT ctx (ExceptT (Variant (exception :: Error | err)) IO) a)

derive newtype instance Functor (OmIO ctx err)
derive newtype instance Apply (OmIO ctx err)
derive newtype instance Applicative (OmIO ctx err)
derive newtype instance Bind (OmIO ctx err)
derive newtype instance Monad (OmIO ctx err)
derive newtype instance MonadRec (OmIO ctx err)
derive newtype instance MonadAsk ctx (OmIO ctx err)
derive newtype instance MonadReader ctx (OmIO ctx err)
derive newtype instance MonadThrow (Variant (exception :: Error | err)) (OmIO ctx err)
derive newtype instance MonadError (Variant (exception :: Error | err)) (OmIO ctx err)

instance MonadEffect (OmIO ctx err) where
  liftEffect effect = liftIO (liftEffect effect)

liftIO :: forall ctx err a. IO a -> OmIO ctx err a
liftIO action = OmIO (lift (lift action))

runOmIO :: forall ctx err a. ctx -> OmIO ctx err a -> IO (Either (Variant (exception :: Error | err)) a)
runOmIO context (OmIO om) = runExceptT (runReaderT om context)
