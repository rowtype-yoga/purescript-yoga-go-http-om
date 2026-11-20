module Yoga.Go.HTTP.Om.Server.Layer
  ( ServerL
  , BoundAddressL
  , serverLayer
  , listenLayer
  ) where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Yoga.Go.HTTP.Om.Server (BoundAddress, ListenOptions, Server, ServerOptions, close, createServer, listen)
import Yoga.Om (ask)
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, makeScopedLayer)

type ServerL r = (server :: Server | r)

type BoundAddressL r = (boundAddress :: BoundAddress | r)

-- | Acquire an unbound server and register `close` in the active scope.
-- | Use `wireLayers` to share it with route-registration and `listenLayer`.
serverLayer
  :: forall r
   . ServerOptions
  -> OmLayer (scope :: Scope | r) () (Record (ServerL ()))
serverLayer options = makeScopedLayer acquire release
  where
  acquire = do
    server <- liftEffect (createServer options)
    pure { server }

  release { server } = close server

-- | Bind a server supplied by `serverLayer`. The scoped server layer owns
-- | shutdown, so this layer only exposes the bound address.
listenLayer
  :: forall r
   . ListenOptions
  -> OmLayer (server :: Server | r) () (Record (BoundAddressL ()))
listenLayer options = makeLayer do
  { server } <- ask
  boundAddress <- liftAff (listen options server)
  pure { boundAddress }
