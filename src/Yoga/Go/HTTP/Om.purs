-- | A typed `yoga-http-api` server adapter for Go programs compiled with Iris, on Go's `net/http`.
-- |
-- | Define one API type, register the handler record, then call `listen`.
module Yoga.Go.HTTP.Om
  ( module Yoga.Go.HTTP.Om.Server
  , module Yoga.Go.HTTP.Om.Server.Layer
  , module Yoga.Go.HTTP.Om.Route
  , module Yoga.Go.HTTP.Om.API
  , module Yoga.Go.HTTP.OmIO
  ) where

import Yoga.Go.HTTP.Om.API (registerAPI, registerAPILayer, registerAPIWith)
import Yoga.Go.HTTP.Om.Route (Handler(..), handle, handleAff, mapReject, reject, rejectWith, respond, respondNoContent, respondNotModified, respondWith)
import Yoga.Go.HTTP.Om.Server (BodyKind(..), BodyLimit(..), BoundAddress, Host(..), ListenOptions, Port(..), RequestData, RequestHandle, Server, ServerOptions, addRoute, addRouteAff, close, closeAff, createServer, defaultServerOptions, listen, listenAff, requestData, sendResponse, sendResponseAff, sendResponseWithoutBody, sendResponseWithoutBodyAff)
import Yoga.Go.HTTP.Om.Server.Layer (BoundAddressL, ServerL, listenLayer, serverLayer)
import Yoga.Go.HTTP.OmIO (OmIO, liftIO, runOmIO)
