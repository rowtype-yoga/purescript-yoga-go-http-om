module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Go.IO (launchIO_)
import Yoga.Go.HTTP.Om (Handler, Host(..), Port(..), createServer, defaultServerOptions, handleIO, listenIO, registerAPI, reject, respond)
import Yoga.Go.HTTP.Om.Route (GET, Route)
import Yoga.HTTP.API.Path (type (/), type (:))
import Yoga.Om (ask)

type User = { id :: Int, name :: String }

type GetUser = Route GET ("users" / "id" : Int) {} (ok :: { body :: User }, notFound :: { body :: { error :: String } })

type API = { getUser :: GetUser }

getUser :: Handler GetUser ()
getUser = handleIO do
  { path } <- ask
  if path.id == 42 then respond @"ok" { id: 42, name: "Alice" }
  else reject @"notFound" { error: "no user " <> show path.id }

main :: Effect Unit
main = do
  server <- createServer defaultServerOptions
  registerAPI @API { getUser } server
  launchIO_ do
    address <- listenIO { host: Host "127.0.0.1", port: Port 8080 } server
    log ("listening on " <> show address.port) # liftEffect
