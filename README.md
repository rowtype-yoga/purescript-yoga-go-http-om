# purescript-yoga-go-http-om

Typed `yoga-http-api` routes, served by Go's `net/http`. Compile with [Iris](https://github.com/i-am-the-slime/purescript-analyzer/tree/iris-go)
and your API is a single native binary.

```purescript
module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Yoga.Go.HTTP.Om (Handler, Host(..), Port(..), createServer, defaultServerOptions, handle, listen, registerAPI, reject, respond)
import Yoga.Go.HTTP.Om.Route (GET, Route)
import Yoga.HTTP.API.Path (type (/), type (:))
import Yoga.Om (ask)

type User = { id :: Int, name :: String }

type GetUser = Route GET ("users" / "id" : Int) {} (ok :: { body :: User }, notFound :: { body :: { error :: String } })

type API = { getUser :: GetUser }

getUser :: Handler GetUser ()
getUser = handle do
  { path } <- ask
  if path.id == 42 then respond @"ok" { id: 42, name: "Alice" }
  else reject @"notFound" { error: "no user " <> show path.id }

main :: Effect Unit
main = do
  server <- createServer defaultServerOptions
  registerAPI @API { getUser } server
  launchAff_ do
    address <- listen { host: Host "127.0.0.1", port: Port 8080 } server
    liftEffect (log ("listening on " <> show address.port))
```

```bash
iris go --entry Main.main out
cd out && go build -o api . && ./api
```

```
$ curl localhost:8080/users/42
{"name":"Alice","id":42}
$ curl localhost:8080/users/7
{"error":"no user 7"}
$ curl localhost:8080/users/abc
{"error":"Invalid request","details":[{"field":"path","error":"Invalid path parameter 'id': Expected an integer but got: abc"}]}
```

It's the same API as `yoga-node-http-om`: path captures, query parameters, headers, cookies, plain
text, JSON and form bodies, typed responses and headers, 204 and 304, and a 400 that lists every
field that didn't parse. Unknown paths get a 404, known paths with the wrong method a 405 with
`Allow`, oversized bodies a 413, the wrong content type a 415, and a handler that throws a 500.

## Dependencies

A handler that needs something says so in its type, and `registerAPIWith` hands it over:

```purescript
greet :: Handler GreetRoute (prefix :: String)
greet = handle do
  { prefix } <- ask
  respond @"ok" (prefix <> " there")

registerAPIWith @API { prefix: "hi" } { greet } server
```

## Handlers run at the same time

Go serves every request on its own goroutine. While a handler waits, say in `delay` or on another
HTTP call, other requests keep going. PureScript itself still runs one piece at a time, like in
JavaScript, so a `Ref` behaves the way you're used to.

## Not there yet

- Multipart and raw byte bodies, streaming requests and responses, and WebSockets.
- `registerAPILayer`, `serverLayer` and `listenLayer`: they compile, but `yoga-om-layer` doesn't have
  Go for its foreign imports yet. Use `registerAPIWith`.
- Plain `mkHandler` functions: write handlers with `handle`.
- A client that disconnects doesn't cancel its handler.

## Testing

```bash
./test.sh
```

compiles the specs with Iris and runs them against a real server.
