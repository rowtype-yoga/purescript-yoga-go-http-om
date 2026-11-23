# purescript-yoga-go-http-om

Typed `yoga-http-api` routes, served by Go's `net/http`. Compile with [Iris](https://github.com/i-am-the-slime/purescript-analyzer/tree/iris-go)
and your API is a single native binary.

```purescript
module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Go.IO (launchIO_)
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
  launchIO_ do
    address <- listen { host: Host "127.0.0.1", port: Port 8080 } server
    log ("listening on " <> show address.port)
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

Go serves every request on its own goroutine. Handlers run in `IO`, so as long as your program
doesn't use `Aff`, they really run in parallel, on every core you've got. Reach for `liftIO` to call
other `IO` code from a handler, like `threadDelay` or another HTTP call.

## Got Aff code?

Every `IO` function has an `Aff` twin with the same name plus `Aff`: `handleAff`, `listenAff`,
`closeAff`, `addRouteAff` and `sendResponseAff`. Mix them freely, a `Handler` is a `Handler`
either way:

```purescript
slow :: Handler SlowRoute ()
slow = handleAff do
  delay (Milliseconds 100.0) # liftAff
  respond @"ok" "done"
```

The catch: once `Aff` is in the program, PureScript runs one piece at a time, like in JavaScript.
Other requests still keep going while a handler waits, and a `Ref` behaves the way you're used to,
but you're down to one core.

## How fast?

About as fast as writing it by hand. I pointed [oha](https://github.com/hatoo/oha) at the example and
at two hand-written `net/http` servers answering the same route: one using `encoding/json`, one
writing pre-baked bytes. Loopback, M5 Max with 18 cores, median of three 10 second runs:

| Server | `/users/42`, 50 connections | `/users/42`, 200 connections | `/users/7` (404), 200 connections |
| --- | ---: | ---: | ---: |
| this library | 108,017 req/s | 131,607 req/s | 131,980 req/s |
| hand-written, `encoding/json` | 120,337 req/s | 133,612 req/s | 133,268 req/s |
| hand-written, bytes | 121,387 req/s | 133,266 req/s | 133,058 req/s |

p99 latency at 200 connections is 3.1 ms here against 2.6 to 2.7 ms by hand. Run it yourself with

```bash
bench/bench.sh && bench/summarize.sh
```

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
