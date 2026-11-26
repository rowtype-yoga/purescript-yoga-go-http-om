# Inventory API

A small shop's stock as a REST API, in one PureScript file that googoo turns into one Go binary.

- Four routes, each a type that says what it reads and what it can answer: a path capture, a query
  parameter, JSON bodies, and 200, 201, 400, 404 and 409 as the variants of its response row.
- SKUs, names and quantities become newtypes the moment they arrive. A SKU in the path is parsed
  by the route itself, so a handler never sees a bad one.
- Errors are `reject @"conflict" { error: … }`: the compiler checks the route can answer that.
- The stock is an `IORef (Map Sku Product)` that every handler gets from the Om context, and every
  change is one atomic swap. Requests run on goroutines, on every core.

```purescript
type Sell = Route POST ("products" / "sku" : Sku / "sell")
  { body :: JSON { quantity :: Int } }
  (ok :: { body :: Product }, badRequest :: { body :: Problem }, notFound :: { body :: Problem }, conflict :: { body :: Problem })

sell :: Handler Sell (inventory :: Inventory)
sell = handle do
  { path, body, inventory } <- ask
  quantity <- parseStock body.quantity # orBadRequest
  sale <- takeFromShelf path.sku quantity inventory # liftIO
  case sale of
    Sold product -> respond @"ok" product
    OnlyLeft left -> reject @"conflict" { error: "only " <> show (un Stock left) <> " left" }
    NotStocked -> reject @"notFound" (unknown path.sku)
```

The whole thing is [src/Main.purs](src/Main.purs).

## Running it

You need googoo 0.1.18-go.5 or newer and Go 1.27 (`brew install spago i-am-the-slime/tap/googoo`).

```bash
cd examples/inventory-api
googoo run --go
```

It gets its packages from the
[Go package set](https://github.com/rowtype-yoga/purescript-go-package-sets) `go-0.1.18-20261126.3`.

```
$ curl --json '{"sku":"WIDGET-1","name":"Widget","stock":12}' localhost:8070/products
{"stock":12,"sku":"WIDGET-1","name":"Widget"}                                         201
$ curl --json '{"sku":"WIDGET-1","name":"Widget again","stock":1}' localhost:8070/products
{"error":"WIDGET-1 is already on the shelf"}                                          409
$ curl --json '{"sku":"widget","name":"Widget","stock":1}' localhost:8070/products
{"error":"a SKU is 3 to 16 of A-Z, 0-9 and -, not \"widget\""}                        400
$ curl --json '{"sku":"GADGET-2","name":"  Gadget ","stock":3}' localhost:8070/products
{"stock":3,"sku":"GADGET-2","name":"Gadget"}                                          201
$ curl localhost:8070/products/NOPE-9
{"error":"no product NOPE-9"}                                                         404
$ curl localhost:8070/products/no
{"error":"Invalid request","details":[{"field":"path","error":"Invalid path parameter 'sku': a SKU is 3 to 16 of A-Z, 0-9 and -, not \"no\""}]}   400
$ curl 'localhost:8070/products?below=5'
[{"stock":3,"sku":"GADGET-2","name":"Gadget"}]                                        200
$ curl --json '{"quantity":5}' localhost:8070/products/WIDGET-1/sell
{"stock":7,"sku":"WIDGET-1","name":"Widget"}                                          200
$ curl --json '{"quantity":50}' localhost:8070/products/WIDGET-1/sell
{"error":"only 7 left"}                                                               409
$ curl --json '{"quantity":-2}' localhost:8070/products/WIDGET-1/sell
{"error":"a quantity can't be negative, got -2"}                                      400
```

(The status codes on the right are from `curl -w`.)

## Against Go by hand

[go/main.go](go/main.go) is the same API with `net/http`, `encoding/json` and a `sync.Mutex`: the
same routes, checks and answers, on port 8071. It's 184 lines of code against 120 here, and it
does less: a body that doesn't parse gets a bare `Invalid request` instead of a list of what was
wrong, and nothing stops a handler from answering with a status its route doesn't document.

## How fast?

Both servers stocked with the same 31 products, [oha](https://github.com/hatoo/oha) with 200
connections for 10 seconds per route, the two servers taking turns, three runs each. Apple M5 Max,
18 cores, Go 1.27, googoo 0.1.18-go.5. The machine was quiet before it started (load average
3.5), the table has the best run of each:

| | `GET /products/WIDGET-1` | `POST /products/WIDGET-1/sell` | `GET /products?below=10` |
| --- | ---: | ---: | ---: |
| this example | 136,474 req/s | 134,033 req/s | 105,424 req/s |
| by hand, `net/http` | 137,414 req/s | 134,033 req/s | 125,167 req/s |
| | 99% | 100% | 84% |

Looking up and selling are as fast as `net/http`. The list is the slow one: the map becomes a list
and then an array before it's filtered and turned into JSON.

To run it yourself:

```bash
googoo go                                    # prints where the binary is
(cd go && go build -o ../.spago/by-hand .)
.spago/go/inventory-api/bin/inventory-api & .spago/by-hand &
for port in 8070 8071; do
  curl -s --json '{"sku":"WIDGET-1","name":"Widget","stock":2000000000}' localhost:$port/products
done
oha -z 10s -c 200 http://127.0.0.1:8070/products/WIDGET-1
oha -z 10s -c 200 http://127.0.0.1:8071/products/WIDGET-1
oha -z 10s -c 200 -m POST -H 'content-type: application/json' -d '{"quantity":1}' http://127.0.0.1:8070/products/WIDGET-1/sell
```

## The Go googoo writes

`googoo go out` puts the Go for this API in `out/`. Each release made it read more like Go:

| | 0.1.18-go.2 | 0.1.18-go.3 | 0.1.18-go.5 |
| --- | --- | --- | --- |
| `switch either` | 7 | 1 | 0 |
| average function name | 29.8 characters | 23.3 characters | 20.4 characters |
| function names of 30 characters or more | 54 | 34 | 22 |
| longest function name | 72 characters | 62 characters | 52 characters |

A handler that rejects returns `(value, err)`, so there's no `Either` left to switch on.
