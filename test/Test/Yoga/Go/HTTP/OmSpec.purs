module Test.Yoga.Go.HTTP.OmSpec where

import Prelude

import Data.Array as Array
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String as String
import Control.Parallel (parTraverse)
import Data.DateTime.Instant (diff)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, bracket)
import Effect.Now (now)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Go.Net.HTTP (ClientResponse, Header, HeaderName(..), HeaderValue(..), Method(..), Status(..), URL(..))
import Go.Net.HTTP as HTTP
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Yoga.Go.HTTP.Om.API (registerAPI, registerAPIWith)
import Yoga.Go.HTTP.Om.Route (GET, POST, Handler, Route, handle, reject, respond, respondNoContent, respondWith)
import Yoga.Go.HTTP.Om.Server (BodyLimit(..), Host(..), Port(..), close, createServer, defaultServerOptions, listen)
import Yoga.HTTP.API.Path (Required, type (/), type (:), type (:?))
import Yoga.HTTP.API.Route.Encoding (FormData, JSON, PlainText)
import Yoga.Om (ask)

type User = { id :: Int, name :: String }

type GetUserRoute = Route GET
  ("users" / "id" : Int)
  {}
  ( ok :: { body :: User }
  , notFound :: { body :: { error :: String } }
  )

type CreateUserRoute = Route POST
  "users"
  { body :: JSON { name :: String } }
  (created :: { body :: User })

type RepeatRoute = Route GET
  ("repeat" :? { tag :: Required (Array String) })
  {}
  (ok :: { body :: String })

type ContextRoute = Route GET
  "context"
  { headers :: { xrequest :: String }
  , cookies :: { session :: String }
  }
  (ok :: { body :: String })

type NumberRoute = Route GET
  ("numbers" / "id" : Int)
  {}
  (ok :: { body :: String })

type LatestRoute = Route GET
  ("numbers" / "latest")
  {}
  (ok :: { body :: String })

type PlainRoute = Route POST
  "plain"
  { body :: PlainText }
  (ok :: { body :: String })

type FormRoute = Route POST
  "form"
  { body :: FormData { name :: String, tag :: Array String } }
  (ok :: { body :: String })

type ResponseRoute = Route GET
  "response"
  {}
  (created :: { headers :: { "x-test" :: String }, body :: String })

type NoContentRoute = Route POST
  "no-content"
  {}
  (noContent :: {})

type NotModifiedRoute = Route GET
  "not-modified"
  {}
  (notModified :: {})

type SlowRoute = Route GET
  ("slow" / "id" : Int)
  {}
  (ok :: { body :: String })

type CrashRoute = Route GET
  "crash"
  {}
  (ok :: { body :: String })

type DependencyRoute = Route GET
  "dependency"
  {}
  (ok :: { body :: String })

type API =
  { getUser :: GetUserRoute
  , createUser :: CreateUserRoute
  , repeat :: RepeatRoute
  , context :: ContextRoute
  , number :: NumberRoute
  , zLatest :: LatestRoute
  , plain :: PlainRoute
  , form :: FormRoute
  , response :: ResponseRoute
  , noContent :: NoContentRoute
  , notModified :: NotModifiedRoute
  , crash :: CrashRoute
  , slow :: SlowRoute
  }

type DependencyAPI = { dependency :: DependencyRoute }

getUserHandler :: Handler GetUserRoute ()
getUserHandler = handle do
  { path } <- ask
  if path.id == 42 then respond @"ok" { id: path.id, name: "Alice" }
  else reject @"notFound" { error: "missing:" <> show path.id }

createUserHandler :: Handler CreateUserRoute ()
createUserHandler = handle do
  { body } <- ask
  respond @"created" { id: 7, name: body.name }

repeatHandler :: Handler RepeatRoute ()
repeatHandler = handle do
  { query } <- ask
  respond @"ok" (String.joinWith "|" query.tag)

contextHandler :: Handler ContextRoute ()
contextHandler = handle do
  { headers, cookies } <- ask
  respond @"ok" (headers.xrequest <> ":" <> cookies.session)

numberHandler :: Handler NumberRoute ()
numberHandler = handle do
  { path } <- ask
  respond @"ok" (show path.id)

latestHandler :: Handler LatestRoute ()
latestHandler = handle (respond @"ok" "latest")

plainHandler :: Handler PlainRoute ()
plainHandler = handle do
  { body } <- ask
  respond @"ok" body

formHandler :: Handler FormRoute ()
formHandler = handle do
  { body } <- ask
  respond @"ok" (body.name <> ":" <> String.joinWith "|" body.tag)

responseHandler :: Handler ResponseRoute ()
responseHandler = handle (respondWith @"created" { "x-test": "present" } "created")

noContentHandler :: Handler NoContentRoute ()
noContentHandler = handle respondNoContent

notModifiedHandler :: Handler NotModifiedRoute ()
notModifiedHandler = handle (reject @"notModified" unit)

slowHandler :: Handler SlowRoute ()
slowHandler = handle do
  { path } <- ask
  liftAff (Aff.delay (Milliseconds 100.0))
  respond @"ok" (show path.id)

crashHandler :: Handler CrashRoute ()
crashHandler = handle do
  liftAff (Aff.throwError (error "boom") :: Aff Unit)
  respond @"ok" "unreachable"

dependencyHandler :: Handler DependencyRoute (prefix :: String)
dependencyHandler = handle do
  { prefix } <- ask
  respond @"ok" (prefix <> "-injected")

type Fixture = { baseUrl :: String }

withServer :: forall a. (Fixture -> Aff a) -> Aff a
withServer test = bracket acquire (close <<< _.server) (test <<< _.fixture)
  where
  acquire = do
    server <- liftEffect do
      value <- createServer (defaultServerOptions { maxBodyBytes = BodyLimit 1024 })
      registerAPI @API
        { getUser: getUserHandler
        , createUser: createUserHandler
        , repeat: repeatHandler
        , context: contextHandler
        , number: numberHandler
        , zLatest: latestHandler
        , plain: plainHandler
        , form: formHandler
        , response: responseHandler
        , noContent: noContentHandler
        , notModified: notModifiedHandler
        , crash: crashHandler
        , slow: slowHandler
        }
        value
      pure value
    liftEffect (registerAPIWith @DependencyAPI { prefix: "deps" } { dependency: dependencyHandler } server)
    address <- listen { host: Host "127.0.0.1", port: Port 0 } server
    pure { server, fixture: { baseUrl: "http://127.0.0.1:" <> show (un Port address.port) } }

get :: Fixture -> String -> Aff ClientResponse
get fixture route = send fixture "GET" route [] ""

post :: Fixture -> String -> String -> Array Header -> Aff ClientResponse
post fixture route body headers = send fixture "POST" route headers body

send :: Fixture -> String -> String -> Array Header -> String -> Aff ClientResponse
send fixture method route headers body = do
  let request = (HTTP.clientRequest (Method method) (URL (fixture.baseUrl <> route))) { headers = headers, body = body }
  HTTP.sendAff request >>= either (liftEffect <<< throw <<< show) pure

contentType :: String -> Header
contentType value = { name: HeaderName "Content-Type", value: HeaderValue value }

status :: ClientResponse -> Int
status = un Status <<< HTTP.status

spec :: Spec Unit
spec = around withServer
  ( describe "Go HTTP typed adapter" do
      describe "typed routes" do
        it "answers a path capture with JSON" \fixture -> do
          response <- get fixture "/users/42"
          status response `shouldEqual` 200
          HTTP.responseBody response `shouldEqual` "{\"name\":\"Alice\",\"id\":42}"
          HTTP.responseHeader (HeaderName "content-type") response `shouldEqual` Just (HeaderValue "application/json; charset=utf-8")

        it "answers a rejection with its status and body" \fixture -> do
          response <- get fixture "/users/9"
          status response `shouldEqual` 404
          HTTP.responseBody response `shouldEqual` "{\"error\":\"missing:9\"}"

        it "decodes a JSON request body" \fixture -> do
          response <- post fixture "/users" "{\"name\":\"Ada\"}" [ contentType "application/json" ]
          status response `shouldEqual` 201
          HTTP.responseBody response `shouldEqual` "{\"name\":\"Ada\",\"id\":7}"

      describe "typed request decoding" do
        it "preserves repeated query values" \fixture -> do
          response <- get fixture "/repeat?tag=first&tag=second&tag="
          status response `shouldEqual` 200
          HTTP.responseBody response `shouldEqual` "first|second|"

        it "decodes headers and percent-encoded cookies" \fixture -> do
          response <- send fixture "GET" "/context"
            [ { name: HeaderName "xrequest", value: HeaderValue "req-7" }
            , { name: HeaderName "cookie", value: HeaderValue "session=hello%20world" }
            ]
            ""
          status response `shouldEqual` 200
          HTTP.responseBody response `shouldEqual` "req-7:hello world"

        it "decodes plain text bodies" \fixture -> do
          response <- post fixture "/plain" "こんにちは" [ contentType "text/plain" ]
          status response `shouldEqual` 200
          HTTP.responseBody response `shouldEqual` "こんにちは"

        it "prefers literal routes over capture routes" \fixture -> do
          response <- get fixture "/numbers/latest"
          status response `shouldEqual` 200
          HTTP.responseBody response `shouldEqual` "latest"

        it "decodes repeated URL-encoded form fields" \fixture -> do
          response <- post fixture "/form" "name=Ada&tag=compiler&tag=math" [ contentType "application/x-www-form-urlencoded" ]
          status response `shouldEqual` 200
          HTTP.responseBody response `shouldEqual` "Ada:compiler|math"

      describe "typed responses" do
        it "sets mapped status and typed response headers" \fixture -> do
          response <- get fixture "/response"
          status response `shouldEqual` 201
          HTTP.responseHeader (HeaderName "x-test") response `shouldEqual` Just (HeaderValue "present")
          HTTP.responseBody response `shouldEqual` "created"

        it "sends bodyless 204 and 304 responses" \fixture -> do
          noContent <- post fixture "/no-content" "" []
          notModified <- get fixture "/not-modified"
          status noContent `shouldEqual` 204
          HTTP.responseBody noContent `shouldEqual` ""
          status notModified `shouldEqual` 304
          HTTP.responseBody notModified `shouldEqual` ""

        it "injects handler dependencies through registerAPIWith" \fixture -> do
          response <- get fixture "/dependency"
          status response `shouldEqual` 200
          HTTP.responseBody response `shouldEqual` "deps-injected"

      describe "concurrency" do
        it "serves requests whose handlers wait at the same time" \fixture -> do
          started <- liftEffect now
          bodies <- parTraverse (\id -> HTTP.responseBody <$> get fixture ("/slow/" <> show id)) (Array.range 1 20)
          finished <- liftEffect now
          bodies `shouldEqual` map show (Array.range 1 20)
          (diff finished started :: Milliseconds) `shouldSatisfy` (_ < Milliseconds 1000.0)

      describe "protocol failures" do
        it "returns 404 for unknown paths" \fixture -> do
          response <- get fixture "/missing"
          status response `shouldEqual` 404
          HTTP.responseBody response `shouldEqual` "Not Found"

        it "returns 405 with Allow for known paths" \fixture -> do
          response <- post fixture "/repeat" "" []
          status response `shouldEqual` 405
          HTTP.responseHeader (HeaderName "allow") response `shouldEqual` Just (HeaderValue "GET")

        it "returns structured 400 responses for typed parse failures" \fixture -> do
          invalidPath <- get fixture "/numbers/not-an-int"
          invalidJson <- post fixture "/users" "{bad" [ contentType "application/json" ]
          missingBody <- post fixture "/plain" "" []
          status invalidPath `shouldEqual` 400
          status invalidJson `shouldEqual` 400
          status missingBody `shouldEqual` 400
          HTTP.responseBody invalidPath `shouldEqual` "{\"error\":\"Invalid request\",\"details\":[{\"field\":\"path\",\"error\":\"Invalid path parameter 'id': Expected an integer but got: not-an-int\"}]}"

        it "rejects unsupported request media types" \fixture -> do
          response <- post fixture "/users" "{\"name\":\"Ada\"}" [ contentType "text/plain" ]
          status response `shouldEqual` 415

        it "enforces the byte body limit" \fixture -> do
          let oversized = String.joinWith "" (Array.replicate 300 "💥")
          response <- post fixture "/plain" oversized [ contentType "text/plain" ]
          status response `shouldEqual` 413
          HTTP.responseBody response `shouldEqual` "Payload Too Large"

        it "converts handler exceptions to 500" \fixture -> do
          response <- get fixture "/crash"
          status response `shouldEqual` 500
          HTTP.responseBody response `shouldEqual` "Internal Server Error"
  )
