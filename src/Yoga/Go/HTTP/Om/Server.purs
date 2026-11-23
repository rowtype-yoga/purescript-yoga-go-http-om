module Yoga.Go.HTTP.Om.Server
  ( Server
  , Port(..)
  , Host(..)
  , BodyLimit(..)
  , ServerOptions
  , defaultServerOptions
  , ListenOptions
  , BoundAddress
  , RequestHandle
  , RequestData
  , BodyKind(..)
  , Serving
  , Reply
  , Sending
  , createServer
  , addRoute
  , addRouteIO
  , addServing
  , sendingAff
  , sendingIO
  , listen
  , listenIO
  , close
  , closeIO
  , requestData
  , sendResponse
  , sendResponseWithoutBody
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_, makeAff, nonCanceler, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (Foreign, isNull, isUndefined, typeOf, unsafeFromForeign, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Go.IO (catch, throwIO)
import Go.IO.Internal (IO(..))
import Go.IO.MVar (newEmptyMVar, putMVar, takeMVar)
import Yoga.JSON (unsafeStringify)

-- | The route table and, once listening, the Go `http.Server`.
foreign import data Server :: Type

-- | One request a route matched, and its response.
foreign import data RequestHandle :: Type

-- | TCP host or IP address.
newtype Host = Host String

-- | TCP port. `Port 0` asks the operating system for an available port.
newtype Port = Port Int

-- | Positive byte-size limit for request bodies.
newtype BodyLimit = BodyLimit Int

derive instance Newtype Host _
derive instance Newtype Port _
derive instance Newtype BodyLimit _
derive newtype instance Eq Host
derive newtype instance Ord Host
derive newtype instance Show Host
derive newtype instance Eq Port
derive newtype instance Ord Port
derive newtype instance Show Port
derive newtype instance Eq BodyLimit
derive newtype instance Show BodyLimit

-- | Limits and timeouts of the Go `http.Server`.
type ServerOptions =
  { maxBodyBytes :: BodyLimit
  , headersTimeoutMilliseconds :: Milliseconds
  , requestTimeoutMilliseconds :: Milliseconds
  , keepAliveTimeoutMilliseconds :: Milliseconds
  }

-- | One MiB request bodies; 60-second headers, 300-second requests and five-second keep-alive
-- | connections.
defaultServerOptions :: ServerOptions
defaultServerOptions =
  { maxBodyBytes: BodyLimit (1024 * 1024)
  , headersTimeoutMilliseconds: Milliseconds 60_000.0
  , requestTimeoutMilliseconds: Milliseconds 300_000.0
  , keepAliveTimeoutMilliseconds: Milliseconds 5_000.0
  }

type ListenOptions =
  { host :: Host
  , port :: Port
  }

type BoundAddress =
  { address :: String
  , port :: Port
  }

type RequestData =
  { pathParams :: Object String
  , query :: Object Foreign
  , headers :: Object String
  , cookies :: Object String
  , body :: Foreign
  , bodyPresent :: Boolean
  }

data BodyKind
  = NoRequestBody
  | TextRequestBody
  | JSONRequestBody
  | FormRequestBody

bodyKindName :: BodyKind -> String
bodyKindName = case _ of
  NoRequestBody -> "none"
  TextRequestBody -> "text"
  JSONRequestBody -> "json"
  FormRequestBody -> "form"

-- | Create an unbound server. Register every route before `listen`.
createServer :: ServerOptions -> Effect Server
createServer options = pure
  ( createServerImpl
      (un BodyLimit options.maxBodyBytes)
      (un Milliseconds options.headersTimeoutMilliseconds)
      (un Milliseconds options.requestTimeoutMilliseconds)
      (un Milliseconds options.keepAliveTimeoutMilliseconds)
  )

-- | Register a low-level route. Typed API users normally call `registerAPI`. A handler that fails
-- | answers 500, as does one that finishes without a response.
addRoute :: String -> String -> BodyKind -> (RequestHandle -> Aff Unit) -> Server -> Effect Unit
addRoute method path bodyKind handler = addServing method path bodyKind (sendingAff.serving handler)

addRouteIO :: String -> String -> BodyKind -> (RequestHandle -> IO Unit) -> Server -> Effect Unit
addRouteIO method path bodyKind handler = addServing method path bodyKind (sendingIO.serving handler)

addServing :: String -> String -> BodyKind -> Serving -> Server -> Effect Unit
addServing method path bodyKind serving server = addRouteImpl method path (bodyKindName bodyKind) serving server

type Serving = RequestHandle -> Effect Unit -> IO Unit

type Reply = { status :: Int, headers :: Object String, body :: Maybe Foreign }

type Sending m =
  { reply :: Reply -> RequestHandle -> m Unit
  , serving :: (RequestHandle -> m Unit) -> Serving
  }

sendingAff :: Sending Aff
sendingAff = { reply, serving }
  where
  reply response request = sendReply response request # liftEffect

  serving handler request finished = handled # liftEffect
    where
    handled = launchAff_ do
      result <- attempt (handler request)
      liftEffect do
        either (\_ -> sendFailureImpl request) pure result
        finished

sendingIO :: Sending IO
sendingIO = { reply, serving }
  where
  reply response request = IO (sendReply response request)

  serving handler request finished = do
    handler request `catch` \_ -> IO (sendFailureImpl request)
    IO finished

-- | Bind the server and return the actual address. Supports ephemeral port 0. While it listens,
-- | the server keeps the program running.
listen :: ListenOptions -> Server -> Aff BoundAddress
listen { host, port } server = do
  bound <- liftEffect (listenImpl Left Right (un Host host) (un Port port) server)
  either (throwError <<< error) pure bound

listenIO :: ListenOptions -> Server -> IO BoundAddress
listenIO { host, port } server = do
  bound <- IO (listenImpl Left Right (un Host host) (un Port port) server)
  either (throwIO <<< error) pure bound

-- | Stop accepting connections and wait for the requests in flight.
close :: Server -> Aff Unit
close server = makeAff \done -> closeImpl server (done (Right unit)) $> nonCanceler

closeIO :: Server -> IO Unit
closeIO server = do
  closed <- newEmptyMVar
  IO (closeImpl server (signal closed))
  takeMVar closed
  where
  signal closed = case putMVar closed unit of
    IO effect -> effect

requestData :: RequestHandle -> RequestData
requestData request =
  { pathParams: object (pathNamesImpl request) (pathValuesImpl request)
  , query: repeated (queryNamesImpl request) (queryValuesImpl request)
  , headers: object (headerNamesImpl request) (headerValuesImpl request)
  , cookies: object (cookieNamesImpl request) (cookieValuesImpl request)
  , bodyPresent: bodyPresentImpl request
  , body
  }
  where
  body = case bodyKindImpl request of
    "form" -> unsafeToForeign (repeated (formNamesImpl request) (formValuesImpl request))
    _ -> unsafeToForeign (bodyImpl request)

-- | Answer with the status, headers and body: a string body is plain text, `null` and
-- | `undefined` send no body, anything else is JSON.
sendResponse :: Int -> Object String -> Foreign -> RequestHandle -> Aff Unit
sendResponse status headers body = sendingAff.reply { status, headers, body: Just body }

-- | Answer with the status and headers only.
sendResponseWithoutBody :: Int -> Object String -> RequestHandle -> Aff Unit
sendResponseWithoutBody status headers = sendingAff.reply { status, headers, body: Nothing }

sendReply :: Reply -> RequestHandle -> Effect Unit
sendReply { status, headers, body } request = case body of
  Nothing -> send false "" ""
  Just value -> case typeOf value of
    "string" -> send true (unsafeFromForeign value) "text/plain; charset=utf-8"
    _
      | isNull value || isUndefined value -> send false "" ""
      | otherwise -> send true (unsafeStringify value) "application/json; charset=utf-8"
  where
  send hasBody text defaultType = sendResponseImpl status (map fst entries) (map snd entries) hasBody text defaultType request
  entries = Object.toUnfoldable headers :: Array (String /\ String)

object :: Array String -> Array String -> Object String
object names values = Object.fromFoldable (Array.zipWith (/\) names values)

-- | One value as a string, several as an array, as Node's `URLSearchParams` collects them.
repeated :: Array String -> Array (Array String) -> Object Foreign
repeated names values = Object.fromFoldable (Array.zipWith entry names values)
  where
  entry name = case _ of
    [ single ] -> name /\ unsafeToForeign single
    several -> name /\ unsafeToForeign several

foreign import createServerImpl :: Int -> Number -> Number -> Number -> Server
foreign import addRouteImpl :: String -> String -> String -> (RequestHandle -> Effect Unit -> IO Unit) -> Server -> Effect Unit
foreign import listenImpl :: forall r. (String -> r) -> (BoundAddress -> r) -> String -> Int -> Server -> Effect r
foreign import closeImpl :: Server -> Effect Unit -> Effect Unit
foreign import pathNamesImpl :: RequestHandle -> Array String
foreign import pathValuesImpl :: RequestHandle -> Array String
foreign import queryNamesImpl :: RequestHandle -> Array String
foreign import queryValuesImpl :: RequestHandle -> Array (Array String)
foreign import headerNamesImpl :: RequestHandle -> Array String
foreign import headerValuesImpl :: RequestHandle -> Array String
foreign import cookieNamesImpl :: RequestHandle -> Array String
foreign import cookieValuesImpl :: RequestHandle -> Array String
foreign import formNamesImpl :: RequestHandle -> Array String
foreign import formValuesImpl :: RequestHandle -> Array (Array String)
foreign import bodyKindImpl :: RequestHandle -> String
foreign import bodyImpl :: RequestHandle -> String
foreign import bodyPresentImpl :: RequestHandle -> Boolean
foreign import sendResponseImpl :: Int -> Array String -> Array String -> Boolean -> String -> String -> RequestHandle -> Effect Unit
foreign import sendFailureImpl :: RequestHandle -> Effect Unit
