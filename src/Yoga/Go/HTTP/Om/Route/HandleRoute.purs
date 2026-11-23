module Yoga.Go.HTTP.Om.Route.HandleRoute
  ( RouteRunner
  , class RouteShape
  , routeRunner
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), blush)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Variant (Variant)
import Foreign.Object as Object
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Yoga.Go.HTTP.Om.Route.HandleResponse (class HandleResponse, handleResponse)
import Yoga.Go.HTTP.Om.Route.ParseBody (class ParseBody, parseBody, requestBodyKind)
import Yoga.Go.HTTP.Om.Route.ParseCookies (class ParseCookies, parseCookies)
import Yoga.Go.HTTP.Om.Route.ParseHeaders (class ParseHeaders, parseHeaders)
import Yoga.Go.HTTP.Om.Route.ParsePathParams (class ParsePathParams, parsePathParams)
import Yoga.Go.HTTP.Om.Route.ParseQueryParams (class ParseQueryParamsFromObject, parseQueryParamsFromObject)
import Yoga.Go.HTTP.Om.Server (BodyKind, Reply, RequestHandle, Sending, Serving, requestData)
import Yoga.HTTP.API.Path (class PathPattern, pathPattern)
import Yoga.HTTP.API.Route.Handler (Request, class DefaultRequestFields, class EncodingBody, class SegmentPathParams, class SegmentQueryParams)
import Yoga.HTTP.API.Route.RenderMethod (class RenderMethod, renderMethod)
import Yoga.HTTP.API.Route.Route (Route, class ConvertResponseVariant)
import Yoga.JSON (write, writeJSON)

-- | Everything a server needs to serve one route.
type RouteRunner =
  { method :: String
  , pattern :: String
  , bodyKind :: BodyKind
  , serving :: Serving
  }

-- | The parts of a route type, whether its request is written `Request { … }` or `{ … }`.
class RouteShape :: forall k. Type -> Type -> k -> Row Type -> Row Type -> Constraint
class RouteShape route method segments partialRequest userResp | route -> method segments partialRequest userResp

instance RouteShape (Route method segments (Request (Record partialRequest)) userResp) method segments partialRequest userResp
else instance RouteShape (Route method segments (Record partialRequest) userResp) method segments partialRequest userResp

-- | The route's runner: it parses the request into the handler's typed context, runs the handler
-- | and sends its response, or answers 400 with every parse error.
routeRunner
  :: forall @route m method segments partialRequest o_ fullHeaders fullCookies fullEncoding userResp respVariant
       pathParams queryParams body
   . Monad m
  => RouteShape route method segments partialRequest userResp
  => Row.Union partialRequest o_ (headers :: Record fullHeaders, cookies :: Record fullCookies, body :: fullEncoding)
  => DefaultRequestFields partialRequest fullHeaders fullCookies fullEncoding
  => RenderMethod method
  => PathPattern segments
  => SegmentPathParams segments pathParams
  => SegmentQueryParams segments queryParams
  => EncodingBody fullEncoding body
  => ParsePathParams pathParams
  => ParseQueryParamsFromObject queryParams
  => ParseHeaders fullHeaders
  => ParseCookies fullCookies
  => ParseBody fullEncoding body
  => ConvertResponseVariant userResp respVariant
  => HandleResponse respVariant
  => Sending m
  -> ( { path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record fullHeaders
       , cookies :: Record fullCookies
       , body :: body
       }
       -> m (Variant respVariant)
     )
  -> RouteRunner
routeRunner sending handler =
  { method: String.toUpper (renderMethod (Proxy :: Proxy method))
  , pattern: pathPattern (Proxy :: Proxy segments)
  , bodyKind: requestBodyKind (Proxy :: Proxy fullEncoding)
  , serving: sending.serving run
  }
  where
  run :: RequestHandle -> m Unit
  run request = case decoded request of
    Left invalid -> sending.reply invalid request
    Right context -> do
      result <- handler context
      sending.reply (handleResponse (Proxy :: Proxy respVariant) result) request

  decoded request = do
    let { pathParams, query, headers, cookies, bodyPresent, body: rawBody } = requestData request
    let pathResult = parsePathParams (Proxy :: Proxy pathParams) pathParams
    let queryResult = parseQueryParamsFromObject (Proxy :: Proxy queryParams) query
    let headersResult = parseHeaders (Proxy :: Proxy fullHeaders) headers
    let cookiesResult = parseCookies (Proxy :: Proxy fullCookies) cookies
    let bodyResult = parseBody (Proxy :: Proxy fullEncoding) bodyPresent rawBody
    let
      errors = Array.concat
        [ pathResult # blush # foldMap (map (tagField "path"))
        , queryResult # blush # foldMap (map (tagField "query"))
        , headersResult # blush # foldMap (NEA.toArray >>> map (show >>> tagField "headers"))
        , cookiesResult # blush # foldMap (NEA.toArray >>> map (show >>> tagField "cookies"))
        , bodyResult # blush # foldMap (pure >>> map (tagField "body"))
        ]
    case NEA.fromArray errors, pathResult, queryResult, headersResult, cookiesResult, bodyResult of
      Nothing, Right path, Right parsedQuery, Right parsedHeaders, Right parsedCookies, Right body ->
        Right { path, query: parsedQuery, headers: parsedHeaders, cookies: parsedCookies, body }
      _, _, _, _, _, _ -> Left (invalidRequest errors)

  tagField field error = { field, error }

  invalidRequest :: Array { field :: String, error :: String } -> Reply
  invalidRequest details =
    { status: 400
    , headers: Object.singleton "content-type" "application/json; charset=utf-8"
    , body: Just (write (writeJSON { error: "Invalid request", details }))
    }
