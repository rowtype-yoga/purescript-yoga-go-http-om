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
import Effect.Aff (Aff)
import Foreign.Object as Object
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Yoga.Go.HTTP.Om.Route.HandleResponse (class HandleResponse, handleResponse)
import Yoga.Go.HTTP.Om.Route.ParseBody (class ParseBody, parseBody, requestBodyKind)
import Yoga.Go.HTTP.Om.Route.ParseCookies (class ParseCookies, parseCookies)
import Yoga.Go.HTTP.Om.Route.ParseHeaders (class ParseHeaders, parseHeaders)
import Yoga.Go.HTTP.Om.Route.ParsePathParams (class ParsePathParams, parsePathParams)
import Yoga.Go.HTTP.Om.Route.ParseQueryParams (class ParseQueryParamsFromObject, parseQueryParamsFromObject)
import Yoga.Go.HTTP.Om.Server (BodyKind, RequestHandle, requestData, sendResponse)
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
  , run :: RequestHandle -> Aff Unit
  }

-- | The parts of a route type, whether its request is written `Request { … }` or `{ … }`.
class RouteShape :: forall k. Type -> Type -> k -> Row Type -> Row Type -> Constraint
class RouteShape route method segments partialRequest userResp | route -> method segments partialRequest userResp

instance RouteShape (Route method segments (Request (Record partialRequest)) userResp) method segments partialRequest userResp
else instance RouteShape (Route method segments (Record partialRequest) userResp) method segments partialRequest userResp

-- | The route's runner: it parses the request into the handler's typed context, runs the handler
-- | and sends its response, or answers 400 with every parse error.
routeRunner
  :: forall @route method segments partialRequest o_ fullHeaders fullCookies fullEncoding userResp respVariant
       pathParams queryParams body
   . RouteShape route method segments partialRequest userResp
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
  => ( { path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record fullHeaders
       , cookies :: Record fullCookies
       , body :: body
       }
       -> Aff (Variant respVariant)
     )
  -> RouteRunner
routeRunner handler =
  { method: String.toUpper (renderMethod (Proxy :: Proxy method))
  , pattern: pathPattern (Proxy :: Proxy segments)
  , bodyKind: requestBodyKind (Proxy :: Proxy fullEncoding)
  , run
  }
  where
  run :: RequestHandle -> Aff Unit
  run request = do
    let
      { pathParams, query, headers, cookies, bodyPresent, body: rawBody } = requestData request
      pathResult = parsePathParams (Proxy :: Proxy pathParams) pathParams
      queryResult = parseQueryParamsFromObject (Proxy :: Proxy queryParams) query
      headersResult = parseHeaders (Proxy :: Proxy fullHeaders) headers
      cookiesResult = parseCookies (Proxy :: Proxy fullCookies) cookies
      bodyResult = parseBody (Proxy :: Proxy fullEncoding) bodyPresent rawBody

      tagField field error = { field, error }

      collectErrors = Array.concat
        [ pathResult # blush # foldMap (map (tagField "path"))
        , queryResult # blush # foldMap (map (tagField "query"))
        , headersResult # blush # foldMap (NEA.toArray >>> map (show >>> tagField "headers"))
        , cookiesResult # blush # foldMap (NEA.toArray >>> map (show >>> tagField "cookies"))
        , bodyResult # blush # foldMap (pure >>> map (tagField "body"))
        ]

    case NEA.fromArray collectErrors of
      Nothing ->
        case pathResult, queryResult, headersResult, cookiesResult, bodyResult of
          Right path, Right parsedQuery, Right parsedHeaders, Right parsedCookies, Right body -> do
            result <- handler { path, query: parsedQuery, headers: parsedHeaders, cookies: parsedCookies, body }
            handleResponse (Proxy :: Proxy respVariant) result request
          _, _, _, _, _ -> pure unit
      Just errors ->
        sendResponse
          400
          (Object.singleton "content-type" "application/json; charset=utf-8")
          (write (writeJSON { error: "Invalid request", details: errors }))
          request
