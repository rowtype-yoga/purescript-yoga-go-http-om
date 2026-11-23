module Yoga.Go.HTTP.Om.Route.Handler
  ( handle
  , handleIO
  , Handler(..)
  , respond
  , respondWith
  , respondNoContent
  , respondNotModified
  , reject
  , rejectWith
  , mapReject
  , class ToLabel
  , class Is2xxStatus
  , class SplitResponse
  , class SplitResponseRL
  , class SplitResponseEntry
  , class BuildErrorHandlers
  , buildErrorHandlers
  , class RouteResponseVariant
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (either)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff as Aff
import Effect.Exception (Error)
import Prim.Boolean (True, False)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Route.Response (Response(..))
import Yoga.HTTP.API.Route.StatusCode (class StatusCodeToLabel)
import Record as Record
import Yoga.Go.HTTP.Om.Route.HandleResponse (class HandleResponse)
import Yoga.Go.HTTP.Om.Route.HandleRoute (RouteRunner, class RouteShape, routeRunner)
import Yoga.Go.HTTP.Om.Route.ParseBody (class ParseBody)
import Yoga.Go.HTTP.Om.Route.ParseCookies (class ParseCookies)
import Yoga.Go.HTTP.Om.Route.ParseHeaders (class ParseHeaders)
import Yoga.Go.HTTP.Om.Route.ParsePathParams (class ParsePathParams)
import Yoga.Go.HTTP.Om.Route.ParseQueryParams (class ParseQueryParamsFromObject)
import Yoga.HTTP.API.Path (class PathPattern)
import Yoga.HTTP.API.Route.Handler (class DefaultRequestFields, class EncodingBody, class SegmentPathParams, class SegmentQueryParams)
import Yoga.HTTP.API.Route.RenderMethod (class RenderMethod)
import Yoga.HTTP.API.Route.Route (Route, class ConvertResponseVariant)
import Yoga.HTTP.API.Route.RouteHandler (class RouteCookies, class RouteHandler)
import Yoga.Om (Om, handleErrors', runOm)
import Go.IO (throwIO)
import Yoga.Go.HTTP.Om.Server (sendingAff, sendingIO)
import Yoga.Go.HTTP.OmIO (OmIO, runOmIO)

-- | Convert either a status code (Int) or label (Symbol) to a label (Symbol).
-- | This allows functions to accept both @404 and @"notFound" polymorphically.
class ToLabel :: Type -> Symbol -> Constraint
class ToLabel labelOrCode label | labelOrCode -> label

-- Int codes map to their label via StatusCodeToLabel
instance toLabelInt :: StatusCodeToLabel code label => ToLabel (Proxy code) label

-- Symbols map to themselves
else instance toLabelSymbol :: ToLabel (Proxy label) label

-- | Determine whether a variant label corresponds to a 2xx HTTP status code.
class Is2xxStatus (label :: Symbol) (is2xx :: Boolean) | label -> is2xx

instance Is2xxStatus "ok" True
else instance Is2xxStatus "created" True
else instance Is2xxStatus "accepted" True
else instance Is2xxStatus "nonAuthoritativeInformation" True
else instance Is2xxStatus "noContent" True
else instance Is2xxStatus "resetContent" True
else instance Is2xxStatus "partialContent" True
else instance Is2xxStatus "multiStatus" True
else instance Is2xxStatus "alreadyReported" True
else instance Is2xxStatus "imUsed" True
else instance Is2xxStatus label False

-- | Split a response variant row into success (2xx) and error (non-2xx) sub-rows.
class
  SplitResponse (respVariant :: Row Type) (successRow :: Row Type) (errorRow :: Row Type)
  | respVariant -> successRow errorRow

instance
  ( RL.RowToList respVariant rl
  , SplitResponseRL rl successRow errorRow
  ) =>
  SplitResponse respVariant successRow errorRow

class
  SplitResponseRL (rl :: RL.RowList Type) (successRow :: Row Type) (errorRow :: Row Type)
  | rl -> successRow errorRow

instance SplitResponseRL RL.Nil () ()

instance
  ( Is2xxStatus label is2xx
  , SplitResponseEntry is2xx label ty tail successRow errorRow
  ) =>
  SplitResponseRL (RL.Cons label ty tail) successRow errorRow

-- | Dispatch based on Is2xxStatus result.
class
  SplitResponseEntry
    (is2xx :: Boolean)
    (label :: Symbol)
    (ty :: Type)
    (tail :: RL.RowList Type)
    (successRow :: Row Type)
    (errorRow :: Row Type)
  | is2xx label ty tail -> successRow errorRow

-- 2xx: add to success row
instance
  ( SplitResponseRL tail successTail errorRow
  , Row.Cons label ty successTail successRow
  , Row.Lacks label successTail
  ) =>
  SplitResponseEntry True label ty tail successRow errorRow

-- non-2xx: add to error row
instance
  ( SplitResponseRL tail successRow errorTail
  , Row.Cons label ty errorTail errorRow
  , Row.Lacks label errorTail
  ) =>
  SplitResponseEntry False label ty tail successRow errorRow

-- | Build a record of handlers for `Variant.onMatch` that convert each
-- | error variant label into the full response variant.
class
  BuildErrorHandlers
    (rl :: RL.RowList Type)
    (respVariant :: Row Type)
    (handlers :: Row Type)
  | rl respVariant -> handlers where
  buildErrorHandlers
    :: Proxy rl
    -> Proxy respVariant
    -> Builder (Record ()) (Record handlers)

instance BuildErrorHandlers RL.Nil respVariant () where
  buildErrorHandlers _ _ = identity

instance
  ( IsSymbol label
  , Row.Cons label ty _r1 respVariant
  , BuildErrorHandlers tail respVariant tailHandlers
  , Row.Cons label (ty -> Om ctx () (Variant respVariant)) tailHandlers handlers
  , Row.Lacks label tailHandlers
  ) =>
  BuildErrorHandlers (RL.Cons label ty tail) respVariant handlers where
  buildErrorHandlers _ respProxy =
    buildErrorHandlers (Proxy :: Proxy tail) respProxy
      >>> Builder.insert (Proxy :: Proxy label) handler
    where
    handler :: ty -> Om ctx () (Variant respVariant)
    handler val = pure (Variant.inj (Proxy :: Proxy label) val)

-- | Return a response with a specific reason label or status code
-- |
-- | Example:
-- | ```purescript
-- | respond @"ok" { id: 1, name: "Alice" }
-- | respond @200 { id: 1, name: "Alice" }
-- | respond @"created" newUser
-- | respond @201 newUser
-- | ```
respond
  :: forall @labelOrCode label body r1 r2 m
   . ToLabel (Proxy labelOrCode) label
  => IsSymbol label
  => Row.Cons label (Response () body) r1 r2
  => Applicative m
  => body
  -> m (Variant r2)
respond body =
  pure (Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body }))

-- | Return a response with a specific reason label or status code and custom headers
-- |
-- | Example:
-- | ```purescript
-- | respondWith @"created" { "Location": "/users/123" } user
-- | respondWith @201 { "Location": "/users/123" } user
-- | ```
respondWith
  :: forall @labelOrCode label headers body r1 r2 m
   . ToLabel (Proxy labelOrCode) label
  => IsSymbol label
  => Row.Cons label (Response headers body) r1 r2
  => Applicative m
  => Record headers
  -> body
  -> m (Variant r2)
respondWith headers body =
  pure (Variant.inj (Proxy :: Proxy label) (Response { headers, body }))

-- | Return a 204 No Content response (no headers, no body).
-- |
-- | Example:
-- | ```purescript
-- | respondNoContent
-- | ```
respondNoContent
  :: forall m r
   . Applicative m
  => m (Variant (noContent :: Response () Unit | r))
respondNoContent =
  pure ((Variant.inj (Proxy :: Proxy "noContent") (Response { headers: {}, body: unit })))

-- | Return a 304 Not Modified response (no headers, no body).
-- |
-- | Example:
-- | ```purescript
-- | respondNotModified
-- | ```
respondNotModified
  :: forall m r
   . Applicative m
  => m (Variant (notModified :: Response () Unit | r))
respondNotModified =
  pure (Variant.inj (Proxy :: Proxy "notModified") (Response { headers: {}, body: unit }))

-- | Throw a non-2xx error response (short-circuits the handler)
-- |
-- | Example:
-- | ```purescript
-- | reject @"notFound" { error: "User not found" }
-- | reject @404 { error: "User not found" }
-- | reject @"badRequest" { error: "Invalid input" }
-- | reject @400 { error: "Invalid input" }
-- | ```
reject
  :: forall @labelOrCode label body _r1 err _r2 m a
   . ToLabel (Proxy labelOrCode) label
  => IsSymbol label
  => Row.Cons label (Response () body) _r1 err
  => Row.Cons label (Response () body) _r2 (exception :: Error | err)
  => MonadThrow (Variant (exception :: Error | err)) m
  => body
  -> m a
reject body =
  throwError (Variant.inj (Proxy :: Proxy label) (Response { headers: {}, body }))

-- | Throw a non-2xx error response with custom headers (short-circuits)
-- |
-- | Example:
-- | ```purescript
-- | rejectWith @"unauthorized" { "WWW-Authenticate": "Bearer" } { error: "Invalid token" }
-- | rejectWith @401 { "WWW-Authenticate": "Bearer" } { error: "Invalid token" }
-- | ```
rejectWith
  :: forall @labelOrCode label headers body _r1 err _r2 m a
   . ToLabel (Proxy labelOrCode) label
  => IsSymbol label
  => Row.Cons label (Response headers body) _r1 err
  => Row.Cons label (Response headers body) _r2 (exception :: Error | err)
  => MonadThrow (Variant (exception :: Error | err)) m
  => Record headers
  -> body
  -> m a
rejectWith headers body =
  throwError (Variant.inj (Proxy :: Proxy label) (Response { headers, body }))

mapReject
  :: forall @from @toLabelOrCode toLabel tyIn body ctx errIn errMid errOut extra a
   . IsSymbol from
  => ToLabel (Proxy toLabelOrCode) toLabel
  => IsSymbol toLabel
  => Row.Cons from tyIn (exception :: Error | errMid) (exception :: Error | errIn)
  => Row.Cons toLabel (Response () body) (exception :: Error | errMid) (exception :: Error | errOut)
  => Row.Union (exception :: Error | errMid) extra (exception :: Error | errOut)
  => (tyIn -> body)
  -> Om ctx errIn a
  -> Om ctx errOut a
mapReject f = handleErrors' \variant ->
  variant # Variant.on (Proxy :: Proxy from)
    (\v -> throwError (Variant.inj (Proxy :: Proxy toLabel) (Response { headers: {}, body: f v })))
    (throwError <<< Variant.expand)


-- | A handler with deferred dependency injection.
-- | The `ctx` row tracks what extra dependencies are needed beyond the request.
-- | Use `handle` to create one, and `registerAPILayer` to provide the deps.
newtype Handler route (ctx :: Row Type) = Handler (Record ctx -> RouteRunner)


-- | Create a `Handler` from an Om computation.
-- |
-- | The Om computation receives the request context
-- | (path, query, headers, cookies, body) plus any extra dependencies via
-- | `ask`. Request field types are verified against the route at compile time.
-- |
-- | Example:
-- | ```purescript
-- | putUserHandler :: Handler PutUser (userRepo :: UserRepo)
-- | putUserHandler = handle do
-- |   { path, body, userRepo } <- ask
-- |   existing <- userRepo.findByName path.name # liftAff
-- |   case existing of
-- |     Just user -> respond @"ok" user
-- |     Nothing -> do
-- |       user <- userRepo.create path.name body.email # liftAff
-- |       respond @"created" user
-- | ```
handle
  :: forall @route pathParams queryParams reqHeaders reqCookies body respVariant
       successRow errorRow extraCtx pathQuery method segments partialRequest o_ fullEncoding userResp merged
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  => RouteCookies route reqCookies
  => SplitResponse respVariant successRow errorRow
  => Row.Union successRow errorRow respVariant
  => Row.Union errorRow successRow respVariant
  => Row.Union pathParams queryParams pathQuery
  => Row.Nub pathQuery pathQuery
  => Row.Lacks "respondNow" errorRow
  => Row.Lacks "path" extraCtx
  => Row.Lacks "query" extraCtx
  => Row.Lacks "headers" extraCtx
  => Row.Lacks "cookies" extraCtx
  => Row.Lacks "body" extraCtx
  => Row.Union
       ( path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record reqHeaders
       , cookies :: Record reqCookies
       , body :: body
       )
       extraCtx
       merged
  => Row.Nub merged
       ( path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record reqHeaders
       , cookies :: Record reqCookies
       , body :: body
       | extraCtx
       )
  => RouteShape route method segments partialRequest userResp
  => Row.Union partialRequest o_ (headers :: Record reqHeaders, cookies :: Record reqCookies, body :: fullEncoding)
  => DefaultRequestFields partialRequest reqHeaders reqCookies fullEncoding
  => RenderMethod method
  => PathPattern segments
  => SegmentPathParams segments pathParams
  => SegmentQueryParams segments queryParams
  => EncodingBody fullEncoding body
  => ParsePathParams pathParams
  => ParseQueryParamsFromObject queryParams
  => ParseHeaders reqHeaders
  => ParseCookies reqCookies
  => ParseBody fullEncoding body
  => ConvertResponseVariant userResp respVariant
  => HandleResponse respVariant
  => Om
       { path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record reqHeaders
       , cookies :: Record reqCookies
       , body :: body
       | extraCtx
       }
       (respondNow :: Variant successRow | errorRow)
       (Variant successRow)
  -> Handler route extraCtx
handle om = Handler \deps -> routeRunner @route sendingAff \requestCtx ->
  runOm (Record.merge requestCtx deps) { exception: Aff.throwError } $
    handleErrors' errorHandler (Variant.expand <$> om')
  where
  om' = handleErrors' respondNowHandler om
  respondNowHandler = Variant.on (Proxy :: Proxy "respondNow") pure throwError
  errorHandler = Variant.on (Proxy :: Proxy "exception")
    (throwError <<< Variant.inj (Proxy :: Proxy "exception"))
    (pure <<< Variant.expand)

handleIO
  :: forall @route pathParams queryParams reqHeaders reqCookies body respVariant
       successRow errorRow extraCtx pathQuery method segments partialRequest o_ fullEncoding userResp merged
   . RouteHandler route pathParams queryParams reqHeaders body respVariant
  => RouteCookies route reqCookies
  => SplitResponse respVariant successRow errorRow
  => Row.Union successRow errorRow respVariant
  => Row.Union errorRow successRow respVariant
  => Row.Union pathParams queryParams pathQuery
  => Row.Nub pathQuery pathQuery
  => Row.Lacks "respondNow" errorRow
  => Row.Lacks "path" extraCtx
  => Row.Lacks "query" extraCtx
  => Row.Lacks "headers" extraCtx
  => Row.Lacks "cookies" extraCtx
  => Row.Lacks "body" extraCtx
  => Row.Union
       ( path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record reqHeaders
       , cookies :: Record reqCookies
       , body :: body
       )
       extraCtx
       merged
  => Row.Nub merged
       ( path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record reqHeaders
       , cookies :: Record reqCookies
       , body :: body
       | extraCtx
       )
  => RouteShape route method segments partialRequest userResp
  => Row.Union partialRequest o_ (headers :: Record reqHeaders, cookies :: Record reqCookies, body :: fullEncoding)
  => DefaultRequestFields partialRequest reqHeaders reqCookies fullEncoding
  => RenderMethod method
  => PathPattern segments
  => SegmentPathParams segments pathParams
  => SegmentQueryParams segments queryParams
  => EncodingBody fullEncoding body
  => ParsePathParams pathParams
  => ParseQueryParamsFromObject queryParams
  => ParseHeaders reqHeaders
  => ParseCookies reqCookies
  => ParseBody fullEncoding body
  => ConvertResponseVariant userResp respVariant
  => HandleResponse respVariant
  => OmIO
       { path :: Record pathParams
       , query :: Record queryParams
       , headers :: Record reqHeaders
       , cookies :: Record reqCookies
       , body :: body
       | extraCtx
       }
       (respondNow :: Variant successRow | errorRow)
       (Variant successRow)
  -> Handler route extraCtx
handleIO om = Handler \deps -> routeRunner @route sendingIO \requestCtx ->
  runOmIO (Record.merge requestCtx deps) om >>= either answer (pure <<< Variant.expand)
  where
  answer = Variant.on (Proxy :: Proxy "respondNow") (pure <<< Variant.expand)
    (Variant.on (Proxy :: Proxy "exception") throwIO (pure <<< Variant.expand))

class RouteResponseVariant (route :: Type) (respVariant :: Row Type) | route -> respVariant

instance
  ( ConvertResponseVariant userResp respVariant
  ) =>
  RouteResponseVariant (Route method segments request userResp) respVariant
