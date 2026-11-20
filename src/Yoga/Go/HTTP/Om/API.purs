module Yoga.Go.HTTP.Om.API
  ( registerAPI
  , registerAPIWith
  , registerAPILayer
  , class RegisterAPI
  , registerAPIImpl
  , class RegisterAPIRL
  , registerAPIRL
  , class RegisterRunners
  , registerRunners
  , class RegisterRunnersRL
  , registerRunnersRL
  , class PickFields
  , pickFields
  , class ResolveHandlers
  , resolveHandlers
  , class ResolveHandlersRL
  , resolveHandlersRL
  , class ResolveHandlerCtx
  , class ResolveHandlerCtxRL
  , class CheckHandlerDependency
  , class ValidateAPIHandlers
  , class CheckAPIHandler
  , class NoExtraHandlers
  , class CheckAPIFieldExists
  , class APIHandlers
  , class AssertEmptyCtx
  , class AssertEmptyCtxBool
  , class IsEmptyRow
  , class IsNilRL
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Class (liftEffect)
import Prim.Boolean (True, False)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.RowList (class RowToList)
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Equality (class TypeEquals, to)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))
import Yoga.Go.HTTP.Om.Route.Handler (Handler(..))
import Yoga.Go.HTTP.Om.Route.HandleRoute (RouteRunner)
import Yoga.Go.HTTP.Om.Server (Server, addRoute)
import Yoga.HTTP.API.Route.RouteHandler (class ApiRecord)
import Yoga.Om.Layer (OmLayer, makeLayer)

-- | Check if a RowList is Nil
class IsNilRL (rl :: RL.RowList Type) (isNil :: Boolean) | rl -> isNil
instance IsNilRL RL.Nil True
else instance IsNilRL (RL.Cons n t tl) False

-- | Check if a row is empty
class IsEmptyRow (ctx :: Row Type) (empty :: Boolean) | ctx -> empty
instance (RL.RowToList ctx rl, IsNilRL rl empty) => IsEmptyRow ctx empty

-- | Assert based on Boolean result
class AssertEmptyCtxBool (empty :: Boolean) (label :: Symbol) (ctx :: Row Type)
instance AssertEmptyCtxBool True label ctx
else instance
  ( IsSymbol label
  , Fail
      ( Above
          (Text "registerAPI handler has dependencies that cannot be provided.")
        $ Above
            (Beside (Text "Handler: ") (Quote label))
        $ Above
            (Beside (Text "Dependencies: ") (Quote ctx))
            (Text "Use registerAPILayer for handlers with dependencies.")
      )
  ) =>
  AssertEmptyCtxBool False label ctx

-- | Assert that a handler's dependency context is empty.
class AssertEmptyCtx (label :: Symbol) (ctx :: Row Type)
instance (IsEmptyRow ctx empty, AssertEmptyCtxBool empty label ctx) => AssertEmptyCtx label ctx

-- | Serve a route the way its runner says.
register :: RouteRunner -> Server -> Effect Unit
register runner = addRoute runner.method runner.pattern runner.bodyKind runner.run

class RegisterAPI (handlers :: Row Type) where
  registerAPIImpl :: Record handlers -> Server -> Effect Unit

instance
  ( RowToList handlers rl
  , RegisterAPIRL rl handlers
  ) =>
  RegisterAPI handlers where
  registerAPIImpl handlers server = registerAPIRL (Proxy :: Proxy rl) handlers server

registerAPI
  :: forall @api apiRow apiRL handlers handlerRL
   . ApiRecord api apiRow
  => RL.RowToList apiRow apiRL
  => RL.RowToList handlers handlerRL
  => ValidateAPIHandlers apiRL handlerRL handlers
  => NoExtraHandlers handlerRL apiRL apiRow
  => RegisterAPI handlers
  => Record handlers
  -> Server
  -> Effect Unit
registerAPI = registerAPIImpl

class RegisterAPIRL (rl :: RL.RowList Type) (handlers :: Row Type) where
  registerAPIRL :: Proxy rl -> Record handlers -> Server -> Effect Unit

instance RegisterAPIRL RL.Nil handlers where
  registerAPIRL _ _ _ = pure unit

instance
  ( IsSymbol label
  , Row.Cons label (Handler route ctx) rest handlers
  , AssertEmptyCtx label ctx
  , TypeEquals (Record ()) (Record ctx)
  , RegisterAPIRL tail handlers
  ) =>
  RegisterAPIRL (RL.Cons label (Handler route ctx) tail) handlers where
  registerAPIRL _ handlers server = do
    let Handler runner = Record.get (Proxy :: Proxy label) handlers
    register (runner (to {})) server
    registerAPIRL (Proxy :: Proxy tail) handlers server

-- | Registers every runner of a record of them.
class RegisterRunners (runners :: Row Type) where
  registerRunners :: Record runners -> Server -> Effect Unit

instance (RowToList runners rl, RegisterRunnersRL rl runners) => RegisterRunners runners where
  registerRunners = registerRunnersRL (Proxy :: Proxy rl)

class RegisterRunnersRL (rl :: RL.RowList Type) (runners :: Row Type) where
  registerRunnersRL :: Proxy rl -> Record runners -> Server -> Effect Unit

instance RegisterRunnersRL RL.Nil runners where
  registerRunnersRL _ _ _ = pure unit

instance
  ( IsSymbol label
  , Row.Cons label RouteRunner rest runners
  , RegisterRunnersRL tail runners
  ) =>
  RegisterRunnersRL (RL.Cons label RouteRunner tail) runners where
  registerRunnersRL _ runners server = do
    register (Record.get (Proxy :: Proxy label) runners) server
    registerRunnersRL (Proxy :: Proxy tail) runners server

-- | Copies the fields of the row list out of a larger record.
class PickFields (rl :: RL.RowList Type) (from :: Row Type) (picked :: Row Type) | rl -> picked where
  pickFields :: Proxy rl -> Record from -> Builder (Record ()) (Record picked)

instance PickFields RL.Nil from () where
  pickFields _ _ = identity

instance
  ( IsSymbol label
  , Row.Cons label ty _r from
  , PickFields tail from tailPicked
  , Row.Cons label ty tailPicked picked
  , Row.Lacks label tailPicked
  ) =>
  PickFields (RL.Cons label ty tail) from picked where
  pickFields _ from =
    pickFields (Proxy :: Proxy tail) from
      >>> Builder.insert (Proxy :: Proxy label) (Record.get (Proxy :: Proxy label) from)

class ResolveHandlers (ctx :: Row Type) (handlers :: Row Type) (resolved :: Row Type) | ctx handlers -> resolved where
  resolveHandlers :: Record ctx -> Record handlers -> Record resolved

instance
  ( RL.RowToList handlers rl
  , ResolveHandlersRL rl ctx handlers resolved
  ) =>
  ResolveHandlers ctx handlers resolved where
  resolveHandlers ctx handlers = Builder.buildFromScratch (resolveHandlersRL (Proxy :: Proxy rl) ctx handlers)

class ResolveHandlersRL (rl :: RL.RowList Type) (ctx :: Row Type) (handlers :: Row Type) (resolved :: Row Type) | rl handlers -> resolved where
  resolveHandlersRL :: Proxy rl -> Record ctx -> Record handlers -> Builder (Record ()) (Record resolved)

instance ResolveHandlersRL RL.Nil ctx handlers () where
  resolveHandlersRL _ _ _ = identity

class ResolveHandlerCtx (apiLabel :: Symbol) (handlerCtx :: Row Type) (ctx :: Row Type)

instance
  ( RL.RowToList handlerCtx handlerCtxRL
  , RL.RowToList ctx ctxRL
  , ResolveHandlerCtxRL apiLabel handlerCtx handlerCtxRL ctx ctxRL
  ) =>
  ResolveHandlerCtx apiLabel handlerCtx ctx

class ResolveHandlerCtxRL (apiLabel :: Symbol) (handlerCtx :: Row Type) (required :: RL.RowList Type) (ctx :: Row Type) (available :: RL.RowList Type)

instance ResolveHandlerCtxRL apiLabel handlerCtx RL.Nil ctx available

instance
  ( CheckHandlerDependency apiLabel label ty available handlerCtx ctx
  , ResolveHandlerCtxRL apiLabel handlerCtx tail ctx available
  ) =>
  ResolveHandlerCtxRL apiLabel handlerCtx (RL.Cons label ty tail) ctx available

class CheckHandlerDependency (apiLabel :: Symbol) (label :: Symbol) (ty :: Type) (available :: RL.RowList Type) (handlerCtx :: Row Type) (ctx :: Row Type)

instance
  CheckHandlerDependency apiLabel label ty (RL.Cons label ty tail) handlerCtx ctx
else instance
  ( IsSymbol apiLabel
  , IsSymbol label
  , Fail (
      Above
        (Text "Dependency type mismatch.") $
        Above
          (Beside (Text "Handler: ") (Quote apiLabel)) $
          Above
            (Beside (Text "Dependency: ") (Quote label)) $
            Above
              (Beside (Text "Expected: ") (Quote ty)) $
              Above
                (Beside (Text "Actual: ") (Quote actualTy)) $
                Above
                  (Beside (Text "Handler context: ") (Quote handlerCtx))
                  (Beside (Text "Layer context: ") (Quote ctx))
      )
  ) =>
  CheckHandlerDependency apiLabel label ty (RL.Cons label actualTy tail) handlerCtx ctx
else instance
  CheckHandlerDependency apiLabel label ty tail handlerCtx ctx =>
  CheckHandlerDependency apiLabel label ty (RL.Cons otherLabel otherTy tail) handlerCtx ctx
else instance
  ( IsSymbol apiLabel
  , IsSymbol label
  , Fail (
      Above
        (Text "Missing required dependency.") $
        Above
          (Beside (Text "Handler: ") (Quote apiLabel)) $
          Above
            (Beside (Text "Missing dependency: ") (Quote label)) $
            Above
              (Beside (Text "Expected type: ") (Quote ty)) $
              Above
                (Beside (Text "Handler context: ") (Quote handlerCtx))
                (Beside (Text "Layer context: ") (Quote ctx))
      )
  ) =>
  CheckHandlerDependency apiLabel label ty RL.Nil handlerCtx ctx

instance
  ( IsSymbol label
  , Row.Cons label (Handler route handlerCtx) rest handlers
  , ResolveHandlerCtx label handlerCtx ctx
  , ResolveHandlersRL tail ctx handlers tailResolved
  , RowToList handlerCtx handlerCtxRL
  , PickFields handlerCtxRL ctx handlerCtx
  , Row.Cons label RouteRunner tailResolved resolved
  , Row.Lacks label tailResolved
  ) =>
  ResolveHandlersRL (RL.Cons label (Handler route handlerCtx) tail) ctx handlers resolved where
  resolveHandlersRL _ ctx handlers =
    resolveHandlersRL (Proxy :: Proxy tail) ctx handlers
      >>> Builder.insert (Proxy :: Proxy label) (runner dependencies)
    where
    Handler runner = Record.get (Proxy :: Proxy label) handlers
    dependencies = Builder.buildFromScratch (pickFields (Proxy :: Proxy handlerCtxRL) ctx)

class ValidateAPIHandlers (apiRL :: RL.RowList Type) (handlerRL :: RL.RowList Type) (handlers :: Row Type)

instance ValidateAPIHandlers RL.Nil handlerRL handlers

instance
  ( CheckAPIHandler label route handlerRL
  , ValidateAPIHandlers tail handlerRL handlers
  ) =>
  ValidateAPIHandlers (RL.Cons label route tail) handlerRL handlers

class CheckAPIHandler (label :: Symbol) (route :: Type) (handlerRL :: RL.RowList Type)

instance
  CheckAPIHandler label route (RL.Cons label (Handler route handlerCtx) tail)
else instance
  ( IsSymbol label
  , Fail (
      Above
        (Text "Handler route mismatch.") $
        Above
          (Beside (Text "Handler field: ") (Quote label)) $
          Above
            (Beside (Text "Expected route: ") (Quote route))
            (Beside (Text "Actual handler type: ") (Quote handler))
      )
  ) =>
  CheckAPIHandler label route (RL.Cons label handler tail)
else instance
  CheckAPIHandler label route tail =>
  CheckAPIHandler label route (RL.Cons otherLabel handler tail)
else instance
  ( IsSymbol label
  , Fail (
      Above
        (Text "Missing handler.") $
        Above
          (Beside (Text "Missing field: ") (Quote label))
          (Beside (Text "Expected route: ") (Quote route))
      )
  ) =>
  CheckAPIHandler label route RL.Nil

class NoExtraHandlers (handlerRL :: RL.RowList Type) (apiRL :: RL.RowList Type) (apiRow :: Row Type)

instance NoExtraHandlers RL.Nil apiRL apiRow

instance
  ( CheckAPIFieldExists label apiRL apiRow
  , NoExtraHandlers tail apiRL apiRow
  ) =>
  NoExtraHandlers (RL.Cons label handler tail) apiRL apiRow

class CheckAPIFieldExists (label :: Symbol) (apiRL :: RL.RowList Type) (apiRow :: Row Type)

instance CheckAPIFieldExists label (RL.Cons label route tail) apiRow
else instance
  CheckAPIFieldExists label tail apiRow =>
  CheckAPIFieldExists label (RL.Cons otherLabel route tail) apiRow
else instance
  ( IsSymbol label
  , Fail (
      Above
        (Text "Extra handler not in API.") $
        Above
          (Beside (Text "Extra field: ") (Quote label))
          (Beside (Text "API shape: ") (Quote apiRow))
      )
  ) =>
  CheckAPIFieldExists label RL.Nil apiRow

class APIHandlers (rl :: RL.RowList Type) (handlers :: Row Type) | rl -> handlers

instance APIHandlers RL.Nil ()

instance
  ( APIHandlers tail tailHandlers
  , Row.Cons label (Handler route handlerCtx) tailHandlers handlers
  , Row.Lacks label tailHandlers
  ) =>
  APIHandlers (RL.Cons label route tail) handlers

-- | Like `registerAPI`, for handlers with dependencies: each handler gets the fields of `deps` it
-- | asks for.
registerAPIWith
  :: forall @api apiRow apiRL handlers handlerRL resolved deps
   . ApiRecord api apiRow
  => RL.RowToList apiRow apiRL
  => RL.RowToList handlers handlerRL
  => ValidateAPIHandlers apiRL handlerRL handlers
  => NoExtraHandlers handlerRL apiRL apiRow
  => ResolveHandlers deps handlers resolved
  => RegisterRunners resolved
  => Record deps
  -> Record handlers
  -> Server
  -> Effect Unit
registerAPIWith deps handlers = registerRunners (resolveHandlers deps handlers)

registerAPILayer
  :: forall @api apiRow apiRL handlers handlerRL resolved ctx
   . ApiRecord api apiRow
  => RL.RowToList apiRow apiRL
  => RL.RowToList handlers handlerRL
  => ValidateAPIHandlers apiRL handlerRL handlers
  => NoExtraHandlers handlerRL apiRL apiRow
  => ResolveHandlers (server :: Server | ctx) handlers resolved
  => RegisterRunners resolved
  => Record handlers
  -> OmLayer (server :: Server | ctx) () {}
registerAPILayer handlers = makeLayer do
  ctx <- ask
  let resolved = resolveHandlers ctx handlers
  registerRunners resolved (Record.get (Proxy :: Proxy "server") ctx) # liftEffect
  pure {}
