module Yoga.Go.HTTP.Om.Route.HandleResponse
  ( class HandleResponse
  , handleResponse
  , class HandleResponseRL
  , handleResponseRL
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Route.Response (Response(..))
import Yoga.HTTP.API.Route.StatusCode (StatusCode(..), class StatusCodeMap, statusCodeFor)
import Yoga.JSON (class WriteForeign, writeImpl)
import Yoga.Go.HTTP.Om.Route.SetHeaders (class SetHeaders, setHeaders)
import Yoga.Go.HTTP.Om.Server (RequestHandle, sendResponse, sendResponseWithoutBody)

class HandleResponse (respVariant :: Row Type) where
  handleResponse :: Proxy respVariant -> Variant respVariant -> RequestHandle -> Aff Unit

instance (RowToList respVariant rl, HandleResponseRL rl respVariant) => HandleResponse respVariant where
  handleResponse _ = handleResponseRL (Proxy :: Proxy rl)

class HandleResponseRL (rl :: RowList Type) (respVariant :: Row Type) | rl -> respVariant where
  handleResponseRL :: Proxy rl -> Variant respVariant -> RequestHandle -> Aff Unit

instance HandleResponseRL RL.Nil () where
  handleResponseRL _ = Variant.case_

instance
  ( IsSymbol label
  , StatusCodeMap label
  , SetHeaders headers
  , HandleResponseRL tail rest
  , Cons label (Response headers Unit) rest respVariant
  , Lacks label rest
  ) =>
  HandleResponseRL (RL.Cons label (Response headers Unit) tail) respVariant where
  handleResponseRL _ variant request =
    Variant.on (Proxy :: Proxy label) handler rest variant
    where
    handler :: Response headers Unit -> Aff Unit
    handler (Response response) =
      let StatusCode status = statusCodeFor (Proxy :: Proxy label)
      in
        sendResponseWithoutBody
          status
          (setHeaders response.headers)
          request

    rest :: Variant rest -> Aff Unit
    rest response = handleResponseRL (Proxy :: Proxy tail) response request

else instance
  ( IsSymbol label
  , StatusCodeMap label
  , SetHeaders headers
  , WriteForeign body
  , HandleResponseRL tail rest
  , Cons label (Response headers body) rest respVariant
  , Lacks label rest
  ) =>
  HandleResponseRL (RL.Cons label (Response headers body) tail) respVariant where
  handleResponseRL _ variant request =
    Variant.on (Proxy :: Proxy label) handler rest variant
    where
    handler :: Response headers body -> Aff Unit
    handler (Response response) =
      let StatusCode status = statusCodeFor (Proxy :: Proxy label)
      in
        sendResponse
          status
          (setHeaders response.headers)
          (writeImpl response.body)
          request

    rest :: Variant rest -> Aff Unit
    rest response = handleResponseRL (Proxy :: Proxy tail) response request
