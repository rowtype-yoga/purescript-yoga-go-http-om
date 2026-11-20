module Yoga.Go.HTTP.Om.Route.SetHeaders
  ( class SetHeaders
  , setHeaders
  , class SetHeadersRL
  , setHeadersRL
  ) where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class SetHeaders (headers :: Row Type) where
  setHeaders :: Record headers -> Object String

instance (RowToList headers rl, SetHeadersRL rl headers) => SetHeaders headers where
  setHeaders = setHeadersRL (Proxy :: Proxy rl)

class SetHeadersRL (rl :: RowList Type) (headers :: Row Type) | rl -> headers where
  setHeadersRL :: Proxy rl -> Record headers -> Object String

instance SetHeadersRL RL.Nil () where
  setHeadersRL _ _ = Object.empty

instance
  ( IsSymbol name
  , SetHeadersRL tail tailRow
  , Cons name String tailRow headers
  , Lacks name tailRow
  ) =>
  SetHeadersRL (RL.Cons name String tail) headers where
  setHeadersRL _ headers =
    Object.insert headerName value (setHeadersRL (Proxy :: Proxy tail) tailHeaders)
    where
    headerName = reflectSymbol (Proxy :: Proxy name)
    value = Record.get (Proxy :: Proxy name) headers
    tailHeaders = Record.delete (Proxy :: Proxy name) headers :: Record tailRow
