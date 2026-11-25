-- | A small shop's inventory as a typed REST API. Every route says in its type what it reads
-- | and what it can answer, SKUs and quantities are checked once at the door, and the stock
-- | lives in an `IORef` that every handler gets from the Om context. Each request runs on its
-- | own goroutine.
module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Go.IO (IO, launchIO_)
import Go.IO.Console (log)
import Go.IO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Yoga.Go.HTTP.Om (Handler, Host(..), OmIO, Port(..), createServer, defaultServerOptions, handle, listen, liftIO, registerAPIWith, reject, respond)
import Yoga.Go.HTTP.Om.Route (GET, JSON, POST, Response, Route)
import Yoga.HTTP.API.Path (class ParseParam, type (/), type (:), type (:?))
import Yoga.JSON (class WriteForeign)
import Yoga.Om (ask)

type InventoryAPI = { addProduct :: AddProduct, getProduct :: GetProduct, lowStock :: LowStock, sell :: Sell }

type AddProduct = Route POST "products"
  { body :: JSON NewProduct }
  (created :: { body :: Product }, badRequest :: { body :: Problem }, conflict :: { body :: Problem })

type GetProduct = Route GET ("products" / "sku" : Sku)
  {}
  (ok :: { body :: Product }, notFound :: { body :: Problem })

type LowStock = Route GET ("products" :? { below :: Int })
  {}
  (ok :: { body :: Array Product })

type Sell = Route POST ("products" / "sku" : Sku / "sell")
  { body :: JSON { quantity :: Int } }
  (ok :: { body :: Product }, badRequest :: { body :: Problem }, notFound :: { body :: Problem }, conflict :: { body :: Problem })

main :: Effect Unit
main = launchIO_ do
  inventory <- newIORef Map.empty
  server <- createServer defaultServerOptions # liftEffect
  registerAPIWith @InventoryAPI { inventory } { addProduct, getProduct, lowStock, sell } server # liftEffect
  address <- listen { host: Host "127.0.0.1", port: Port 8070 } server
  log ("inventory on http://127.0.0.1:" <> show (un Port address.port))

addProduct :: Handler AddProduct (inventory :: Inventory)
addProduct = handle do
  { body, inventory } <- ask
  product <- validProduct body # orBadRequest
  added <- insertNew product inventory # liftIO
  if added then respond @"created" product
  else reject @"conflict" { error: un Sku product.sku <> " is already on the shelf" }

getProduct :: Handler GetProduct (inventory :: Inventory)
getProduct = handle do
  { path, inventory } <- ask
  found <- readIORef inventory <#> Map.lookup path.sku # liftIO
  maybe (reject @"notFound" (unknown path.sku)) (respond @"ok") found

lowStock :: Handler LowStock (inventory :: Inventory)
lowStock = handle do
  { query, inventory } <- ask
  products <- readIORef inventory <#> Map.values >>> Array.fromFoldable # liftIO
  respond @"ok" (Array.filter (runningLow query.below) products)
  where
  runningLow below product = maybe true (un Stock product.stock < _) below

sell :: Handler Sell (inventory :: Inventory)
sell = handle do
  { path, body, inventory } <- ask
  quantity <- parseStock body.quantity # orBadRequest
  sale <- takeFromShelf path.sku quantity inventory # liftIO
  case sale of
    Sold product -> respond @"ok" product
    OnlyLeft left -> reject @"conflict" { error: "only " <> show (un Stock left) <> " left" }
    NotStocked -> reject @"notFound" (unknown path.sku)

-- The domain: values that made it past the door are valid.

newtype Sku = Sku String
newtype ProductName = ProductName String
newtype Stock = Stock Int

type Product = { sku :: Sku, name :: ProductName, stock :: Stock }
type NewProduct = { sku :: String, name :: String, stock :: Int }
type Problem = { error :: String }
type Inventory = IORef (Map Sku Product)

data Sale = Sold Product | OnlyLeft Stock | NotStocked

validProduct :: NewProduct -> Either String Product
validProduct { sku, name, stock } = { sku: _, name: _, stock: _ } <$> parseSku sku <*> parseName name <*> parseStock stock

parseSku :: String -> Either String Sku
parseSku text =
  if looksLikeASku then Right (Sku text)
  else Left ("a SKU is 3 to 16 of A-Z, 0-9 and -, not " <> show text)
  where
  looksLikeASku = between 3 16 (String.length text) && Array.all skuCharacter (toCharArray text)
  skuCharacter character = between 'A' 'Z' character || between '0' '9' character || character == '-'

parseName :: String -> Either String ProductName
parseName text
  | String.trim text == "" = Left "a product needs a name"
  | otherwise = Right (ProductName (String.trim text))

parseStock :: Int -> Either String Stock
parseStock count
  | count < 0 = Left ("a quantity can't be negative, got " <> show count)
  | otherwise = Right (Stock count)

-- The shelf: every change is one atomic swap, so goroutines never see half a sale.

insertNew :: Product -> Inventory -> IO Boolean
insertNew product inventory = atomicModifyIORef' inventory \products ->
  if Map.member product.sku products then products /\ false
  else Map.insert product.sku product products /\ true

takeFromShelf :: Sku -> Stock -> Inventory -> IO Sale
takeFromShelf sku (Stock wanted) inventory = atomicModifyIORef' inventory \products ->
  case Map.lookup sku products of
    Nothing -> products /\ NotStocked
    Just product@{ stock: Stock onShelf }
      | onShelf < wanted -> products /\ OnlyLeft product.stock
      | otherwise -> do
          let sold = product { stock = Stock (onShelf - wanted) }
          Map.insert sku sold products /\ Sold sold

-- Small helpers.

orBadRequest :: forall ctx err a. Either String a -> OmIO ctx (badRequest :: Response () Problem | err) a
orBadRequest = either (\error -> reject @"badRequest" { error }) pure

unknown :: Sku -> Problem
unknown sku = { error: "no product " <> un Sku sku }

derive instance Newtype Sku _
derive instance Newtype Stock _
derive newtype instance Eq Sku
derive newtype instance Ord Sku
derive newtype instance WriteForeign Sku
derive newtype instance WriteForeign ProductName
derive newtype instance WriteForeign Stock

instance ParseParam Sku where
  parseParam = parseSku
