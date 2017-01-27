module Main where

import Data.List
import Data.Char
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

{-
  Working with 2 effects: combining the effects of Maybe and Either.
  Maybe will represent the non-existence of an entity. Either will
  represent a validation error. A simple and yes, contrived, example
  for pedagogical purposes.
-}
type TransformerStack a = MaybeT (Either String) a

type ProductName = String
type CustomerName = String

data Product = Product {
    productName :: ProductName
  , productBasePrice :: Float
}

data Customer = Customer {
    customerName :: CustomerName
  , customerDiscount :: Float
}

embedMaybe :: (Monad monad) => Maybe a -> MaybeT monad a
embedMaybe maybe = MaybeT $ return maybe

products :: [Product]
products = [
    Product "MacBook Pro" 2500.00
  , Product "Lenovo T460" 1500.00
 ]

customers :: [Customer]
customers = [
    Customer "Whole Foods" 0.05
  , Customer "Safeway" 0.10
 ]

validateProductName :: ProductName -> Either String ProductName
validateProductName name =
    let validator = \n -> not (null n) && (isAlpha $ head n)
    in validate name validator $ "invalid product name: " ++ name

validateCustomerName :: CustomerName -> Either String CustomerName
validateCustomerName name =
    let validator = \n -> not (null n) && (isUpper $ head n)
    in validate name validator $ "invalid customer name: " ++ name

validate :: value -> (value -> Bool) -> String -> Either String value
validate value validator message = if validator value then Right value else Left message

findBasePrice :: ProductName -> TransformerStack Float
findBasePrice name = do
    validName <- lift $ validateProductName name
    product <- embedMaybe $ find ((== validName) . productName) products
    return $ productBasePrice product

findCustomerDiscount :: CustomerName -> TransformerStack Float
findCustomerDiscount name = do
    validName <- lift $ validateCustomerName name
    customer <- embedMaybe $ find ((== validName) . customerName) customers
    return $ customerDiscount customer

findPriceForCustomer :: CustomerName -> ProductName -> TransformerStack Float

findPriceForCustomer customerName productName = do
    basePrice <- findBasePrice productName
    customerDiscount <- findCustomerDiscount customerName
    return (basePrice * (1 - customerDiscount))

main :: IO ()
main = do
    print $ findPriceForCustomer "Whole Foods" "MacBook Pro"
    print $ findPriceForCustomer "Whole Foods" "Non-Existent"
    print $ findPriceForCustomer "Non-Existent" "MacBook Pro"
    print $ findPriceForCustomer "Safeway" "$acBook Pro"

