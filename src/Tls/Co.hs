module Tls.Co
(
    multiremberCo,
    existViaCo,
    numOfDifference,
    multiInsertLRCo,
    evensOnlyCo
) where

type ColRember a b = [a] -> [a] -> b

multiremberCo :: (Eq a) => a -> [a] -> ColRember a b -> b
multiremberCo _ [] col = col [] []
multiremberCo y (x:xs) col
    | y == x = multiremberCo y xs (\ diff same -> col diff (x:same))
    | otherwise = multiremberCo y xs (\ diff same -> col (x:diff) same)


aFriend :: [a] -> [a] -> Bool
aFriend _ = null

-- Using `aFriend` as collector, it acts like `notExist` method on `[]`
existViaCo :: (Eq a) => a -> [a] -> Bool
existViaCo z xs = not $ multiremberCo z xs aFriend


lastFriend :: [a] -> [a] -> Int
lastFriend xs _ = length xs

-- Using `lastFriend` as collector, 
-- it searches for the number of elems which are not `==` to `z`
numOfDifference :: (Eq a) => a -> [a] -> Int
numOfDifference z xs = multiremberCo z xs lastFriend


type ColInsertLR a b = [a] -> Int -> Int -> b

multiInsertLRCo :: (Eq a) => a -> a -> a -> [a] -> ColInsertLR a b -> b
multiInsertLRCo _ _ _ [] col = col [] 0 0
multiInsertLRCo new oldL oldR (x:xs) col
    | x == oldL = multiInsertLRCo new oldL oldR xs (\ newLat l r -> col (new:oldL:newLat) (l+1) r)
    | x == oldR = multiInsertLRCo new oldL oldR xs (\ newLat l r -> col (oldR:new:newLat) l (r+1))
    | otherwise = multiInsertLRCo new oldL oldR xs (\ newLat l r -> col (x:newLat) l r)



type ColEvens a b = [a] -> Int -> Int -> b 

-- Because list in Haskell cannot be heterogeneous, 
-- this implementation is a simplified version
evensOnlyCo :: [Int] -> ColEvens Int b -> b
evensOnlyCo [] col = col [] 1 0
evensOnlyCo (x:xs) col
    | even x = evensOnlyCo xs (\ new prod sum -> col (x:new) (x*prod) (x+sum))
    | otherwise = evensOnlyCo xs col