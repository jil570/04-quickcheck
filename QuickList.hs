{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuickList where

import Data.List as List
import Test.QuickCheck

prop_const' :: Eq a => a -> a -> Bool
prop_const' a b = const a b == a

data Undefined

instance Testable Undefined where
  property = error "Unimplemented property"

prop_const :: Eq a => (a -> a -> a) -> a -> a -> Bool
prop_const const' a b = const' a b == a

constBug :: a -> a -> a
constBug _ b = b -- Oops: this returns the *second* argument, not the first.

prop_minimum :: Ord a => ([a] -> a) -> Undefined
prop_minimum minimum' = undefined

prop_minimum0 :: Ord a => ([a] -> a) -> [a] -> [a] -> Property
prop_minimum0 minimum' l1 l2 =
  not (null l1) ==> not (null l2)
    ==> minimum' (l1 ++ l2) == min (minimum' l1) (minimum' l2)

prop_minimum1 :: Ord a => ([a] -> a) -> [a] -> Property
prop_minimum1 minimum' l1 =
  not (null l1) ==> minimum' l1 == head (sort l1)

prop_minimum2 :: Ord a => ([a] -> a) -> [a] -> Bool
prop_minimum2 minimum' l1 =
  null l1 || minimum' l1 == head (sort l1)

prop_minimum3 :: Ord a => ([a] -> a) -> [a] -> Property
prop_minimum3 minimum' l1 =
  not (null l1) ==> minimum' l1 == List.minimum l1

prop_minimum4 :: Ord a => ([a] -> a) -> [a] -> Property
prop_minimum4 minimum' l1 =
  not (null l1)
    ==> all (\x -> minimum' l1 <= x) l1

minimumBug :: Ord a => [a] -> a
minimumBug = undefined

newtype SmallNonNegInt = SmallNonNegInt Int deriving (Eq, Ord, Show, Read)

instance Arbitrary SmallNonNegInt where
  arbitrary = undefined
  shrink = undefined

prop_replicate :: (Int -> a -> [a]) -> Undefined
prop_replicate replicate' = undefined

replicateBug :: Int -> a -> [a]
replicateBug = undefined

prop_group_1 :: Eq a => ([a] -> [[a]]) -> Undefined
prop_group_1 group' = undefined

prop_group_2 :: Eq a => ([a] -> [[a]]) -> Undefined
prop_group_2 group' = undefined

groupBug :: Eq a => [a] -> [[a]]
groupBug = undefined

prop_reverse_1 :: ([a] -> [a]) -> Undefined
prop_reverse_1 reverse' = undefined

prop_reverse_2 :: ([a] -> [a]) -> Undefined
prop_reverse_2 reverse' = undefined

reverseBug_1 :: [a] -> [a]
reverseBug_1 = undefined

reverseBug_2 :: [a] -> [a]
reverseBug_2 = undefined

main :: IO ()
main = do
  let qcName name prop = do
        putStr $ name ++ ": "
        quickCheck prop

  putStrLn "The following tests should all succeed:"
  qcName "const" $ prop_const (const :: Char -> Char -> Char)
  qcName "minimum" $ prop_minimum (minimum :: String -> Char)
  qcName "replicate" $ prop_replicate (replicate :: Int -> Char -> String)
  qcName "group_1" $ prop_group_1 (group :: String -> [String])
  qcName "group_2" $ prop_group_2 (group :: String -> [String])
  qcName "reverse_1" $ prop_reverse_1 (reverse :: String -> String)
  qcName "reverse_2" $ prop_reverse_2 (reverse :: String -> String)

  putStrLn ""

  putStrLn "The following tests should all fail:"
  qcName "const" $ prop_const (constBug :: Char -> Char -> Char)
  qcName "minimum" $ prop_minimum (minimumBug :: String -> Char)
  qcName "replicate" $ prop_replicate (replicateBug :: Int -> Char -> String)
  qcName "group_1" $ prop_group_1 (groupBug :: String -> [String])
  qcName "group_2" $ prop_group_2 (groupBug :: String -> [String])
  qcName "reverse_1" $ prop_reverse_1 (reverseBug_1 :: String -> String)
  qcName "reverse_2" $ prop_reverse_2 (reverseBug_2 :: String -> String)
