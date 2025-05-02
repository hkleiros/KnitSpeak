module Minimize (minimize, minimize2) where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Monad (join)
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Data.List (tails, nub)
import KSSyntax (Course (..), Instruction (..), Instructions, Pattern (..))
import Knittels (Knittel (..))
import Unroll (unroll)
import Utils (patternLength)

--import qualified Data.Memocombinators as Memo

minimize :: Pattern -> Pattern
--minimize = mini . unroll 
minimize p =
  let minimized = mini (unroll p) in
      if patternLength minimized > patternLength p
        then p
      else minimized
  where
    mini (Pattern pa) = Pattern (map cm pa)
    cm (Course r is c) = Course r (ma is) c
    cm e = e

minimize2 :: Pattern -> Pattern
minimize2 = mini . unroll
  where
    mini (Pattern p) = Pattern (map cm p)
    cm (Course r is c) = Course r (m is) c
    cm e = e

{-combineCourses :: Pattern -> Pattern
combineCourses = c sortBy (\(Course _ is) (Course r2 is2) -> compare is  is2)
    where c (i: is) = undefined
  -}
-- TODO: also combine courses with the same instructions

ma :: Instructions -> Instructions
ma [] = []
ma is =
    case slidingWindow is of -- Get all repeating substructures 
        [] -> is
        r  ->
          --- Choose the substructure with largest number of repeats 
          let (s, t, i, l) = maximumBy (compare `on` snd4) r 
            in case s of
              -- Call ma again on the list and repeating structure
              [Knittel (KInst k _ a tbl)] ->
                  ma $ join [take i is, [Knittel (KInst k t a tbl)], drop (i + (l * t)) is]
              p ->
                  ma $ join [take i is, [Rep (ma p) t], drop (i + (l * t)) is]


m :: Instructions -> Instructions
m [] = []
m is = minimumBy (compare `on` len) (m'' is)
  where len :: Instructions -> Integer
        len [] = 0
        len (Knittel _ : xs) = 1 + len xs
        len (Rep  i _ : xs) = len i + len xs
        len (Loop i _ : xs) = len i + len xs

m' :: Instructions -> [Instructions]
m' [] = []
m' is =
    case slidingWindow is of -- Get all repeating substructures 
        [] -> [is]
        r  -> r >>= allOptions

    where allOptions (structure, times, index, len) = --- Choose the substructure with largest number of repeats 
             case structure of
              -- Call m' again on the list and repeating structure
              [Knittel (KInst k _ a tbl)] ->
                    m' (join [take index is, [Knittel (KInst k times a tbl)], drop (index + (len * times)) is])
              p ->  m' $ join [take index is, [Rep (m p) times], drop (index + (len * times)) is]

m'' :: Instructions -> [Instructions]
m'' [] = []
m'' is =
    case slidingWindow is of -- Get all repeating substructures 
        [] -> [is]
        r  ->  nub $ join $ map allOptions (nub r) -- >>= allOptions

    where allOptions (structure, times, index, len) = --- Choose the substructure with largest number of repeats 
             case structure of
              -- Call m'' again on the list and repeating structure
              [Knittel (KInst k _ a tbl)] ->
                    m'' (join [take index is, [Knittel (KInst k times a tbl)], drop (index + (len * times)) is])
              p ->  m'' $ join [take index is, [Rep (m p) times], drop (index + (len * times)) is]

slidingWindow :: (Eq a) => [a] -> [([a], Int, Int, Int)]
slidingWindow [] = [([], 1, 0, 0)]
slidingWindow l =
  filter (\x -> snd4 x > 1) $
    [1 .. length l `div` 2]
      >>= ( \wSize ->
              zipWith
                (\w i -> (w, numberOfTimes w wSize (drop i l), i, wSize))
                (windows wSize l)
                [0, 1 ..]
          )

numberOfTimes :: (Eq a) => [a] -> Int -> [a] -> Int
numberOfTimes [] _ [] = 0
numberOfTimes rep w p
  | rep == take w p = go 1
  | otherwise = 0
  where
    go n
      | rep == take w (drop (w * n) p) = go (n + 1)
      | otherwise = n


snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

-- Fra https://stackoverflow.com/a/27733778
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

-- O(n*m)
windows :: Int -> [a] -> [[a]]
windows s = transpose' . take s . tails