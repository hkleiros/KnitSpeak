module Minimize (
  minimize, 
  minimize2, 
  possibiblities
  ) where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Monad (join)
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Data.List (tails, nub)
import KSSyntax (Course (..), Instruction (..), Instructions, Pattern (..))
import Knittels (Knittel (..))
import Unroll (unroll)
import Utils (patternLength)

minimize :: Pattern -> Pattern
minimize p =
  let minimized = mini (unroll p) in
      if patternLength minimized > patternLength p
        then p
      else minimized
  where
    mini (Pattern pa) = Pattern (map cm pa)
    cm (Course r is c) = Course r (ma is) c
    cm e = e

-- The real minimzation function
minimize2 :: Pattern -> Pattern
minimize2 = mini . unroll
  where
    mini (Pattern p) = Pattern (map cm p)
    cm (Course r is c) = Course r (m is) c
    cm e = e

possibiblities :: Pattern -> [Instructions]
possibiblities = nub . mini . unroll
    where
      mini (Pattern p) = join $ map cm p
      cm (Course _ is _) = m' is
      cm _ = []

{- NOTE: can minimize patterns that don't contain multiline repeats or comments.
verticallyMinimize :: Pattern -> Pattern
verticallyMinimize (Pattern p) = Pattern $ sortBy (\(Course r is cm) (Course r2 is2 cm2) -> rowN r r2) (c (sortBy (\(Course _ is cm) (Course r2 is2 cm2) -> compare is is2) p))

    where c :: [Course] -> [Course]
          c ((Course r is cm) : ((Course r2 is2 cm2): iss))
              | (is == is2) && rt r r2 = c ((Course (cr r r2) is "") : iss)
          c (i : is) = i : (c is)
          c [] = []
          rt (Row _ s) (Row _ s2) | s == s2 = True
          rt (Round _ s) (Round _ s2) | s == s2 = True
          rt _ _ = False
          cr (Row rn s) (Row rn2 s2) = Row (rn ++ rn2) s
          rowN (Row rn s) (Round rn2 s2) = compare (head rn)  (head rn2)
          rowN (Round rn s) (Row rn2 s2) =   compare (head rn) (head rn2)
          rowN (Row rn s) (Row rn2 s2) =     compare (head rn) (head rn2)
          rowN (Round rn s) (Round rn2 s2) = compare (head rn) (head rn2)
-}

ma :: Instructions -> Instructions
ma [] = []
ma is =
    -- Get all repeating substructures
    case allRepetitions is of
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
m is = minimumBy (compare `on` len) (m' is)
  where len :: Instructions -> Integer
        len [] = 0
        len (Knittel _ : xs) = 1 + len xs
        len (Rep  si _ : xs) = len si + len xs
        len (Loop si _ : xs) = len si + len xs

m' :: Instructions -> [Instructions]
m' [] = []
m' is =
    -- Get all repeating substructures
    case allRepetitions is of
        [] -> [is]
        r  -> r >>= allOptions 
        
    --- Examine the substructures structure, and chose the case accordingly 
    where allOptions (structure, times, index, len) =
            case structure of
            -- Call m' again on the list and repeating structure
            [Knittel (KInst k t a tbl)] ->
                  m' (join [take index is, [Knittel (KInst k (times * t) a tbl)], drop (index + (len * times)) is])
            p ->  m' $ join [take index is, [Rep (m p) times], drop (index + (len * times)) is]


allRepetitions :: (Eq a) => [a] -> [([a], Int, Int, Int)] -- Repeating sublist, times it repeats, index of sublist, size of sublist
allRepetitions [] = [([], 1, 0, 0)]
allRepetitions l =
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

-- From https://stackoverflow.com/a/27733778
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows s = transpose' . take s . tails
-- windows s = foldr (zipWith (:)) (repeat []) . take s . tails