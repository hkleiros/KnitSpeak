module Minimize (minimize, minimize2, unroll, unrollRows) where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Monad (join)
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Data.List (tails)
import KSSyntax (Course (Course), Instruction (..), Instructions, Line (..), Pattern (..))
import Knittels (Knittel (..))
import Utils (patternLength)
import qualified Data.Memocombinators as Memo

minimize :: Pattern -> Pattern
minimize = mini . unroll 
{- minimize p = 
  let min = mini (unroll p) in 
      if patternLength min > patternLength p
        then p 
      else min -}

  where
    mini (Pattern p) = Pattern (map cm p) 
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
          let (s, t, i, l) = maximumBy (compare `on` snd4)(r) --- Choose the substructure with largest number of repeats 
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

------------ Unroll ------------
unroll :: Pattern -> Pattern
unroll (Pattern p) = Pattern $ map u p
  where
    u (Course r is c) = Course r (unrollInstructions is) c
    u c = c

unrollRows :: Pattern -> Pattern
unrollRows (Pattern p) = Pattern $ join $ map u p
  where
    u (Course l is c)
      | len l == 1 = [Course l is c]
      | otherwise = map (\r -> Course r is c) (allLines l)
    -- TODO: also unroll multiline repeats; need lookup function to do this.
    u c = [c]
    len (Row ln _) = length ln
    len (Round ln _) = length ln
    allLines (Row ln s) = map (\l -> Row [l] s) ln
    allLines (Round ln s) = map (\l -> Round [l] s) ln

unrollInstructions :: Instructions -> Instructions
unrollInstructions ((Knittel k) : is) = unrollKnittel k ++ unrollInstructions is
unrollInstructions ((Rep r t) : is)   = join (replicate t (unrollInstructions r)) ++  unrollInstructions is
unrollInstructions (l : is)           = l : unrollInstructions is -- NOTE: dont unroll Loops
unrollInstructions [] = []

unrollKnittel :: Knittel -> Instructions
unrollKnittel (KInst kn r a t) = replicate r (Knittel (KInst kn 1 a t))

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

-- Fra https://stackoverflow.com/a/27733778
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

-- O(n*m)
windows :: Int -> [a] -> [[a]]
windows s = transpose' . take s . tails