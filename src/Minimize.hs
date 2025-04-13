module Minimize (minimize, unroll) where
import KSSyntax (Instructions, Instruction (..), Pattern, Course (Course), Line (..), LineNums)

import Data.Function (on)
import Data.Foldable (maximumBy)
import Control.Applicative ( ZipList(ZipList, getZipList) )
import Data.List (tails, sortBy)
import Control.Monad (join)
import Knittels (Knittel (..))


minimize :: Pattern -> Pattern
minimize = map min
    where min (Course r is) = Course r  (m is)
          min c = c

{-combineCourses :: Pattern -> Pattern 
combineCourses = c sortBy (\(Course _ is) (Course r2 is2) -> compare is  is2)
    where c (i: is) = undefined
  -}
-- TODO: also combine courses with the same instructions 

m :: Instructions -> Instructions
m [] = []
m is =
    case slidingWindow is of -- Get all repeating substructures 
        [] -> is
        r  -> let (s, t, i, l) = maximumBy (compare `on` snd4) r --- Choose the one with largest number of repeats 
              in case s of
                -- Call m again on the list and repeat
                [Knittel (KInst k _ a tbl)] -> m $ take i is ++ [Knittel (KInst k t a tbl) ] ++ drop (i + (l*t)) is
                p                           -> m $ take i is ++ [Rep (m p) t] ++ drop (i + (l * t)) is

slidingWindow :: Eq a => [a] -> [([a], Int, Int, Int)]
slidingWindow [] = [([], 1, 0, 0)]
slidingWindow l  =
        filter (\x -> snd4 x > 1) $
            [1 .. length l `div` 2 ] >>=
              (\wSize ->
                zipWith
                  (\ w i -> (w, numberOfTimes w wSize (drop i l), i, wSize))
                  (windows wSize l) [0, 1 ..]
              )

numberOfTimes :: Eq a => [a] -> Int -> [a] -> Int
numberOfTimes [] _ [] = 0
numberOfTimes rep w p | rep == take w p =  go 1
                      | otherwise =  0

           where go n | rep == take w (drop (w * n) p) = go (n + 1)
                      | otherwise = n


------------ Unroll ------------
unroll :: Pattern -> Pattern
unroll =  map u --join . map u
          where u (Course r is) = Course r $ unrollInstructions is
                u c = c

          -- NOTE: implemented unrolling of rows, but we dont want to do this when minimizing. 
          --where u (Course r is) | len r == 1 = [Course r $ unrollInstructions is]
           --                     | otherwise = map (`Course` unrollInstructions is) (allLines r)
            --    u c = [c]
                len (Round is s) = length is
                len (Row is s) = length is

allLines :: Line -> [Line]
allLines (Round ls s) = map (\l -> Round [l] s) ls
allLines (Row ls s) = map (\l -> Row [l] s) ls 

unrollInstructions :: Instructions -> Instructions
unrollInstructions ((Rep r t) : is)   =  join (replicate t (unrollInstructions r)) ++ is
unrollInstructions ((Knittel k) : is) = unrollKnittel k ++ unrollInstructions is
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