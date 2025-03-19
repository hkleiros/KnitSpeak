module Minimize (minimize, unroll) where
import KSSyntax (Instructions, Instruction (..), Pattern, Line (Course))

import Data.Function (on)
import Data.Foldable (maximumBy)
import Control.Applicative ( ZipList(ZipList, getZipList) )
import Data.List (tails)
import Control.Monad (join)
import Knittels (Knittel (..), KName (..))


minimize :: Pattern -> Pattern
minimize = map (\(Course r is) -> Course r $ m is)


m :: Instructions -> Instructions
m [] = []
m is =
    case slidingWindow is of
        [] -> is
        r -> let (s, t, i, l) = maximumBy (compare `on` snd4) r
             in m (take i is) ++ [Rep (m s) t] ++ m (drop (i + (l * t)) is)


slidingWindow :: Eq a => [a] -> [([a], Int, Int, Int)]
slidingWindow [] = [([], 1, 0, 0)]
slidingWindow is  =
        filter (\x -> snd4 x > 1) $
            [1 .. length is `div` 2 ] >>=
              (\x ->
                zipWith (\ w i -> (w, numberOfTimes w x (drop i is), i, x)) (windows x is) [0, 1 ..]
              )


numberOfTimes :: Eq a => [a] -> Int -> [a] -> Int
numberOfTimes [] _ [] = 0
numberOfTimes rep w p | rep == take w p =  go 1
                      | otherwise =  0

           where go n | rep == take w (drop (w * n) p) = go (n + 1)
                      | otherwise = n


------------ Unroll ------------
unroll :: Pattern -> Pattern
unroll = map (\(Course r is) -> Course r $ unrollInstructions is)

unrollInstructions :: Instructions -> Instructions
unrollInstructions ((Rep r t) : is)   =  join (replicate t (unrollInstructions r)) ++ is
unrollInstructions ((Knittel k) : is) = unrollKnittel k ++ unrollInstructions is
unrollInstructions (l : is)           = l : unrollInstructions is -- NOTE: loops
unrollInstructions [] = []

unrollKnittel :: Knittel -> Instructions
unrollKnittel (KInst kn a t) = let (k, n) = size kn in replicate n (Knittel (KInst k a t))


size :: KName -> (KName, Int)
size (K n)         = (K 1, n)
size (P n)         = (P 1, n)
size (Slip n p)    = (Slip 1 p, n)
--size (BO (Just n)) = (BO Nothing, n)
--size (CO (Just n)) = (CO Nothing, n)
size k = (k, 1)


snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

-- Fra https://stackoverflow.com/a/27733778
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

-- O(n*m)
windows :: Int -> [a] -> [[a]]
windows s = transpose' . take s . tails