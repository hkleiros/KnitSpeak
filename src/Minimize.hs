module Minimize (minimize, m, sw, ft) where
import KSSyntax (Instructions, Instruction (..), Pattern, Line (Course))

import Data.Function (on)
import Data.Foldable (maximumBy)
import Control.Applicative ( ZipList(ZipList, getZipList) )
import Data.List (tails, genericReplicate)
import Control.Monad (join)


minimize :: Pattern -> Pattern
minimize = map go
    where go (Course r is) = Course r $ m is


m :: Instructions -> Instructions
m [] = []
m is =
    case sw is of
        [] -> is
        r -> let (s, t, i, l) = maximumBy (compare `on` snd4) r
             in m (take i is) ++ [Rep (m s) t] ++ m (drop (i + (l * t)) is)

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x


sw :: Eq a => [a] -> [([a], Int, Int, Int)]
sw [] = [([], 1, 0, 0)]
sw is  =
        filter (\x -> snd4 x > 1) $
            join $ map (\x ->
                zipWith (\ w i -> (w, ft w x (drop i is), i, x)) (windows x is) [0, 1 ..]
                )
            [1 .. length is `div` 2 ]



ft :: Eq a => [a] -> Int -> [a] -> Int
ft [] _ [] = 0
ft rep w p = if rep == take w p
            then go 1
            else 0
    where go n =
            if rep == take w (drop (w * n) p) then go (n+1) else n


transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

-- O(n*m)
windows :: Int -> [a] -> [[a]]
windows s = transpose' . take s . tails
