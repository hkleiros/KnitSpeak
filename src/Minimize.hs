module Minimize (minimize, unroll, unrollRows) where
import KSSyntax (Instructions, Instruction (..), Pattern(..), Course (Course), Line (..))
import Data.Function (on)
import Data.Foldable (maximumBy)
import Control.Applicative ( ZipList(ZipList, getZipList) )
import Data.List (tails)
import Control.Monad (join)
import Knittels (Knittel (..))


minimize :: Pattern -> Pattern
minimize (Pattern p) = Pattern $ map cm p
    where cm (Course r is c) = Course r  (m is) c
          cm c = c

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
unroll (Pattern p) =  Pattern $ map u p 
        --join . map u
          where u (Course r is c) = Course r (unrollInstructions is) c
                u c = c

unrollRows :: Pattern -> Pattern 
unrollRows (Pattern p) = Pattern $ join $ map u p
          where u (Course l is c) | len l == 1 = [Course l is c]
                                  | otherwise = map (\r -> Course r is c) (allLines l)
                -- TODO: also unroll multiline repeats; need lookup function to do this. 
                u c = [c]
                len (Row ln _)        = length ln
                len (Round ln _)      = length ln
                allLines (Row ln s)   = map (\l -> Row [l] s) ln 
                allLines (Round ln s) = map (\l -> Round [l] s) ln



unrollInstructions :: Instructions -> Instructions
unrollInstructions ((Knittel k) : is) = unrollKnittel k ++ unrollInstructions is
unrollInstructions ((Rep r t) : is)   = join (replicate t (unrollInstructions r)) ++ is
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