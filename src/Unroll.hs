module Unroll (unroll, unrollLines) where
import KSSyntax  (Pattern(..), Instructions, Instruction(..), Course(..), Line(..), LineNums)
import Knittels (Knittel(..))
import Control.Monad (join)
import Data.Ord (Ordering)
import Data.List (sortBy)
import Data.Function (on)

------------ Unroll ------------
unroll :: Pattern -> Pattern
unroll (Pattern p) = Pattern $ map u p
  where
    u (Course r is c) = Course r (unrollInstructions is) c
    u c = c

unrollLines :: Pattern -> Pattern
unrollLines (Pattern p) = Pattern $ sortBy (compare `on` lineNums) (join $ map u p)
  where
    u (Course l is c) = map (\r -> Course r is c) (allLines l)
    -- TODO: also unroll multiline repeats; need lookup function to do this.
    u c = [c]
    allLines (Row ln s) = map (\l -> Row [l] s) ln
    allLines (Round ln s) = map (\l -> Round [l] s) ln

lineNums :: Course -> LineNums
lineNums (Course r _ _) = lines r
  where lines (Row ln _) = ln
        lines (Round ln _) = ln

-- compareCourses :: Course -> Course -> Ordering
-- compareCourses (Course r is cm) (Course r2 is2 cm2) =  compare r r2

unrollInstructions :: Instructions -> Instructions
unrollInstructions is = is >>= unrollInstruction

unrollInstruction :: Instruction -> Instructions
unrollInstruction (Knittel k) = unrollKnittel k
unrollInstruction (Rep r t) = join $ replicate t (unrollInstructions r)
unrollInstruction (Loop l e) = [Loop (unrollInstructions l) e]


unrollKnittel :: Knittel -> Instructions
unrollKnittel (KInst kn r a t) = replicate r (Knittel (KInst kn 1 a t))
