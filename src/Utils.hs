module Utils  where 
import KSSyntax
import Knittels


removeComments :: Pattern -> Pattern
removeComments (Pattern p)  = Pattern $ filter cn p
        where   cn (Comment _) = False
                cn _ = True

patternLength :: Pattern -> Integer
patternLength (Pattern p) = sum $ map courseLength p

courseLengths :: Pattern -> [Integer]
courseLengths (Pattern p) = map courseLength p 

courseLength :: Course -> Integer 
courseLength (Course _ is _) = instructionsLen is
courseLength (MultilineRepeat {}) = 0
courseLength (Comment _) = 0

instructionsLen :: Instructions -> Integer
instructionsLen [] = 0
instructionsLen (Knittel _ : xs) = 1 + instructionsLen xs
instructionsLen (Rep  i _ : xs) = instructionsLen i + instructionsLen xs
instructionsLen (Loop i _ : xs) = instructionsLen i + instructionsLen xs

stitchLength :: Instruction -> Int
stitchLength (Rep is _) = max (sum (map stitchLength is)) 0
stitchLength (Loop is _) = max (sum (map stitchLength is)) 0
stitchLength (Knittel (KInst _ _ (KArity n) _)) = max n 0