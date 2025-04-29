module Flip (flipPattern) where
import KSSyntax (Course (..), Instruction (..), Instructions, Pattern (..))
import Utils    (stitchLength)
import Mirror   (mirrorKnittel)


flipPattern :: Pattern -> Pattern
flipPattern (Pattern p) = Pattern $ map inv p
  where
    inv (Course l is c) = Course l (flipInstructions (is, 0)) c
    inv c = c

-- Flip functions
flipInstructions :: (Instructions, Int) -> Instructions
flipInstructions (i : is, len) = flipInstruction (i, len) : flipInstructions (is, len + stitchLength i)
flipInstructions ([], _) = []

flipInstruction :: (Instruction, Int) -> Instruction
flipInstruction (Rep is times, _) = Rep (flipInstructions (is, 0)) times
flipInstruction (Loop is _1, len) = Loop (flipInstructions (is, 0)) len
flipInstruction (Knittel k, _) = Knittel (mirrorKnittel k)
