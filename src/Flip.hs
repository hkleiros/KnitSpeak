module Flip (flipPattern) where
import KSSyntax (Course (..), Instruction (..), Instructions, Pattern (..))
import Mirror   (mirrorKnittel)



flipPattern :: Pattern -> Pattern
flipPattern (Pattern p) = Pattern $ map inv p
  where
    inv (Course l is c) = Course l (flipInstructions is) c
    inv c = c

-- Flip functions
flipInstructions :: Instructions -> Instructions
flipInstructions = map flipInstruction

flipInstruction :: Instruction -> Instruction
flipInstruction (Rep is times) = Rep (flipInstructions is) times
flipInstruction (Loop is e) = Loop (flipInstructions is) e
flipInstruction (Knittel k) = Knittel (mirrorKnittel k)
