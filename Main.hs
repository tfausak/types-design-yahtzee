{-# LANGUAGE LambdaCase #-}

type DiceVals   = [ Integer ]
type DiceTurn  = [(Bool, Integer)]

allRolls :: DiceTurn
         -> Integer
         -> [ (DiceVals, Integer) ]
allRolls t n = [ (vals, n-1) | vals <- allRollsNoN t ]

allRollsNoN = allRollsBetter . map fromTurn

data DiceChoice = Keep Integer | Reroll

fromTurn :: (Bool, Integer) -> DiceChoice
fromTurn (chosen, v) = if chosen then Keep v else Reroll

allRollsBetter :: [DiceChoice] -> [ DiceVals ]
allRollsBetter = mapM $ \case
  Reroll -> [ 1..6 ]
  Keep v -> [v]

main =
  let diceChoices = [ False, True, True, False, False ]
      diceVals = [ 6, 4, 4, 3, 1 ]
  in mapM_ print $ allRolls (zip diceChoices diceVals) 2
