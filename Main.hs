{-# LANGUAGE LambdaCase #-}

type DiceVals   = [ Integer ]
data DiceChoice = Keep Integer | Reroll

allRollsBetter :: [DiceChoice] -> [ DiceVals ]
allRollsBetter = mapM $ \case
  Reroll -> [ 1..6 ]
  Keep v -> [v]

main =
  let diceVals = [ Reroll, Keep 4, Keep 4, Reroll, Reroll ]
  in mapM_ print $ allRollsBetter diceVals
