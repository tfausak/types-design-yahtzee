type DiceVals   = [ Integer ]
type DiceTurn  = [(Bool, Integer)]

allRolls :: DiceTurn
         -> Integer
         -> [ (DiceVals, Integer) ]
allRolls t n = [ (vals, n-1) | vals <- allRollsNoN t ]

allRollsNoN =
    mapM (\(chosen, v) -> if chosen then [v] else [ 1..6 ])

main =
  let diceChoices = [ False, True, True, False, False ]
      diceVals = [ 6, 4, 4, 3, 1 ]
  in mapM_ print $ allRolls (zip diceChoices diceVals) 2
