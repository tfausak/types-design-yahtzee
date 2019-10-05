import Data.List (uncons)

type DiceVals   = [ Integer ]
type DiceTurn  = [(Bool, Integer)]

allRolls :: DiceTurn
         -> Integer
         -> [ (DiceVals, Integer) ]
allRolls t n = [ (vals, n-1) | vals <- allRollsNoN t ]

allRollsNoN :: DiceTurn -> [ DiceVals ]
allRollsNoN t = case uncons t of
  Nothing -> [ [] ]
  Just ((chosen, v), t) ->
    allRollsNoN t >>=
        \roll -> [ d:roll | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]

main =
  let diceChoices = [ False, True, True, False, False ]
      diceVals = [ 6, 4, 4, 3, 1 ]
  in mapM_ print $ allRolls (zip diceChoices diceVals) 2
