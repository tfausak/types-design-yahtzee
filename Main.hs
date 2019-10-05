type DiceChoice = [ Bool ]
type DiceVals   = [ Integer ]
type DiceTurn  = (DiceChoice, DiceVals)

pop :: DiceTurn
    -> Maybe ((Bool, Integer), DiceTurn)
pop ([], []) = Nothing
pop (chosen:choices, v:vs) = Just ((chosen, v), (choices, vs))
pop (_:_, []) = error "Invariant violated: missing val"
pop ([], _:_) = error "Invariant violated: missing choice"

allRolls :: DiceTurn
         -> Integer
         -> [ (DiceVals, Integer) ]
allRolls t n = [ (vals, n-1) | vals <- allRollsNoN t ]

allRollsNoN :: DiceTurn -> [ DiceVals ]
allRollsNoN t = case pop t of
  Nothing -> [ [] ]
  Just ((chosen, v), t) ->
    allRollsNoN t >>=
        \roll -> [ d:roll | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]

main =
  let diceChoices = [ False, True, True, False, False ]
      diceVals = [ 6, 4, 4, 3, 1 ]
  in mapM_ print $ allRolls (diceChoices, diceVals) 2
