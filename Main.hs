type DiceChoice = [ Bool ]
type DiceVals   = [ Integer ]

pop :: (DiceChoice, DiceVals)
    -> Maybe ((Bool, Integer), (DiceChoice, DiceVals))
pop ([], []) = Nothing
pop (chosen:choices, v:vs) = Just ((chosen, v), (choices, vs))
pop (_:_, []) = error "Invariant violated: missing val"
pop ([], _:_) = error "Invariant violated: missing choice"

allRolls :: (DiceChoice, DiceVals)
         -> Integer
         -> [ (DiceVals, Integer) ]
allRolls (choices, vs) n = case pop (choices, vs) of
  Nothing -> [ ([], n-1) ]
  Just ((chosen, v), (choices, vs)) ->
    allRolls (choices, vs) (error "Didn't expect to use") >>=
        \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]

main =
  let diceChoices = [ False, True, True, False, False ]
      diceVals = [ 6, 4, 4, 3, 1 ]
  in mapM_ print $ allRolls (diceChoices, diceVals) 2
