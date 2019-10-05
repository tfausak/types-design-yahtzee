type DiceChoice = [ Bool ]
type DiceVals   = [ Integer ]
type DiceState = (DiceVals, Integer)

allRolls :: DiceChoice -> DiceState -> [ DiceState ]
allRolls [] ([], n) = [ ([], n-1) ]
allRolls [] (_:_, _) =
  error "Invariant violated: choices must be same length as vals"
allRolls (chosen:choices) (v:vs, n) =
    allRolls choices (vs, n-1) >>=
        \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]

main =
  let diceChoices = [ False, True, True, False, False ]
      diceVals = [ 6, 4, 4, 3, 1 ]
  in mapM_ print $ allRolls diceChoices (diceVals, 2)
