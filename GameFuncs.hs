module GameFuncs where

import HaskunoTerms

addPlayer :: GameState -> String -> GameState -- adds player to player list in accordance to GameState record
addPlayer sr name = StateRecord {currentPlayer = currentPlayer sr, playerList = (playerList sr) ++ [(name, [])], turnDirection = turnDirection sr, deck = deck sr, discardPile = discardPile sr}

getNameFromIndex :: GameState -> Integer -> String
getNameFromIndex sr i = fst (playerList sr !! fromIntegral i)

getNextPlayer :: GameState -> GameState -- gets next player in accordance to GameState record
getNextPlayer sr = StateRecord {currentPlayer = fixOverhang ((getTurnDirection sr) (currentPlayer sr) 1) (playerList sr), playerList = playerList sr, turnDirection = turnDirection sr, deck = deck sr, discardPile = discardPile sr}

fixOverhang :: Integer -> [Player] -> Integer -- loops to beginning or ending of player list if necessary
fixOverhang x ps
  | x < 0 = (fromIntegral (length ps)) - 1
  | x > (fromIntegral (length ps)) - 1 = 0
  | otherwise = x

getTurnDirection :: GameState -> (Integer -> Integer -> Integer) -- returns the turn direction from the GameState record
getTurnDirection sr = if turnDirection sr == CWise then (+) else (-)

skipNextPlayer :: GameState -> GameState -- skips the next player, taking turn direction into account
skipNextPlayer sr = getNextPlayer (getNextPlayer sr)

switchTurnDirection :: GameState -> GameState -- switches turn direction in accordance to GameState record
switchTurnDirection sr = StateRecord {currentPlayer = currentPlayer sr, playerList = playerList sr, turnDirection = if turnDirection sr == CWise then CCWise else CWise, deck = deck sr, discardPile = discardPile sr}
