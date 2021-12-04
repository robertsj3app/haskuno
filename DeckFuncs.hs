module DeckFuncs where
import HaskunoTerms
import System.Random
import Data.Array.IO
import Control.Monad

drawCard :: GameState -> Integer -> Integer -> GameState -- remove top card from deck and add to hand in accordance to GameState record
drawCard sr who howmany = StateRecord {currentPlayer = currentPlayer sr, playerList = drawHelper (playerList sr) (deck sr) who howmany, turnDirection = turnDirection sr, deck = drop (fromIntegral howmany) (deck sr), discardPile = discardPile sr}

drawHelper :: [Player] -> Deck -> Integer -> Integer -> [Player] -- remove top card from deck and add to hand
drawHelper ps d who howmany = take (fromIntegral who) ps ++ (fst (ps !! fromIntegral who), snd (ps !! fromIntegral who) ++ take (fromIntegral howmany) d) : drop (fromIntegral (who + 1)) ps

playCard :: GameState -> Integer -> Integer -> GameState -- remove card at index and add to discard pile in accordance to GameState record
playCard sr who which = StateRecord {currentPlayer = currentPlayer sr, playerList = playHelper (playerList sr) who which, turnDirection = turnDirection sr, deck = deck sr, discardPile = (snd(playerList sr !! fromIntegral who) !! fromIntegral which) : discardPile sr}

playHelper :: [Player] -> Integer -> Integer -> [Player] -- removes card from player's hand using dropCardAt
playHelper ps who which = take (fromIntegral who) ps ++ (fst (ps !! fromIntegral who), dropCardAt which (snd (ps !! fromIntegral who))) : drop (fromIntegral (who + 1)) ps

dropCardAt :: Integer -> Hand -> Hand -- determines card to remove from hand via index
dropCardAt n h = take (fromIntegral n) h ++ drop (fromIntegral n + 1) h

updateDeck :: GameState -> Deck -> GameState -- shuffles the deck in accordance to GameState record
updateDeck sr d = StateRecord {currentPlayer = currentPlayer sr, playerList = playerList sr, turnDirection = turnDirection sr, deck = d, discardPile = discardPile sr}

updateDiscardPile :: GameState -> Deck -> GameState -- shuffles the deck in accordance to GameState record
updateDiscardPile sr dp = StateRecord {currentPlayer = currentPlayer sr, playerList = playerList sr, turnDirection = turnDirection sr, deck = deck sr, discardPile = dp}

shuffle :: Deck -> IO Deck
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

getColor :: Card -> Color
getColor (Base x y) = y
getColor (Skip x) = x
getColor (DrawTwo x) = x
getColor (Wild x) = x
getColor (DrawFourWild x) = x
getColor (Reverse x) = x

getType :: Card -> String
getType (Base x _) = "Number"
getType (Skip _) = "Skip"
getType (DrawTwo _) = "DrawTwo"
getType (Wild _) = "Wild"
getType (DrawFourWild _) = "DrawFourWild"
getType (Reverse _) = "Reverse"

sameColor :: Card -> Card -> Bool -- check if two cards have the same color
sameColor x y = if getColor x == Undefined || getColor y == Undefined then True else getColor x == getColor y

sameValue :: Card -> Card -> Bool -- check if two cards have the same value (either card type or number)
sameValue (Base x _) (Base y _) = x == y
sameValue (Skip _) (Skip _) = True
sameValue (DrawTwo _) (DrawTwo _) = True
sameValue (Wild _) (Wild _) = True
sameValue (DrawFourWild _) (DrawFourWild _) = True
sameValue (Reverse _) (Reverse _) = True
sameValue _ _ = False 

validSelection :: Card -> DiscardPile -> Bool
validSelection x [] = True
validSelection x (y:ys) = sameColor x y || sameValue x y

clearWilds :: DiscardPile -> DiscardPile
clearWilds [] = []
clearWilds (x:xs) | getType x == "Wild" = (Wild Undefined) : clearWilds xs
                  | getType x == "DrawFourWild" = (DrawFourWild Undefined) : clearWilds xs
                  | otherwise = x : clearWilds xs

filterFirstOccurrence :: (a -> Bool) -> [a] -> [a] -- remove first occurrence of list item that satisfies function f
filterFirstOccurrence f [] = []
filterFirstOccurrence f (x:xs) = if f x then xs else x : filterFirstOccurrence f xs