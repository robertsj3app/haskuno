module DeckFuncs where
import HaskunoTerms

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

shuffleDeck :: GameState -> [Integer] -> GameState -- shuffles the deck in accordance to GameState record
shuffleDeck sr seed = StateRecord {currentPlayer = currentPlayer sr, playerList = playerList sr, turnDirection = turnDirection sr, deck = shuffle (deck sr) seed, discardPile = discardPile sr}

shuffle :: Deck -> [Integer] -> Deck -- take seed (list of int, generated randomly at runtime) and uses that seed to drive shufflehelper; not sure if this is sufficiently random yet
shuffle xs [] = xs
shuffle xs (i:is) = shuffle (shuffleHelper xs i) is

shuffleHelper :: Deck -> Integer -> Deck -- shift card at position i to head of deck
shuffleHelper xs i = p : filterFirstOccurrence (== p) xs where p = (xs !! fromIntegral i)

swapElementsAt :: Integer -> Integer -> [a] -> [a]
swapElementsAt i j xs = let elemI = xs !! fromIntegral i
                            elemJ = xs !! fromIntegral j
                            left = take (fromIntegral i) xs
                            middle = take (fromIntegral j - fromIntegral i - 1) (drop (fromIntegral i + 1) xs)
                            right = drop (fromIntegral j + 1) xs
                        in  left ++ [elemJ] ++ middle ++ [elemI] ++ right

getColor :: Card -> Color
getColor (Base x y) = y
getColor (Skip x) = x
getColor (DrawTwo x) = x
getColor (Wild x) = x
getColor (DrawFourWild x) = x
getColor (Reverse x) = x

sameColor :: Card -> Card -> Bool -- check if two cards have the same color
sameColor x y = if getColor x == Undefined then True else getColor x == getColor y

sameValue :: Card -> Card -> Bool -- check if two cards have the same value (either card type or number)
sameValue (Base x _) (Base y _) = x == y
sameValue (Skip _) (Skip _) = True
sameValue (DrawTwo _) (DrawTwo _) = True
sameValue (Wild _) (Wild _) = True
sameValue (DrawFourWild _) (DrawFourWild _) = True
sameValue (Reverse _) (Reverse _) = True
sameValue _ _ = False 

validSelection :: Card -> DiscardPile -> Bool
validSelection x (y:ys) = sameColor x y || sameValue x y

filterFirstOccurrence :: (a -> Bool) -> [a] -> [a] -- remove first occurrence of list item that satisfies function f
filterFirstOccurrence f [] = []
filterFirstOccurrence f (x:xs) = if f x then xs else x : filterFirstOccurrence f xs