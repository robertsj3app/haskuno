module DeckFuncs where
    import HaskunoTerms

drawCard :: (Deck, Hand) -> (Deck, Hand) -- remove card from deck and add to hand
drawCard (x, y) = (tail x, head x : y)

shuffleHelper :: Deck -> Integer -> Deck -- shift card at position i to head of deck
shuffleHelper xs i = p : filter (/= p) xs where p = (xs !! fromIntegral i)

shuffle :: Deck -> [Integer] -> Deck -- take seed (list of int, generated randomly at runtime) and uses that seed to drive shufflehelper; not sure if this is sufficiently random yet
shuffle xs [] = xs
shuffle xs (i:is) = shuffle (shuffleHelper xs i) is

sameColor :: Card -> Card -> Bool -- check if two cards have the same color
sameColor x y = True -- placeholder
{--
    PSEUDOCODE:
    samecolor (PATTERN MATCH COLOR) (PATTERN MATCH COLOR) = True
        repeat above for each color choice
--}

sameNumber :: Card -> Card -> Bool -- check if two cards have the same number
sameNumber (Base x _) (Base y _) = x == y
sameNumber _ _ = False 

validSelection :: Card -> DiscardPile -> Bool
validSelection x (y:ys) = sameColor x y || sameNumber x y

filterFirstOccurrence :: (a -> Bool) -> [a] -> [a] -- remove first occurrence of list item that satisfies function f
filterFirstOccurrence f [] = []
filterFirstOccurrence f (x:xs) = if f x then xs else x : filterFirstOccurrence f xs

playCard :: (Hand, Card, DiscardPile) -> (Hand, DiscardPile) -- remove card from hand and add to discard pile
playCard (x,y,z) = ([], []) -- placeholder
{--
    PSEUDOCODE:
    playcard (h, c, d) = if card is elem of hand and card is valid selection
                            then filterfirstoccurence of card from hand
                                 add card to top of discard pile
--}