module DeckFuncs where
import HaskunoTerms
import System.Random
import Control.Monad (replicateM)

drawCard :: (Deck, Hand) -> (Deck, Hand) -- remove card from deck and add to hand
drawCard (x, y) = (tail x, head x : y)

shuffleHelper :: Deck -> Integer -> Deck -- shift card at position i to head of deck
shuffleHelper xs i = p : filter (/= p) xs where p = (xs !! fromIntegral i)

shuffle :: Deck -> [Integer] -> Deck -- take seed (list of int, generated randomly at runtime) and uses that seed to drive shufflehelper
shuffle xs [] = xs
shuffle xs (i:is) = shuffle (shuffleHelper xs i) is

sameColor :: Card -> Card -> Bool -- TODO: This does the opposite of what we want, wioldcards not working other way for some reason
sameColor (Base x _) (Base y _) = True
sameColor (DrawTwo _) (DrawTwo _) = True
sameColor (Skip _) (Skip _) = True
sameColor (Wild) x = True
sameColor (DrawFourWild) x = True
sameColor (Reverse _) (Reverse _) = True
sameColor _ _ = False

sameNumber :: Card -> Card -> Bool
sameNumber (Base x _) (Base y _) = x == y
sameNumber _ _ = False 

validSelection :: Card -> DiscardPile -> Bool
validSelection x (y:ys) = sameColor x y || sameNumber x y

filterFirstOccurrence :: (a -> Bool) -> [a] -> [a]
filterFirstOccurrence f [] = []
filterFirstOccurrence f (x:xs) = if f x then xs else x : filterFirstOccurrence f xs

playCard :: (Hand, Card, DiscardPile) -> (Hand, DiscardPile) -- TODO: This is filler
playCard (x, y, z) = if y == y then (x,z) else (x, z)