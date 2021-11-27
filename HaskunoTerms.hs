module HaskunoTerms where

-- FILE CONTAINING NECESSARY DATA TYPES FOR HASKUNO

-- The four possible colors on an Uno card.
data Color = Red | Yellow | Blue | Green

-- The possible card types a player can play.
data Card = Base Integer Color | Skip Color | DrawTwo Color | Wild | DrawFourWild | Reverse Color

-- A deck is a pile (list) of cards.
type Deck = [Card]

-- A player's current hand is a group (list) of cards.
type Hand = [Card]

-- The discard/played pile is a pile (list) of cards.
type DiscardPile = [Card]

-- Record to keep track of various things in the game. (Recommended for use by Dr. Polonsky)
data StateRecord = StateRecord
  { currentPlayer :: Integer
  }