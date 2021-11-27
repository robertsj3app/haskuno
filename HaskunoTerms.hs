-- FILE CONTAINING NECESSARY DATA TYPES FOR HASKUNO

-- The four possible colors on an Uno card.
data Color = Red | Yellow | Blue | Green

-- The possible card types a player can play.
data Card = Base Int Color | Skip Color | DrawTwo Color | Wild | DrawFourWild | Reverse Color

-- When a reverse card is played, the turn direction changes.
data TurnDirection = CWise | CCWise

-- A deck is a pile (list) of cards.
type Deck = [Card]

-- A player's current hand is a group (list) of cards.
type Hand = [Card]

-- The discard/played pile is a pile (list) of cards.
type DiscardPile = [Card]