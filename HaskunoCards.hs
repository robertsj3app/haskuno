import HaskunoTerms

-- If a player plays a base card:
-- Add this card to the discard pile
-- Subtract one from current player's hand size

-- CONSTRAINT: Base card can only be played if:
-- current card matches the base card color or
-- current card matches the base card value

baseCard :: StateRecord -> StateRecord
baseCard = undefined

-- If a player plays a skip card:
-- Add this card to discard pile
-- Subtract one from current player's hand size
-- The current player is allowed a consecutive turn

-- CONSTRAINT: Skip Card can only be played if:
-- current card matches the skip card color or
-- current card is a skip card

skipCard :: StateRecord -> StateRecord
skipCard currentPlayer = currentPlayer

-- If a player plays a draw two card:
-- Add this card to the discard pile
-- Subtract one from the current player's hand size
-- The other player must draw two cards
-- Add two to the other player's hand size
-- The current player is allowed a consecutive turn
-- If next player has a draw two, they are allowed to stack it with the previous

-- CONSTRAINT: Draw Two card can only be played if:
-- current card matches the draw two color or
-- current card is a draw two card

drawTwoCard :: StateRecord -> StateRecord
drawTwoCard = undefined

-- If a player plays a wild card:
-- Current player chooses a new current color
-- Add this card to the discard pile
-- Subtract one from the current player's hand size

-- NO CONSTRAINTS

wildCard :: StateRecord -> StateRecord
wildCard = undefined

-- If a player plays a draw four wild card:
-- Current player chooses a new current color
-- Add this card to the discard pile
-- Subtract one from the current player's hand size
-- Next player must draw four cards from the deck
-- Add four to the next player's hand size
-- Current player is allowed a consecutive turn

-- NO CONSTRAINTS

drawFourWildCard :: StateRecord -> StateRecord
drawFourWildCard = undefined

-- If a player plays a reverse card:
-- Add this card to discard pile
-- Subtract one from current player's hand size
-- The current player is allowed a consecutive turn

-- CONSTRAINT: Reverse Card can only be played if:
-- current card matches the reverse card color or
-- current card is a reverse card

reverseCard :: StateRecord -> StateRecord
reverseCard currentPlayer = currentPlayer
