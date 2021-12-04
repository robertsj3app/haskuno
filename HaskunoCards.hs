module HaskunoCards where
import HaskunoTerms

cardDeck :: Deck
cardDeck = [Base 0 Red, Base 1 Red, Base 2 Red, Base 3 Red, Base 4 Red, Base 5 Red, Base 6 Red, Base 7 Red, Base 8 Red, Base 9 Red,
            Base 1 Red, Base 2 Red, Base 3 Red, Base 4 Red, Base 5 Red, Base 6 Red, Base 7 Red, Base 8 Red, Base 9 Red,
            Skip Red, Skip Red, Reverse Red, Reverse Red, DrawTwo Red, DrawTwo Red,
            Base 0 Blue, Base 1 Blue, Base 2 Blue, Base 3 Blue, Base 4 Blue, Base 5 Blue, Base 6 Blue, Base 7 Blue, Base 8 Blue, Base 9 Blue,
            Base 1 Blue, Base 2 Blue, Base 3 Blue, Base 4 Blue, Base 5 Blue, Base 6 Blue, Base 7 Blue, Base 8 Blue, Base 9 Blue,
            Skip Blue, Skip Blue, Reverse Blue, Reverse Blue, DrawTwo Blue, DrawTwo Blue,
            Base 0 Yellow, Base 1 Yellow, Base 2 Yellow, Base 3 Yellow, Base 4 Yellow, Base 5 Yellow, Base 6 Yellow, Base 7 Yellow, Base 8 Yellow, Base 9 Yellow,
            Base 1 Yellow, Base 2 Yellow, Base 3 Yellow, Base 4 Yellow, Base 5 Yellow, Base 6 Yellow, Base 7 Yellow, Base 8 Yellow, Base 9 Yellow,
            Skip Yellow, Skip Yellow, Reverse Yellow, Reverse Yellow, DrawTwo Yellow, DrawTwo Yellow,
            Base 0 Green, Base 1 Green, Base 2 Green, Base 3 Green, Base 4 Green, Base 5 Green, Base 6 Green, Base 7 Green, Base 8 Green, Base 9 Green,
            Base 1 Green, Base 2 Green, Base 3 Green, Base 4 Green, Base 5 Green, Base 6 Green, Base 7 Green, Base 8 Green, Base 9 Green,
            Skip Green, Skip Green, Reverse Green, Reverse Green, DrawTwo Green, DrawTwo Green,
            Wild Undefined, Wild Undefined, Wild Undefined, Wild Undefined,
            DrawFourWild Undefined, DrawFourWild Undefined, DrawFourWild Undefined, DrawFourWild Undefined]

numCards :: Deck -> Integer
numCards d = fromIntegral (length d)