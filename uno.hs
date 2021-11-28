module Uno where
    import DeckFuncs

main :: IO ()
{--
    PSEUDOCODE
    Prompt number of players
    accept input for number of players
    create n players, (name string, hand) tuples, and add to list of players (possibly record? use for gamestate) 
    create deck
    use System.Random to generate random seed for shuffle
    shuffle deck
    call drawcard 7 times for each player to create starting hands
    move top card of deck into discard pile
    set current player variable to first player in list of players
    loop:
        print {current player}'s turn!
        print top card of discard pile
        print each card in player's hand, with index number next to it, eg 1) Blue Skip
        if player has no playable card
            notify player
            draw card
            inform player of drawn card
        else
            accept input from player to choose card to play
            if chosen card is valid
                resolve any special effects
                place card on discard pile
            else
                display error for invalid card
                jump to loop
        if player hand size is 1
            print {player name} has UNO
        else if player hand size is 0
            print {player name} has won!
            terminate
        else
            set current player to next in turn direction (index into player list either ++ or --)
        jump to loop

--}