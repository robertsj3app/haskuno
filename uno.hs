module Uno where
import DeckFuncs
import GameFuncs
import HaskunoCards
import HaskunoTerms
import System.Random
import Data.Array.IO
import Control.Monad

emptyGameState :: GameState -- initial GameState structure
emptyGameState = StateRecord {currentPlayer = 0, playerList = [], turnDirection = CWise, deck = cardDeck, discardPile = []}

getPlayers :: GameState -> Integer -> IO GameState -- prompt name entry and create player reference inside gamestate i times
getPlayers gs i = do
    if i > 0 then do
        putStrLn "\nEnter your name"
        name <- getLine
        getPlayers (addPlayer gs name) (i - 1)
    else do
        return gs

checkWin :: [Player] -> String -- if player's hand size is zero, they have won
checkWin [] = ""
checkWin (x:xs) = if numCards (snd x) == 0 then fst x else checkWin xs 

checkUno :: [Player] -> String -- if player's hand size is one, they have Uno!
checkUno [] = ""
checkUno (x:xs) = if numCards (snd x) == 1 then fst x else checkUno xs 

startingHands :: GameState -> Integer -> Integer -> GameState -- draw each player seven cards from the shuffled deck
startingHands gs i n = if i < n then startingHands (drawCard gs i 7) (i + 1) n else gs

showHand :: Hand -> Integer -> IO () -- print a player's hand one card at a time
showHand (x:[]) i = do
    putStrLn ((show (i + 1)) ++ ") " ++ (show x))
    putStrLn ((show (i + 2)) ++ ") Draw from deck")
showHand (x:xs) i = do
    putStrLn ((show (i + 1)) ++ ") " ++ (show x))
    showHand xs (i+1)

main :: IO () -- entry point, create starting gamestate
main = do 
    putStrLn "\nWelcome to HaskUno!\n\nHow many people will be playing?"
    num <- getLine
    let numPlayers = (read num :: Integer)
    partialState <- getPlayers emptyGameState numPlayers
    shuffledDeck <- shuffle (deck partialState)
    let shuffledState = updateDeck partialState shuffledDeck
    let readyState = startingHands shuffledState 0 numPlayers
    gameLoop readyState

gameLoop :: GameState -> IO () -- loop where game logic happens
gameLoop gs = do
    if checkWin (playerList gs) /= "" then do
        putStrLn (checkWin (playerList gs) ++ " has no cards left in hand and has won the game!")
    else if numCards (deck gs) == 0 then do
        shuffledDeck <- shuffle (clearWilds (discardPile gs))
        let shuffledState = updateDeck gs shuffledDeck
        let clearedState = updateDiscardPile gs []
        gameLoop clearedState
    else do
        if checkUno (playerList gs) /= "" then do
            putStrLn (checkUno (playerList gs) ++ " has one card left in hand! UNO!")
        else do
            return ()
        putStrLn ("\nIt is now " ++ getNameFromIndex gs (currentPlayer gs) ++ "'s turn!\n")
        if discardPile gs == [] then do
            putStrLn ("The discard pile is empty! Any card can be played!")
        else do
            putStrLn ("The top card of the discard pile is: " ++ show (head (discardPile gs))) 
        putStrLn (getNameFromIndex gs (currentPlayer gs) ++ "'s hand is: \n")
        let hand = snd (playerList gs !! fromIntegral (currentPlayer gs))
        showHand hand 0
        putStrLn ("\nEnter the number before the parenthesis to take that action.")
        chosenAction <- getLine
        let actionNum = (read chosenAction :: Integer)
        if actionNum > (numCards hand) + 1 then do
            putStrLn "Invalid selection"
            gameLoop gs
        else do
            if actionNum > numCards hand then do
                putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " drew " ++ show (head (deck gs))) 
                let drawState = drawCard gs (currentPlayer gs) 1
                gameLoop (getNextPlayer drawState)
            else do
                let chosenCard = (hand !! fromIntegral (actionNum - 1))
                if validSelection (chosenCard) (discardPile gs) then do
                    putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " played " ++ show chosenCard)
                    if getType chosenCard == "Skip" || (getType chosenCard == "Reverse" && length (playerList gs) == 2) then do
                        putStrLn (getNameFromIndex gs (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) ++ "'s Turn was skipped!")
                        gameLoop (skipNextPlayer (playCard gs (currentPlayer gs) (actionNum - 1)))
                    else if getType chosenCard == "Reverse" then do
                        putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " reversed the turn order!")
                        gameLoop (getNextPlayer (switchTurnDirection (playCard gs (currentPlayer gs) (actionNum - 1))))
                    else if getType chosenCard == "Wild" then do
                        putStrLn ("Enter the color you wish to select!\n1) Red\n2) Blue\n3) Green\n4) Yellow")
                        choice <- getLine
                        let choiceNum = (read choice :: Integer)
                        let playedState = (playCard gs (currentPlayer gs) (actionNum - 1))
                        if choiceNum == 1 then do                            
                            let wildFixedState = (updateDiscardPile playedState (Wild Red : tail (discardPile playedState)))
                            putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " changed the color to Red!")
                            gameLoop (getNextPlayer (wildFixedState))
                        else if choiceNum == 2 then do
                            let wildFixedState = (updateDiscardPile playedState (Wild Blue : tail (discardPile playedState)))
                            putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " changed the color to Blue!")
                            gameLoop (getNextPlayer (wildFixedState))
                        else if choiceNum == 3 then do
                            let wildFixedState = (updateDiscardPile playedState (Wild Green : tail (discardPile playedState)))
                            putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " changed the color to Green!")
                            gameLoop (getNextPlayer (wildFixedState))
                        else if choiceNum == 4 then do
                            let wildFixedState = (updateDiscardPile playedState (Wild Yellow : tail (discardPile playedState)))
                            putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " changed the color to Yellow!")
                            gameLoop (getNextPlayer (wildFixedState))
                        else do
                            putStrLn ("Invalid selection")
                            gameLoop gs
                    else if getType chosenCard == "DrawTwo" then do
                        putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " forced " ++ (getNameFromIndex gs (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs))) ++ " to draw 2!")
                        putStrLn (getNameFromIndex gs (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) ++ " drew " ++ show (deck gs !! 0) ++ " and " ++ show (deck gs !! 1) ++ "!")
                        let playedState = (playCard gs (currentPlayer gs) (actionNum - 1))
                        let drawnState = (drawCard playedState (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) 2)
                        gameLoop (getNextPlayer (drawnState))
                    else if getType chosenCard == "DrawFourWild" then do
                        putStrLn ("Enter the color you wish to select!\n1) Red\n2) Blue\n3) Green\n4) Yellow")
                        choice <- getLine
                        let choiceNum = (read choice :: Integer)
                        let playedState = (playCard gs (currentPlayer gs) (actionNum - 1))
                        putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " forced " ++ (getNameFromIndex gs (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs))) ++ " to draw 4!")
                        if choiceNum == 1 then do                            
                            let wildFixedState = (updateDiscardPile playedState (DrawFourWild Red : tail (discardPile playedState)))                            
                            putStrLn (getNameFromIndex gs (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) ++ " drew " ++ show (deck gs !! 0) ++ ", " ++ show (deck gs !! 1) ++ ", " ++ show (deck gs !! 2) ++ " and " ++ show (deck gs !! 3) ++ "!")
                            let drawnState = (drawCard wildFixedState (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) 4)
                            putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " changed the color to Red!")
                            gameLoop (getNextPlayer (drawnState))
                        else if choiceNum == 2 then do
                            let wildFixedState = (updateDiscardPile playedState (DrawFourWild Blue : tail (discardPile playedState)))
                            putStrLn (getNameFromIndex gs (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) ++ " drew " ++ show (deck gs !! 0) ++ ", " ++ show (deck gs !! 1) ++ ", " ++ show (deck gs !! 2) ++ " and " ++ show (deck gs !! 3) ++ "!")
                            let drawnState = (drawCard wildFixedState (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) 4)
                            putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " changed the color to Blue!")
                            gameLoop (getNextPlayer (drawnState))
                        else if choiceNum == 3 then do
                            let wildFixedState = (updateDiscardPile playedState (DrawFourWild Green : tail (discardPile playedState)))
                            putStrLn (getNameFromIndex gs (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) ++ " drew " ++ show (deck gs !! 0) ++ ", " ++ show (deck gs !! 1) ++ ", " ++ show (deck gs !! 2) ++ " and " ++ show (deck gs !! 3) ++ "!")
                            let drawnState = (drawCard wildFixedState (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) 4)
                            putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " changed the color to Green!")
                            gameLoop (getNextPlayer (drawnState))
                        else if choiceNum == 4 then do
                            let wildFixedState = (updateDiscardPile playedState (DrawFourWild Yellow : tail (discardPile playedState)))
                            putStrLn (getNameFromIndex gs (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) ++ " drew " ++ show (deck gs !! 0) ++ ", " ++ show (deck gs !! 1) ++ ", " ++ show (deck gs !! 2) ++ " and " ++ show (deck gs !! 3) ++ "!")
                            let drawnState = (drawCard wildFixedState (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) 4)
                            putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " changed the color to Yellow!")
                            gameLoop (getNextPlayer (drawnState))
                        else do
                            putStrLn ("Invalid selection")
                            gameLoop gs
                    else do
                        gameLoop (getNextPlayer (playCard gs (currentPlayer gs) (actionNum - 1)))
                else do
                    putStrLn "This card does not match a type or color with the top card of the discard pile!"
                    gameLoop gs 
        

    
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