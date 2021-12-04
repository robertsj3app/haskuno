module Uno where

import Control.Monad
import Data.Array.IO
import DeckFuncs
import GameFuncs
import HaskunoCards
import HaskunoTerms
import System.Random

emptyGameState :: GameState -- initial GameState structure
emptyGameState = StateRecord {currentPlayer = 0, playerList = [], turnDirection = CWise, deck = cardDeck, discardPile = [Wild Undefined]}

getPlayers :: GameState -> Integer -> IO GameState
getPlayers gs i = do
  if i > 0
    then do
      putStrLn "\nEnter your name"
      name <- getLine
      getPlayers (addPlayer gs name) (i - 1)
    else do
      return gs

startingHands :: GameState -> Integer -> Integer -> GameState
startingHands gs i n = if i < n then startingHands (drawCard gs i 7) (i + 1) n else gs

showHand :: Hand -> Integer -> IO ()
showHand (x : []) i = do
  putStrLn ((show (i + 1)) ++ ") " ++ (show x))
  putStrLn ((show (i + 2)) ++ ") Draw from deck")
showHand (x : xs) i = do
  putStrLn ((show (i + 1)) ++ ") " ++ (show x))
  showHand xs (i + 1)

main :: IO ()
main = do
  putStrLn "\nWelcome to HaskUno!\n\nHow many people will be playing?"
  num <- getLine
  let numPlayers = (read num :: Integer)
  partialState <- getPlayers emptyGameState numPlayers
  shuffledDeck <- shuffle (deck partialState)
  let shuffledState = updateDeck partialState shuffledDeck
  let readyState = startingHands shuffledState 0 numPlayers
  gameLoop readyState

gameLoop :: GameState -> IO ()
gameLoop gs = do
  if numCards (deck gs) == 0
    then do
      shuffledDeck <- shuffle (discardPile gs)
      let shuffledState = updateDeck gs shuffledDeck
      let clearedState = updateDiscardPile gs []
      gameLoop clearedState
    else do
      putStrLn ("\nIt is now " ++ getNameFromIndex gs (currentPlayer gs) ++ "'s turn!\n")
      putStrLn ("The top card of the discard pile is: " ++ show (head (discardPile gs)))
      putStrLn (getNameFromIndex gs (currentPlayer gs) ++ "'s hand is: \n")
      let hand = snd (playerList gs !! fromIntegral (currentPlayer gs))
      showHand hand 0
      putStrLn ("\nEnter the number before the parenthesis to take that action.")
      chosenAction <- getLine
      let actionNum = (read chosenAction :: Integer)
      if actionNum > (numCards hand) + 1
        then do
          putStrLn "Invalid selection"
          gameLoop gs
        else do
          if actionNum > numCards hand
            then do
              putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " drew " ++ show (head (deck gs)))
              let drawState = drawCard gs (currentPlayer gs) 1
              gameLoop (getNextPlayer drawState)
            else do
              let chosenCard = (hand !! fromIntegral (actionNum - 1))
              if validSelection (chosenCard) (discardPile gs)
                then do
                  putStrLn (getNameFromIndex gs (currentPlayer gs) ++ " played " ++ show chosenCard)
                  if getType chosenCard == "Skip"
                    then do
                      putStrLn (getNameFromIndex gs (fixOverhang ((getTurnDirection gs) (currentPlayer gs) 1) (playerList gs)) ++ "'s Turn was skipped!")
                      gameLoop (skipNextPlayer (playCard gs (currentPlayer gs) (actionNum - 1)))
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