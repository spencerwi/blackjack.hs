{-# LANGUAGE NamedFieldPuns #-}

module Blackjack
    ( Game(..)
    , Player(..)
    , hit
    , stand
    , determineWinner
    , cardValue
    , totalHandValue
    , initialGameState
    ) where

import System.Random (RandomGen)
import Data.List (maximumBy)
import PlayingCards

data Player = Human | CPU deriving (Show, Eq)

data Game = Game 
    { deck      :: Deck 
    , humanHand :: [Card]
    , cpuHand   :: [Card] 
    , nextPlayer :: Player
    , isFinished :: Bool
    } deriving (Show, Eq)

initialGameState :: RandomGen g => g -> Game
initialGameState suppliedRNG = Game 
    { deck = (shuffleDeck suppliedRNG generateDeck)
    , humanHand = []
    , cpuHand = []
    , nextPlayer = CPU
    , isFinished = False
    }

handIsFinished :: [Card] -> Bool
handIsFinished hand
    | (length hand) == 5            = True
    | (totalHandValue hand) > 21    = True
    | otherwise                     = False

hit :: Game -> Game
hit (Game {deck, humanHand, cpuHand, nextPlayer}) = Game 
    { deck       = updatedDeck
    , humanHand  = newHumanHand
    , cpuHand    = newCPUHand
    , nextPlayer
    , isFinished = isFinished'
    }
    where 
        (drawnCard, updatedDeck) = drawCard deck 
        (newCPUHand, newHumanHand) = case nextPlayer of
            Human   -> (cpuHand, humanHand ++ [drawnCard])
            CPU     -> (cpuHand ++ [drawnCard], humanHand)
        isFinished' = (nextPlayer == Human && handIsFinished cpuHand) || (nextPlayer == CPU && handIsFinished humanHand)
        
        
stand :: Game -> Game
stand gameState = gameState { nextPlayer = nextPlayer' }
    where
        nextPlayer' = if (nextPlayer gameState) == CPU then Human else CPU

totalHandValue :: [Card] -> Int
totalHandValue hand = 
    let acesHighValue = sum $ map (cardValue True) hand
        acesLowValue  = sum $ map (cardValue False) hand
    in
        if acesHighValue > 21 then acesLowValue else acesHighValue


determineWinner :: Game -> Player
determineWinner (Game { humanHand, cpuHand }) = 
    let humanHandValue = totalHandValue humanHand
        cpuHandValue = totalHandValue cpuHand
    in
        if      humanHandValue > 21             then CPU
        else if cpuHandValue   > 21             then Human
        else if humanHandValue > cpuHandValue   then Human
        else                                         CPU

cardValue :: Bool -> Card -> Int
cardValue aceHigh (Card {rank}) = 
    case rank of
        Ace -> if aceHigh then 10 else 1
        Two -> 2
        Three -> 3
        Four -> 4
        Five -> 5
        Six -> 6
        Seven -> 7
        Eight -> 8
        Nine -> 9
        _ -> 10
