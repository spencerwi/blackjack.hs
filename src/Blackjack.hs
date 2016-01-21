{-# LANGUAGE NamedFieldPuns #-}

module Blackjack
    ( Game
    , Player
    , hit
    , stand
    , determineWinner
    , totalHandValue
    , initialGameState
    ) where

import System.Random (RandomGen)
import PlayingCards

data Player = Human | CPU deriving (Show, Eq)

data Game = Game 
    { deck      :: Deck 
    , humanHand :: [Card]
    , cpuHand   :: [Card] 
    , nextPlayer :: Player
    } deriving (Show)

initialGameState :: RandomGen g => g -> Game
initialGameState suppliedRNG = Game 
    { deck = (shuffleDeck suppliedRNG generateDeck)
    , humanHand = []
    , cpuHand = []
    , nextPlayer = Human
    }

swapPlayer :: Player -> Player
swapPlayer Human = CPU
swapPlayer CPU = Human

hit :: Game -> Game
hit (Game {deck, humanHand, cpuHand, nextPlayer}) = Game 
    { deck       = updatedDeck
    , humanHand  = newHumanHand
    , cpuHand    = newCPUHand
    , nextPlayer = (swapPlayer nextPlayer) 
    }
    where 
        (drawnCard, updatedDeck) = drawCard deck 
        (newCPUHand, newHumanHand) = case nextPlayer of
                                        Human   -> (cpuHand, humanHand ++ [drawnCard])
                                        CPU     -> (cpuHand ++ [drawnCard], humanHand)
        
stand :: Game -> Game
stand gameState = gameState { nextPlayer = (swapPlayer $ nextPlayer gameState) }

totalHandValue :: [Card] -> Int
totalHandValue hand = sum $ map cardValue hand
-- TODO: Make this smart about Ace-high vs Ace-low


determineWinner :: Game -> Player
determineWinner (Game { humanHand, cpuHand }) = 
    let humanHandValue = totalHandValue humanHand
        cpuHandValue = totalHandValue cpuHand
    in
        if      humanHandValue > 21             then Human
        else if cpuHandValue > 21               then CPU
        else if (humanHandValue > cpuHandValue) then Human
        else    CPU
