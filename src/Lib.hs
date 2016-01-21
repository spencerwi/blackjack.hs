{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( Game
    , Player
    , hit
    , stand
    , determineWinner
    , totalHandValue
    , initialGameState
    ) where

import System.Random (StdGen)
import PlayingCards

data Player = Human | CPU deriving (Show, Eq)

data Game = Game 
    { deck      :: Deck 
    , humanHand :: [Card]
    , cpuHand   :: [Card] 
    , rng       :: StdGen
    , nextPlayer :: Player
    } deriving (Show)

initialGameState :: StdGen -> Game
initialGameState suppliedRNG = Game 
    { deck = (generateDeck)
    , humanHand = []
    , cpuHand = []
    , rng = suppliedRNG
    , nextPlayer = Human
    }

swapPlayer :: Player -> Player
swapPlayer Human = CPU
swapPlayer CPU = Human

hit :: Game -> Game
hit gameState = gameState { deck = updatedDeck, rng = updatedRNG, humanHand = newHumanHand, cpuHand = newCPUHand, nextPlayer = (swapPlayer nextPlayer) }
    where 
        Game { deck, humanHand, cpuHand, rng, nextPlayer } = gameState
        (drawnCard, updatedDeck, updatedRNG) = drawCard deck rng
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
