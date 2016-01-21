{-# LANGUAGE NamedFieldPuns #-}

module PlayingCards
    ( Suit(..)
    , Rank(..)
    , Card(..)
    , Deck
    , generateDeck
    , shuffleDeck
    , drawCard
    , cardValue
    ) where

import System.Random
import System.Random.Shuffle (shuffle')
import Control.Applicative ((<$>), (<*>))

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq, Enum)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Ord, Enum)

data Card = Card 
    { rank :: Rank
    , suit :: Suit } deriving (Eq)
instance Show Card where
    show (Card {rank, suit}) = (show rank) ++ " of " ++ (show suit)

type Deck = [Card]

generateDeck :: Deck
generateDeck = Card <$> [Ace .. King] <*> [Hearts .. Diamonds]

shuffleDeck :: RandomGen g => g -> Deck -> Deck
shuffleDeck _ [] = []
shuffleDeck rng deck = shuffle' deck (length deck) rng

drawCard :: Deck -> (Card, Deck)
drawCard (drawnCard:updatedDeck) = (drawnCard, updatedDeck)

cardValue :: Card -> Int
cardValue (Card {rank}) = case rank of
                            Ace -> 1
                            Two -> 2
                            Three -> 3
                            Four -> 4
                            Five -> 5
                            Six -> 6
                            Seven -> 7
                            Eight -> 8
                            Nine -> 9
                            _ -> 10
