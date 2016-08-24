{-# LANGUAGE NamedFieldPuns #-}

module PlayingCards
    ( Suit(..)
    , Rank(..)
    , Card(..)
    , Deck
    , generateDeck
    , shuffleDeck
    , drawCard
    ) where

import System.Random
import System.Random.Shuffle (shuffle')
import Control.Applicative ((<$>), (<*>))

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq, Bounded, Enum)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Ord, Bounded, Enum)

data Card = Card 
    { rank :: Rank
    , suit :: Suit } deriving (Eq)
instance Show Card where
    show (Card {rank, suit}) = (show rank) ++ " of " ++ (show suit)

type Deck = [Card]

generateDeck :: Deck
generateDeck = Card <$> [Ace .. King] <*> [Hearts .. Spades]

shuffleDeck :: RandomGen g => g -> Deck -> Deck
shuffleDeck _ [] = []
shuffleDeck rng deck = shuffle' deck (length deck) rng

drawCard :: Deck -> (Card, Deck)
drawCard (drawnCard:updatedDeck) = (drawnCard, updatedDeck)

