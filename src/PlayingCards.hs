{-# LANGUAGE NamedFieldPuns #-}

module PlayingCards
    ( Suit
    , Rank
    , Card
    , Deck
    , generateDeck
    , drawCard
    , cardValue
    ) where

import Data.Maybe (fromJust)
import System.Random

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Ord)

data Card = Card 
    { rank :: Rank
    , suit :: Suit } deriving (Eq)
instance Show Card where
    show (Card {rank, suit}) = (show rank) ++ " of " ++ (show suit)

type Deck = [Card]


generateDeck :: Deck
generateDeck = concat $ map generateCardsForSuit [Hearts, Diamonds, Clubs, Spades] 
    where
        generateCardsForSuit suit = map (\r -> Card (fromJust $ mkRank r) suit) [1..13]
        mkRank :: Int -> Maybe Rank
        mkRank 1  = Just Ace
        mkRank 2  = Just Two
        mkRank 3  = Just Three
        mkRank 4  = Just Four
        mkRank 5  = Just Five
        mkRank 6  = Just Six
        mkRank 7  = Just Seven
        mkRank 8  = Just Eight
        mkRank 9  = Just Nine
        mkRank 10 = Just Ten
        mkRank 11 = Just Jack
        mkRank 12 = Just Queen
        mkRank 13 = Just King
        mkRank _  = Nothing 

drawCard :: Deck -> StdGen -> (Card, Deck, StdGen)
drawCard deck rng = (drawnCard, updatedDeck, updatedRNG)
    where
        (cardNumber, updatedRNG) = randomR (0, (length deck) - 1) rng
        drawnCard = deck !! cardNumber
        updatedDeck = filter ((/=) drawnCard) deck

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
