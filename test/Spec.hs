{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables, OverlappingInstances #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Blackjack
import PlayingCards 
import System.Random (getStdGen, Random(..), next)
import Control.Applicative ((<$>), (<*>))


blankGameState :: Game
blankGameState = Game { deck = [(Card Ace Spades)] 
                      , humanHand = [] 
                      , cpuHand = [] 
                      , nextPlayer = Human 
                      }



main :: IO ()
main = hspec $ do
    describe "Blackjack" $ do
        describe "hit" $ do
            it "draws a card from the deck and places it into the next player's hand, then switches turns" $ do
                let gameState = Game { deck = [(Card Ace Spades)] 
                                     , humanHand = [] 
                                     , cpuHand = [] 
                                     , nextPlayer = Human 
                                     }
                (hit gameState) `shouldBe` (gameState {deck = [], humanHand = [(Card Ace Spades)], nextPlayer = CPU})

        describe "stand" $ do
            it "just switches turns without affecting the deck or player hands" $ do
                let gameState = Game { deck = [(Card Ace Spades)] 
                                     , humanHand = [(Card Two Clubs)] 
                                     , cpuHand = [(Card Three Hearts)] 
                                     , nextPlayer = Human 
                                     }
                (stand gameState) `shouldBe` gameState { nextPlayer = CPU }

        describe "totalHandValue" $ do
            it "sums the ranks of all cards in a given hand" $ do
                (totalHandValue [(Card Two Hearts), (Card Ten Spades)]) `shouldBe` 12

        describe "determineWinner" $ do
            context "when a player has busted (exceeded 21)" $ do
                context "when that player is human" $ do
                    it "declares the computer the winner" $ do
                        let gameState = blankGameState { humanHand = [(Card Ten Hearts), (Card Ten Spades), (Card Ten Clubs)] }
                        (determineWinner gameState) `shouldBe` CPU

                context "when that player is the computer" $ do
                    it "declares the human player the winner" $ do
                        let gameState = blankGameState { cpuHand = [(Card Ten Hearts), (Card Ten Spades), (Card Ten Clubs)] }
                        (determineWinner gameState) `shouldBe` Human

            context "when neither player has busted (exceeded 21)" $ do
                context "when the human player has a higher score than the computer" $ do
                    it "declares the human player the winner" $ do
                        let gameState = blankGameState  { humanHand = [(Card Ten Hearts), (Card Eight Spades)] 
                                                        , cpuHand = [(Card Five Clubs)]
                                                        }
                        (determineWinner gameState) `shouldBe` Human

                context "when the computer has a higher score than the human player" $ do
                    it "declares the computer the winner" $ do
                        let gameState = blankGameState  { humanHand = [(Card Ten Hearts), (Card Two Spades)] 
                                                        , cpuHand = [(Card Ten Clubs), (Card Five Clubs)]
                                                        }
                        (determineWinner gameState) `shouldBe` CPU

                context "when the players' hands are equal" $ do
                    it "declares the computer the winner (because the house wins!)" $ do
                        let gameState = blankGameState  { humanHand = [(Card Ten Hearts)] 
                                                        , cpuHand = [(Card Ten Clubs)]
                                                        }
                        (determineWinner gameState) `shouldBe` CPU


    describe "PlayingCards" $ do

        describe "generateDeck" $ do
            it "generates a full deck of cards" $ property $
                \card -> let deck = generateDeck in card `elem` generateDeck

        describe "drawCard" $ do
            it "draws the top card off the deck and returns an updated deck" $ do
                let deck = [(Card Ace Clubs)]
                (drawCard deck) `shouldBe` ((Card Ace Clubs), [])

        describe "cardValue" $ do
            it "translates a card to its given value" $ property $
                \card -> (cardValue card) == case (rank card) of
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

        -- TODO: How do I test a shuffle?



class (Bounded a, Enum a) => BoundedEnum a
instance (Bounded a, Enum a) => BoundedEnum a
instance BoundedEnum a => Random a where
    random gen = randomR ((minBound :: a), (maxBound :: a)) gen
    randomR (f,t) gen = (toEnum r :: a, nextGen)
        where
            (rnd, nextGen) = next gen
            r = fromEnum f + (rnd `mod` length [f..t])
            

instance Arbitrary Card where
    arbitrary = Card <$> (choose (Ace, King)) <*> (choose (Hearts, Spades))
