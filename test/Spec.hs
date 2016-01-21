import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Blackjack
import PlayingCards 
import System.Random (getStdGen)


blankGameState :: Game
blankGameState = Game { deck = [(Card Ace Spades)] 
                      , humanHand = [] 
                      , cpuHand = [] 
                      , nextPlayer = Human 
                      }

main :: IO ()
main = hspec $ do
    describe "Lib.hit" $ do
        it "draws a card from the deck and places it into the next player's hand, then switches turns" $ do
            let gameState = Game { deck = [(Card Ace Spades)] 
                                 , humanHand = [] 
                                 , cpuHand = [] 
                                 , nextPlayer = Human 
                                 }
            (hit gameState) `shouldBe` (gameState {deck = [], humanHand = [(Card Ace Spades)], nextPlayer = CPU})

    describe "Lib.stand" $ do
        it "just switches turns without affecting the deck or player hands" $ do
            let gameState = Game { deck = [(Card Ace Spades)] 
                                 , humanHand = [(Card Two Clubs)] 
                                 , cpuHand = [(Card Three Hearts)] 
                                 , nextPlayer = Human 
                                 }
            (stand gameState) `shouldBe` gameState { nextPlayer = CPU }

    describe "Lib.totalHandValue" $ do
        it "sums the ranks of all cards in a given hand" $ do
            (totalHandValue [(Card Two Hearts), (Card Ten Spades)]) `shouldBe` 12

    describe "Lib.determineWinner" $ do
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
