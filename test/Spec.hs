import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Blackjack
import PlayingCards 
import System.Random (getStdGen)

main :: IO ()
main = hspec $ do
    describe "Lib.hit" $ do
        it "draws a card from the deck and places it into the next player's hand, then switches turns" $ do
            rng <- getStdGen
            let gameState = Game { deck = [(Card Ace Spades)] 
                                 , humanHand = [] 
                                 , cpuHand = [] 
                                 , nextPlayer = Human 
                                 }
            (hit gameState) `shouldBe` (Game {deck = [], humanHand = [(Card Ace Spades)], cpuHand = [], nextPlayer = CPU})
