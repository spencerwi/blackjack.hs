module Main where

import Lib
import System.Random (getStdGen)

data GameAction = Hit | Stand

parseGameAction :: String -> Maybe GameAction
parseGameAction "hit"   = Just Hit
parseGameAction "stand" = Just Stand
parseGameAction _       = Nothing

main :: IO ()
main = do
    rng <- getStdGen
    let game = initialGameState rng
    putStrLn $ show game
