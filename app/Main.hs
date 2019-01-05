--Word Search Game
--Yusra Irfan

module Main where 
    
import Lib
import Data
import System.IO 
    
main :: IO ()
main = do 
    let game = makeGame grid languages
    hSetBuffering stdout NoBuffering
    playTurn game 
    
playTurn game = do 
    putStrLn . formatGame $ game 
    putStr "Please enter a word: "
    word <- getLine
    let newGame = playGame game word
    if completed newGame then 
        putStrLn "Congrats!"
    else 
        playTurn newGame
