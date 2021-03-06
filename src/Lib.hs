module Lib
    ( formatGrid
    , outputGrid   
    , findWord  
    , findWords
    , findWordInLine  
    , findWordInCellLinePrefix 
    , getLines 
    , skew
    , zipOverGrid
    , zipOverGridWith
    , coordsGrid
    , gridWithCoords
    , cell2char
    , Cell (Cell,Indent)
    , Game (gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , playGame
    , formatGame
    , completed
    , fillInBlanks
    , formatGameGrid
    ) where 

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as M
import System.Random
import Data.Char (toLower)


type Grid a = [[a]]
data Game = Game {
                gameGrid :: Grid Cell,
                gameWords :: M.Map String (Maybe [Cell])
                }
            deriving Show

data Cell = Cell (Integer, Integer) Char 
    | Indent 
    deriving (Eq, Ord, Show)

makeGame :: Grid Char -> [String] -> Game 
makeGame grid words = 
    let gwc = gridWithCoords grid 
        tuplify word = (word, Nothing)
        list = map tuplify words
        dict = M.fromList  list 
    in Game gwc dict 

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word =
    let grid = gameGrid game
        foundWord = findWord grid word
    in case foundWord of 
            Nothing -> game
            Just cs -> 
                let dict = gameWords game
                    newDict = M.insert word foundWord dict
                in game { gameWords = newDict }
    

formatGame :: Game -> String
formatGame game = formatGameGrid game 
                ++ "\n\n"
                ++ (show $ score game)
                ++ "/"
                ++ (show $ totalWords game)

completed :: Game -> Bool
completed game = score game == totalWords game

makeRandomGrid gen = 
    let (gen1, gen2) = split gen 
        row = randomRs ('A', 'Z') gen1
    in row : makeRandomGrid gen2 

fillInBlanks gen grid =
    let r = makeRandomGrid gen  
        fill '_' r = r
        fill c _ = c
    in zipOverGridWith fill grid r

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid (Integer, Integer)
coordsGrid = 
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid 

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid) 


formatGrid :: Grid Cell-> String 
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char 
cell2char (Cell _ c) = c
cell2char Indent = '?'


getLines :: Grid Cell -> [[Cell]] 
getLines grid =  grid -- horizontal
               ++ (map reverse grid) --reverse horizontal
               ++ (transpose grid) --vertical
               ++ (map reverse (transpose grid)) --reverse vertical 
               ++ diagonalize grid  
               ++ diagonalize (map reverse grid) --diagonal right to left  
               ++ (map reverse (diagonalize grid)) -- reverse diagonal 
               ++ (map reverse (diagonalize (map reverse grid)))--reverse diagonal right to left  


diagonalize :: Grid Cell -> Grid Cell 
diagonalize  = transpose . skew  -- left to right 

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = Indent : line 

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
    let lines =  getLines grid  --so we can check left to right and right to left 
        foundWords = map (findWordInLine word) lines 
    in listToMaybe (catMaybes foundWords)
   
findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words = 
    let foundWords = map (findWord grid) words 
    in catMaybes foundWords 

findWordInLine :: String -> [Cell] -> Maybe [Cell] 
findWordInLine _ [] = Nothing 
findWordInLine word line = 
    let found = findWordInCellLinePrefix [] word line 
    in case found of 
        Nothing -> findWordInLine word (tail line)
        cells@(Just _) -> cells    


findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (char : chars) (cell : cells) | char == cell2char cell
    = findWordInCellLinePrefix (cell : acc) chars cells
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

formatGameGrid :: Game -> String
formatGameGrid game =
    let grid = gameGrid game 
        dict = gameWords game :: M.Map String (Maybe [Cell])
        cellSet = concat . catMaybes . M.elems $ dict
        formatCell cell = 
            let char = cell2char cell
            in if cell `elem` cellSet then toLower char else char --Changes the charater to lower case if found 
        charGrid = mapOverGrid formatCell grid 
    in unlines charGrid
