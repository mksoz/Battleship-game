module Main where
import Text.Read (readMaybe)
import BattleShip
import Data.Char (toUpper)
--import Types 
import System.Random
maxShipSize = 5
minShipSize = 1
maxTries = 1000

main :: IO ()
main = do
    putStrLn "Enter the size of the board (5 to 15):"
    ms <- getInt 
    case ms of
    -- if a number isn't provided, print error and restart
        Nothing -> putStrLn "Enter a valid number" >> main
        Just s  -> if s >= minBoardSize && s <= maxBoardSize
      -- if a valid number is provided within the accepted range, initiate the game
        then newShip (getSquares $ makeBoard s) (makeBoardCoord s)

      -- otherwise print error and restart
        else putStrLn "Invalid board size" >> main
      
getInt :: IO (Maybe Int)
getInt = do 
    readMaybe <$> getLine
      
newShip :: [[Bool]] -> Board-> IO()
newShip b bCoord= do
    let left = (^2) (getSizeCoord bCoord) - sum (totalShips b) 
    putStrLn  ("Size of ship 1 to 5, remaining "++show left)
    let szBoard = getSizeCoord bCoord
    msg <- getInt 
    case msg of
        Nothing -> putStrLn "Enter a valid size"  >> newShip b bCoord
        Just sz -> if sz>= 1 && sz<= szBoard then
                do
                 if left-sz <= 0
                    then putStrLn "Not available space for that ship" >> main
                    else 
                        searchVect 0 sz szBoard b bCoord
            else putStrLn "Enter a valid size" >> newShip b bCoord
            
                
searchVect :: Int-> Int -> Size -> [[Bool]] ->Board -> IO()            
searchVect count sz szBoard b bCoord = do
                row <- randNum szBoard
                col <- randNum szBoard
                let coord  = searchCoord row col bCoord
                let bBool  = Board{getSize=szBoard, getSquares = b}
                putStrLn "Searching place at board" 
                let try = (+1) count
                let vect   = placeShip (coord, makeShip sz) (bBool,bCoord)
                if try > maxTries 
                    then putStrLn "Not finding ship place, try other game" >> main
                else
                    if null vect then searchVect try sz szBoard b bCoord
                    else 
                    do
                    putStrLn "Placing the ship..."
                    let matUpt = fillBoardAllShips b [vect]
                -- Ask for introduce another ship
                    putStrLn "Create another ship Yes:Y No:Press any key"
                    nwSh <- getLine
                    if checkNewShip nwSh then
                         putStrLn"New Ship" >> newShip matUpt bCoord
                    else  print matUpt --lets play
                where 
                    checkNewShip :: String -> Bool
                    checkNewShip s | toUpper (head s) =='Y' 
                                         &&  length s == 1  = True 
                                   | otherwise = False

userCoord :: Int -> Int -> [[Bool]] -> IO()
userCoord hit water bBool = do
        --print Board
        putStrLn "Choose coordinates and try to sink (ex. A1)"
        coord <- getLine
        if not $ checkCoord coord (length bBool) then
            putStrLn "Wrong coordinate range" >> userCoord hit water bBool
        else
            putStrLn "continue"
     

randNum :: Int -> IO Int
randNum i = randomRIO (0, i-1) :: IO Int



