
module Main where
import Text.Read (readMaybe)
import Data.List (intercalate, insert)
import Lib
import Types 
import System.Random
import Data.Char (toUpper)


main :: IO ()
main = do
    putStrLn "Enter the size of the board (5 to 15):"
    ms <- getInt 
    case ms of
    -- if a number isn't provided, print error and restart
        Nothing -> putStrLn "Enter a valid number" >> main
        Just s  -> if s >= minBoardSize && s <= maxBoardSize
      -- if a valid number is provided within the accepted range, initiate the game
        then newShip [] (getSquares $ makeBoard s) (makeBoardCoord s)

      -- otherwise print error and restart
        else putStrLn "Invalid board size" >> main
      
getInt :: IO (Maybe Int)
getInt = do 
    readMaybe <$> getLine
      
newShip :: [Coordinate] -> [[Bool]] -> Board-> IO()
newShip listC b bCoord= do
    let left    = (^2) (getSizeCoord bCoord) - sum (totalShips b) 
    let szBoard = getSizeCoord bCoord
    putStrLn  ("Size of ship 1 to "++show szBoard++", remaining "++show left++" spaces")
    msg <- getInt 
    case msg of
        Nothing -> putStrLn "Enter a valid size"  >> newShip listC b bCoord
        Just sz -> if sz>= 1 && sz<= szBoard then
                do
                 if left-sz <= 0
                    then putStrLn "Not available space for that ship" >> main
                    else 
                        searchVect listC 0 sz szBoard b bCoord
            else putStrLn "Enter a valid size" >> newShip listC b bCoord    

randNum :: Int -> IO Int
randNum i = randomRIO (0, i-1) :: IO Int
               
searchVect :: [Coordinate] -> Int -> Int -> Size -> [[Bool]] ->Board -> IO()            
searchVect  listC count sz szBoard b bCoord = do
                row <- randNum szBoard
                col <- randNum szBoard
                let coord  = searchCoord row col bCoord
                let bBool  = Board{getSize=szBoard, getSquares = b}
                putStrLn "Searching place at board" 
                let try  = (+1) count
                let vect = placeShip (coord, makeShip sz) (bBool,bCoord)
                if try > maxTries 
                    then putStrLn "Not finding ship place, try other game" >> main
                else
                    if null vect then searchVect listC try sz szBoard b bCoord
                    else 
                    do
                    putStrLn "Placing the ship..."
                    let matUpt = fillBoardAllShips b [vect]
                    let allCoord = listC ++ vect

                    putStrLn "Create another ship Yes:Y No:Press any key"
                    nwSh <- getLine
                    if checkNewShip nwSh then
                         putStrLn "New Ship" >> newShip allCoord matUpt bCoord
                    else 
                        do
                        let matColor = makeBoardToPrint szBoard
                        let numShips = sum (totalShips matUpt)
                        showBoard (makeBoardToPrint szBoard)
                        userCoord allCoord (numShips, numShips+extraBullets) [] matUpt matColor
                where 
                    checkNewShip :: [Char] -> Bool
                    checkNewShip [s] = toUpper s =='Y'       
                    checkNewShip _   = False

userCoord :: [Coordinate] -> (Int,Int) -> [String] -> [[Bool]] -> BoardColor -> IO()
userCoord listC (hit,water) lCoord bBool bColor = do
        if hit == 0 && water > 0 then 
            putStrLn "You win! GAME OVER"
        else if hit > 0 && water == 0 then
            putStrLn "You loose! GAME OVER" >>
            showResult listC (makeBoardToPrint (getLength bColor))
        else do
            putStrLn "Choose coordinates and try to sink (ex. A1):"
            coord <- getLine
            if or (checkList lCoord coord) then 
                putStrLn "Coordinate already set" >> userCoord listC (hit,water) lCoord bBool bColor
            else do
                if not $ checkCoord coord (length bBool) then
                    putStrLn "Wrong coordinate range" >> userCoord listC (hit,water) lCoord bBool bColor
                else do
                    -- ALOME SEGUNDA FUNCION PARA ESTO
                    case tryShoot coord (mapAxis bBool) of
                        ( _ , Nothing) -> userCoord listC (hit,water) lCoord bBool bColor
                        ( diana, Just rc) -> if diana then do
                            let uptBoard = colorCoord rc S bColor
                            showBoard uptBoard
                            putStrLn "It's a HIT!" >> userCoord listC (hit-1, water) (insert coord lCoord) bBool uptBoard
                        else do
                            let uptBoard = colorCoord rc W bColor
                            showBoard uptBoard
                            putStrLn "Water!" >> userCoord listC (hit, water-1) (insert coord lCoord) bBool uptBoard

-- Prints the current representation of the board state to the terminal:
showBoard :: BoardColor -> IO ()
showBoard b = mapM_ putStrLn allStrs
  where
    s       = getLength b
    sqs     = getIcons b
    -- prepare the header string with numbered columns up to the board width
    header  = "  | " ++ intercalate " | " (map show [0 .. s - 1]) ++ " |"
    -- combine header string with formatted board row strings prefixed by row letters
    allStrs = header : zipWith (:) (map (\x -> toEnum $ x + 65) [0 .. s - 1]) (map fmtRow sqs)
    -- creates a formatted string representation of the row
    fmtRow :: Row -> String
    fmtRow r = " | " ++ intercalate " | " (map showIcon r) ++ " |"

showResult :: [Coordinate]  -> BoardColor ->IO ()
showResult [] bc = showBoard bc
showResult (x:xs) bc = showResult xs lastBoard
    where
        lastBoard = colorCoord x S bc