
--{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE OverloadedStrings #-}
module Lib where
import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import System.Random
import Data.Char (toUpper)
import Text.Read (readMaybe)
import Types

makeBoard :: Size  -> Board 
makeBoard s = Board{ getSize = smin
                    ,getSquares = replicate smin (replicate smin False)
                    }
            where smin = min maxBoardSize (max s minBoardSize)

makeBoardCoord :: Size  -> Board 
makeBoardCoord s = BoardCoord{ getSizeCoord = smin
                    ,getCoordSquares = makeMatrixCoord (smin,65)
                    }
            where smin = min maxBoardSize (max s minBoardSize)

makeShip :: Size -> Ship 
makeShip s = Ship { getSizes = s
                    , getShipSquares = replicate s False
                    }

makeBoardToPrint :: Size -> BoardColor
makeBoardToPrint s = BoardColor{ getLength =s
                   , getIcons = replicate s (replicate s D) 
                   }

--cex1= mapM_ putStrLn pex1

showIcon :: Icon -> String
showIcon D = addDefault ++ [symbol] 
showIcon S = addRed     ++ [symbol] ++ addDefault
showIcon W = addBlue    ++ [symbol] ++ addDefault

-- (size Board, A::Int == 65)
makeMatrixCoord ::(Int, Int) -> [[Coordinate]]
makeMatrixCoord (0,_) = []
makeMatrixCoord (x,90) = [[('Z', x)]]
makeMatrixCoord (x,n) = first : second
        where 
                first  = zip (replicate x (toEnum n :: Char)) [0.. x-1]
                second | (n-63) > x = makeMatrixCoord (0, n+1) --63 because at first time always gona be 2 rows  
                       | otherwise  = makeMatrixCoord (x, n+1) 

checkCoord :: String -> Int -> Bool
checkCoord [r,c] max = cRow && go charC
        where 
            charR = subtract 65 . fromEnum $ toUpper r
            charC = readMaybe [c] :: Maybe Int 
            cRow  = charR >= 0 && charR <= max-1
            go :: Maybe Int -> Bool
            go Nothing   = False
            go (Just x)  = x>=0 && x<= max-1
checkCoord _ _ = False

searchCoord :: Int -> Int -> Board -> Coordinate
searchCoord r c b = elem
        where
                row  = getCoordSquares b !! r
                elem = row !! c

vectorLength :: Coordinate -> Board  -> [(Int , Axis)]
vectorLength (r , c) b = [(xp, Xp ), (xn, Xn), (yp, Yp), (yn, Yn)]
        where
                xn = c + 1
                xp = getSizeCoord b - xn + 1--0...size = size +1 +1(itself)
                yp = fromEnum r - 65 + 1 
                yn = getSizeCoord b - yp + 1
                
--CHANGE INPUTS [] AT FUNCTIOM, USING MAP IN THE CALLIN AVOID THAT
compareShipVector ::  Ship -> (Int , Axis)-> Bool
compareShipVector _ (0, _)  = False
compareShipVector s (len,a)  = getSizes s <= len

                        --check because resize of ships (_,1)

availableSpace ::  Board -> Coordinate -> Axis -> Bool 
availableSpace (Board _ b) (r, c) a = 
        case a of
                Xp -> foldr (||) False (drop c (b !! charNum)) 
                Xn -> foldr (||) False (take (c+1) (b !! charNum))
                Yn -> foldr (||) False (drop charNum (transposeBoard !! c))
                Yp -> foldr (||) False (take (charNum+1) (transposeBoard !! c))
                where
                        charNum = fromEnum r - 65
                        transposeBoard = transpose b

-- (length ship, BoardCoord) -> return axisAvail (using map)
getAxisVectors :: (Int, Board ) -> (Coordinate, Axis) ->  [Coordinate]
getAxisVectors (len, BoardCoord s b ) ((r, c), a) = 
        case a of
                Xp -> take len (drop c takeRow)
                Xn -> drop (c-len+1) (take (c+1) takeRow)
                Yp -> drop (rowChar-len+1) (take (rowChar+1) takeCol) 
                Yn -> take len (drop rowChar takeCol)
                where
                        rowChar = fromEnum r - 65
                        transposeBoard = transpose b
                        takeRow = b !! rowChar
                        takeCol = transposeBoard !! c

axisShorter :: (Board, Ship) -> Coordinate-> [Axis]
axisShorter (b , s) c = axisAvailable
        where
                vectorLen     = vectorLength c b
                listSVbool    = map (compareShipVector s) vectorLen
                zipAxisSV     = M.fromList $ zip vectorLen listSVbool
                filtZip       = M.filter (&&True) zipAxisSV
                listCoord     = M.keys filtZip 
                axisAvailable = M.elems $ M.fromList listCoord

--RETURN LIST OF AXIS EMPTY (AND LARGER CHECKED PREVIUOSLY) FOR THE SHIP
-- Bool Board -> the random coord -> the return of axisShorter
listAxisAvail :: Board -> Coordinate -> [Axis] -> [(Coordinate,Axis)]
listAxisAvail _ _ [] = []
listAxisAvail b c a = zip (replicate (length listAxis) c) listAxis
                where
                listBool      = map (availableSpace b c) a
                zipAxisSV     = M.fromList $ zip a listBool
                filtZip       = M.filter not zipAxisSV
                listAxis      = M.keys filtZip 

mapAxis :: [[Bool]] -> [Map Int Bool]
mapAxis [] = []
mapAxis (xs : xss) = head : tail 
        where   
                sz   = length xs
                head = M.fromList $ zip [0..sz-1] xs
                tail = mapAxis xss

mapB1 = mapAxis [[False,True],[False,False]]

tryShoot :: String -> [Map Int Bool]-> (Bool,Maybe Coordinate)
tryShoot [r, c] rowsMap = go charC
        where 
            charR = subtract 65 . fromEnum $ toUpper r
            charC = readMaybe [c] :: Maybe Int   
            go :: Maybe Int -> (Bool, Maybe Coordinate)
            go Nothing   = (False, Nothing)
            go (Just x)  = case M.lookup x (rowsMap !! charR) of
                Just y  -> (y, Just (toUpper r,x)) 
                
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (x:xs) = f x : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

--Place 1 coordinate of 1 ship
fillBoardCoord :: [[Bool]] -> Coordinate -> [[Bool]]
fillBoardCoord matrix (r,c) = newMatrix
        where
                rInt  = fromEnum r -65
                head  = take rInt matrix
                modif = modifyAt c (||True) (matrix !! rInt)
                tail  = drop (rInt+1) matrix
                newMatrix = head ++ [modif] ++ tail

--Extract the result (matrix) of all coordinates of 1 ship 
fillBoardShip ::  [[[Bool]]] ->[Coordinate] -> [[Bool]]
fillBoardShip [m] cs = m
fillBoardShip (m:ms) (c:cs)  = fillBoardShip (map (fillBoardCoord m) cs) cs

fillBoardAllShips :: [[Bool]] -> [[Coordinate]] -> [[Bool]]
fillBoardAllShips m [] = m
fillBoardAllShips m (s : ss) = fillBoardAllShips matOneShip ss
                        where
                                matPerCoord = map (fillBoardCoord m) s
                                matOneShip  = fillBoardShip matPerCoord s

checkList :: [String] -> String-> [Bool]
checkList xs rc = foldr (\ x -> (:) (x == rc)) [False] xs

placeShip :: (Coordinate, Ship) -> (Board, Board)-> [Coordinate]
placeShip (coord,s) (bBool, bCoord)  = oneVect
    where 
        listAxis   = axisShorter (bCoord, s) coord
        axisAvail  = listAxisAvail bBool coord listAxis
        vectOpt    = map (getAxisVectors (getSizes s, bCoord)) axisAvail
        oneVect    = if not (null vectOpt) then head vectOpt
                        else []
                   
totalShips :: [[Bool]] -> [Int]
totalShips [] = [0]
totalShips xs = foldr ((:) . length . filter (&& True)) [0] xs

colorCoord :: Coordinate -> Icon -> BoardColor -> BoardColor
colorCoord (r,c) icon (BoardColor sz matChar) = BoardColor {getLength = sz, getIcons=matColor}
        where
                rInt  = fromEnum r -65
                head  = take rInt matChar
                modif = take c (matChar !! rInt) ++ [icon] ++ drop (c+1) (matChar !! rInt)
                tail  = drop (rInt+1) matChar
                matColor = head ++ [modif] ++ tail

