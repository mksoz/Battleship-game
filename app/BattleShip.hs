{-# LANGUAGE InstanceSigs #-}
module BattleShip where
import Data.List
--CHANGE INPUTS [] AT FUNCTIOM, USING MAP IN THE CALLIN AVOID THAT

--types
type Size = Int
type RowIndex = Char
type ColIndex  = Int 
type Coordinate = (RowIndex, ColIndex)
type ShipSize = (Int, Int)

data Axis = Xp | Xn | Yp | Yn deriving(Show)
data Board b  =   Board{ getSize   :: Size,
                       getSquares :: [[Bool]]}
                | BoardCoord{ getSizeCoord   :: Size, getCoordSquares :: [[Coordinate]]} deriving(Show, Eq) 
                       
data Ship s   =   Ship{ getSizes :: ShipSize,
                       getShipSquares :: [[Bool]]}
                | ShipCoord{getShipSizesCoord :: ShipSize, getShipCoordSquares:: [[Coordinate]]} deriving (Show, Eq)

listSizeShip = [(5,1),(2,2),(4,1),(2,1)] 
minBoardSize = 5
maxBoardSize = 25

makeBoard :: Size  -> Board b
makeBoard s = Board{ getSize = smin
                    ,getSquares = replicate smin (replicate smin False)
                    }
            where smin = min maxBoardSize (max s minBoardSize)
                        -- maximun between s and minBoardSize
b1 = makeBoard 6

makeShip :: ShipSize -> Ship s
makeShip (r, c) = Ship { getSizes = (nr,nc)
                    , getShipSquares = replicate nr (replicate nc True)
                    }
                where 
                        nr = max 1 r
                        nc = max 1 c


--CHANGE INPUTS [] AT FUNCTIOM, USING MAP IN THE CALLIN AVOID THAT OR LIST $
makeListShip :: [ShipSize] -> [Ship s]
makeListShip = map makeShip 

listships = makeListShip listSizeShip

--CHANGE INPUTS [] AT FUNCTIOM, USING MAP IN THE CALLIN AVOID THAT OR LIST $
getShipsArea :: [Ship s] -> [Int]
getShipsArea [] = []
getShipsArea (s : ss) = (getArea.getSizes $ s) : getShipsArea ss
        where
            getArea (c,r) = c*r

listAreasShips = getShipsArea listships

checkSquares :: [Ship s] -> Board b -> (Bool, [Char])
checkSquares [] _ = (False, "No ships")
checkSquares ls b = case compare ((^) (getSize b) 2) (sum.getShipsArea $ ls) of
                     LT -> (False, "More ships area than board area")
                     EQ -> (False, "No water area")
                     GT -> (True, "More board than ships")
checkB1Ships = checkSquares listships b1


-- toEnum 65 :: Char = 'A'
-- (size Board, A::Int == 65)
makeMatrixCoord ::(Int, Int) -> [[Coordinate]]
makeMatrixCoord (0,_) = []
makeMatrixCoord (x,90) = [[('Z', x)]]
makeMatrixCoord (x,n) = first : second
        where 
                first  = zip (replicate x (toEnum n :: Char)) [0.. x-1]
                second | (n-63) > x = makeMatrixCoord (0, n+1) --63 because at first time always gona be 2 rows  
                       | otherwise  = makeMatrixCoord (x, n+1) 

b2 = BoardCoord{getSizeCoord = getSize b1, getCoordSquares = makeMatrixCoord (getSize b1, 65)}

searchCoord :: Int -> Int -> Board b-> Coordinate
searchCoord r c b = elem
        where
                row  = getCoordSquares b !! r
                elem = row !! c

vectorLength :: Coordinate -> Board b -> [(Int , Axis)]
vectorLength (r , c) b = [(xp, Xp ), (xn, Xn), (yp, Yp), (yn, Yn)]
        where
                xn = c + 1
                xp = getSizeCoord b - xn + 1--0...size = size +1 +1(itself)
                yp = fromEnum r - 65 + 1 
                yn = getSizeCoord b - yp + 1
                
veclength = vectorLength ('D', 4) b2

untuple :: (Int, Int) -> [Int]
untuple (a, b) = [a, b]

--CHANGE INPUTS [] AT FUNCTIOM, USING MAP IN THE CALLIN AVOID THAT
compareShipVector :: [(Int , Axis)] -> Ship s -> [(Bool,Axis)]
compareShipVector [] _ = []
compareShipVector ((len,a) : tail) s = (maxSize <= len, a) : compareShipVector tail s
        where
                maxSize = maximum.untuple $ getSizes s

availableSpace :: (Coordinate, Axis) ->  Board b -> Bool 
availableSpace ((r, c), a) (Board _ b) = 
        case a of
                Xp -> foldr (||) False (drop c (b !! charNum)) 
                Xn -> foldr (||) False (take (c+1) (b !! charNum))
                Yn -> foldr (||) False (drop charNum (transposeBoard !! c))
                Yp -> foldr (||) False (take (charNum+1) (transposeBoard !! c))
                where
                        charNum = fromEnum r - 65
                        transposeBoard = transpose b

------------------------TEST
b3 =[[False, False, False, True, False],
     [False, False, False, False, False],
     [False, False, False, False, True],
     [False, False, False, False, False],
     [False, True, False, False, False],
     [False, False, False, False, False]]

availableXp = availableSpace (('C', 3), Xp) Board{getSquares = b3, getSize = 0}
availableXn = availableSpace (('C', 3), Xn) Board{getSquares = b3, getSize = 0}
availableYp = availableSpace (('C', 3), Yp) Board{getSquares = b3, getSize = 0}
availableYn = availableSpace (('C', 3), Yn) Board{getSquares = b3, getSize = 0}
----------------------FINIH TEST

-- SOLVE PROBLEM OF 2 TYPES OF RETURN
getAxisVectors :: (Coordinate, Axis) -> (Int, Board b)  -> [Coordinate]
getAxisVectors ((r, c), a) (len, BoardCoord s b ) = 
        case a of
                Xp -> take len (drop c takeRow)
                Xn -> drop (c+1-len) (take (c+1) takeRow) 
                Yn -> drop (c+1-len) (take (c+1) takeCol) 
                Yp -> take len (drop c takeRow)
                where
                        rowChar = fromEnum r - 65
                        transposeBoard = transpose b
                        takeRow = b !! rowChar
                        takeCol = transposeBoard !! c

b4 = BoardCoord{getSizeCoord = 5, getCoordSquares = makeMatrixCoord (5, 65)}
vectorXp= getAxisVectors (('B',2), Yn) (3,b4)