{-# LANGUAGE InstanceSigs #-}
module BattleShip where
--types
type Size = Int
type RowIndex = Char
type ColIndex  = Int 
type Coordinate = (RowIndex, ColIndex)
type ShipSize = (Int, Int)
data Board b  =   Board{ getSize   :: Size,
                       getSquares :: [[Bool]]}
                | BoardCoord{getCoordSquares :: [[Coordinate]]} deriving(Show, Eq) 
                       
data Ship s   = Ship{ getSizes :: ShipSize,
                       getShipSquares :: [[Bool]]} deriving (Show, Eq)

listSizeShip = [(5,1),(2,2),(4,1),(2,1)] 
minBoardSize = 5

makeBoard :: Size  -> Board b
makeBoard s = Board{ getSize = smin
                    ,getSquares = replicate smin (replicate smin False)
                    }
            where smin = max s minBoardSize
                        -- maximun between s and minBoardSize
b1 = makeBoard 6
b1sq = getSquares $ makeBoard 4

makeShip :: ShipSize -> Ship s
makeShip (r, c) = Ship { getSizes = (nr,nc)
                    , getShipSquares = replicate nr (replicate nc True)
                    }
                where 
                        nr = max 1 r
                        nc = max 1 c

makeListShip :: [ShipSize] -> [Ship s]
makeListShip = map makeShip 

listships = makeListShip listSizeShip

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
-- (size Board, A::Int)
makeMatrixCoord ::(Int, Int) -> [[Coordinate]]
makeMatrixCoord (0,_) = []
makeMatrixCoord (x,90) = [[('Z', x)]]
makeMatrixCoord (x,n) = first : second
        where 
                first  = zip (replicate x (toEnum n :: Char)) [0.. x-1]
                second | (n-63) > x = makeMatrixCoord (0, n+1) --63 because at first time always gona be 2 rows 
                       | otherwise  = makeMatrixCoord (x, n+1)
b2 = BoardCoord{getCoordSquares = makeMatrixCoord (getSize b1, 65)}