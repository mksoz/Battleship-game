{-# LANGUAGE InstanceSigs #-}
module BattleShip where

type Size = Int
type RowIndex = Char
type ColIndex  = Int 
type UserCoordinates = (RowIndex, ColIndex)
type ShipSize = (Int, Int)
data Board  b = Board{ getSize   :: Size,
                       getSquares :: [[Bool]]} deriving(Show, Eq)
data Ship s   = Ship{ getSizes :: ShipSize,
                       getShipSquares :: [[Bool]]} deriving (Show, Eq)

listSizeShip = [(5,1),(2,2),(1,1),(2,1)] 
minBoardSize = 5

makeBoard :: Size  -> Board b
makeBoard s = Board{ getSize = smin
                    ,getSquares = replicate smin (replicate smin False)
                    }
            where smin = max s minBoardSize
                        -- maximun between s and minBoardSize
b1 = getSquares $ makeBoard 4

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

checkSquares :: [Ship s] -> Board b -> (Bool, [Char])
checkSquares [] _ = (False, "No ships")
checkSquares ls b = case compare ((^) (getSize b) 2) (sum.getShipsArea $ ls) of
                     LT -> (False, "More ships area than board area")
                     EQ -> (False, "No water area")
                     GT -> (True, "More board than ships")
