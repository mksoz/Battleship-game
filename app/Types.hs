{-# LANGUAGE OverloadedStrings #-}
module Types where
--types
type Size = Int
type Coordinate = (Char, Int)


data Axis = Xp | Xn | Yp | Yn deriving(Show, Eq, Ord)
data Board = Board{ getSize   :: Size,
                       getSquares :: [[Bool]]}
                | BoardCoord{ getSizeCoord   :: Size, getCoordSquares :: [[Coordinate]]} deriving(Show, Eq) 
                       
data Ship = Ship{ getSizes :: Size,
                       getShipSquares :: [Bool]}
                | ShipCoord{getShipSizesCoord :: Size, getShipCoordSquares:: [Coordinate]} deriving (Show, Eq)

listSizeShip = [5,3,4,2] 
minBoardSize = 5
maxBoardSize = 25
maxShipSize = 5
minShipSize = 1
maxTries = 1000