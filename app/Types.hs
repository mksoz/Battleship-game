
module Types where

type Size = Int
type Coordinate = (Char, Int)
type Row      = [Icon]
type Squares  = [Row]
data Icon = D  | W | S  deriving(Eq, Read, Show)

data Axis = Xp | Xn | Yp | Yn deriving(Show, Eq, Ord)
data Board = Board{ getSize   :: Size,
                       getSquares :: [[Bool]]}
                | BoardCoord{ getSizeCoord   :: Size, getCoordSquares :: [[Coordinate]]} deriving(Show, Eq) 
                    
data Ship = Ship{ getSizes :: Size,
                       getShipSquares :: [Bool]} deriving (Show, Eq)
data BoardColor = BoardColor{ getLength    :: Int
                   , getIcons :: Squares
                   }deriving(Show)

listSizeShip = [5,3,4,2] 
minBoardSize = 5 :: Int
maxBoardSize = 25 ::Int
maxShipSize = 5 :: Int
minShipSize = 1 :: Int
maxTries = 1000 :: Int
symbol       = '*'
addBlue      = "\ESC[36m" 
addRed       = "\ESC[35m"
addDefault   = "\ESC[0m"
