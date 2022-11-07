## Battleship-game

# First step : Haskell tolchain installation
Follow recommended installation instructions:  
    https://www.haskell.org/downloads/
    
or just run in a terminal

    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh    

# Run cabal to start playing
Open a new terminal and go to the BattleShip folder, where must be the Haskell.cabal file. Run next command to start:
    
    cabal run

# Game rules
First of all set the board dimensions nxn. There is a minumum size (5) and a maximum (25) because coordinates go from A to a maximum of Z.

User choose the size of the ships and the system will place them automatically and randomly. The size of every ship is limited by the board. The system is set to make 1000 tries for placing a ship so, do not try to full the board.

Once is all the ships placed, the system will ask for coordinates. There are a number extra bullets, which depends on the holes filled with water so, take it in mind. If you choose wisely, you will sink everything. Let's try it! 