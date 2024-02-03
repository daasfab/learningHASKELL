import System.Environment

-- fun. 1 - takes str containging a file path for  maze and returns representtion of that maze as a list of str
get_maze :: String -> IO [String] --e.g., "get_maze "M:\\maze2.txt" returns ["#####","# #","# # #","# # #","#####"]
get_maze location = 
    do 
        x <- readFile ( location )
        return ( lines ( x ))

-- fun. 2 - takes a maze an prints it out to screen as a full maze
print_maze :: [String] -> IO ()
print_maze m = 
    do 
        putStrLn ( unlines m )

-- fun. 3 - takes a maze and pair of int, returning True if the tile at that position is a wall(represented by #), and False if its a corridor represented by''. Coords are given as pairs (x,y)
is_wall :: [String] -> (Int, Int) -> Bool
is_wall m (x, y) = 
    do 
        if get m x y == '#' then True else False

-- fun. 4 - takes a maze and pair of coords, returning a new maze with @ sumbol at those coords. 
place_player :: [String] -> (Int, Int) -> [String]
place_player m (x, y) = set m x y '@'

-- fun. 5 - takes pair of coords., and a char, returning a pair of coords moved in the appropriate direction. If char is not WASD then coords is left unchanged
move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) char 
    | char == 'w' = (x, (y - 1))
    | char == 's' = (x, (y + 1))
    | char == 'a' = (x - 1, y )
    | char == 'd' = (x + 1 , y)
    | otherwise = (x, y)

-- fun. 6 - takes a maze, current pos. of the player, direction character (WASD), and returns True if the player can walk in that direction, False if otherwise
can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move m (x, y) char = not $ is_wall m ( move (x, y) char ) -- using NOT because it outputed False to where I can go and True to where I couldnt due to the is_wall function

-- fun. 7 - - takes previous functions and now allows WASD input, moving @ in that direction (if next coord is not a wall)
game_loop :: [String] -> (Int, Int) -> IO ()
game_loop m (x, y) =
    do 
        print_maze ( place_player m (x, y) )
        userInput <- getLine
        if can_move m (x, y) (head userInput) == True then game_loop m (move (x, y) (head userInput))
        else game_loop m (move (x, y) 'q') 

-- fun. 8 - returns the path from start coord to target coords in a given maze               Not working yet!!
get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path m currentCoord goalCoord 
    | currentCoord /= goalCoord = 
            let outputList = reverse (listCreator m currentCoord [] )
            in  get_path m (move currentCoord (last (availablePaths m currentCoord )) ) goalCoord 
    | otherwise = reverse (listCreator m currentCoord [] )
    
listCreator m currentCoord path = currentCoord : tupleToList (move currentCoord (last (availablePaths m currentCoord )) ) ++ path



--Helper Function 1 (finds clear paths (no walls) around the player, outputs a char or string of chars where player can go)
availablePaths m (x,y) = filter (\char -> can_move m (x,y) char == True ) ['w', 'a', 's', 'd']

--Helper Function 2 (Frontier finder function, outputs coords of available paths around the player's current coordinate) 
availablePathsCoords m (x,y) = map (\ char -> move (x,y) char) (availablePaths m (x,y) ) --moves player to coord of path

--Helper Function 3 (Converts puts tuple into a list so that I could use the cons (:) operator in listCreator without it giving me a type error )
tupleToList coord = [coord]

--Helper Function 4 (Allows me to perform 2 functions at the same time (listCreator and make a recursive call of get_path)

--Helper Function 5 (Creates and modifies frontier list)
frontierMaker m currentCoord list = currentCoord : tupleToList (newCurrentCoord m currentCoord) ++ list

--Helper Function 6 (Returns value of the new current coordinate to the player, to be used in frontierMaker)
newCurrentCoord m currentCoord = move currentCoord (last (availablePaths m currentCoord )) 


-- fun. 9
main :: IO ()
main = error "Not implemented"

