import Network.HTTP
import Data.List.Split
import Data.List
import Debug.Trace
import Control.Parallel

type GameBoardTile = Bool
type GameBoard     = [[GameBoardTile]]
type GameBoardStep = (GameBoard, BoardPosition)
type GameData      = (Int, Int, GameBoard)
type BoardPosition = (Int, Int)
data Path          = Node Direction Path | End
  deriving (Show, Eq)
data Direction     = U | D | L | R
  deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn "All data is transmitted over unencrypted HTTP. Please be aware of your network security!"
  putStrLn "Enter username"
  name <- getLine
  putStrLn "Enter password"
  pass <- getLine
  solve name pass

solve :: String -> String -> IO ()
solve usr pass = do
  let req = "http://www.hacker.org/coil/index.php?name="
          ++ usr ++ "&password=" ++ pass
  response <- ((simpleHTTP (getRequest req)) >>= getResponseBody)
  putStrLn "Got level!"
  let (_, _, board) = parseFlashVars $ getFlashVars response
  let req2 = req ++ gameToWebPath board
  response <- ((simpleHTTP (getRequest req2)) >>= getResponseBody)
  putStrLn "Solved level!"
  solve usr pass

getFlashVars :: String -> String
getFlashVars
  = (flip (!!)) 1 . splitOn "\"" . head . filter (isPrefixOf "FlashVars") .
        map (dropWhile (\x -> x == ' ' || x == '\t')) . lines

parseFlashVars :: String -> GameData
parseFlashVars vars
  = (width, height, board)
  where
    varSplit = splitOneOf "&=" vars
    width    = read $ varSplit !! 1
    height   = read $ varSplit !! 3
    board    = parseGameBoard (varSplit !! 5) width


parseGameBoard :: String -> Int -> GameBoard
parseGameBoard board width
  = chunksOf width (map (\x -> x == 'X') board)


getCandidateStartingPoints :: GameBoard -> [BoardPosition]
getCandidateStartingPoints board
  = snd $ unzip orderedZippedPoints 
  where
    zippedList = zip (map (zip [0..]) board) [0..]
    points = [(x, y) | (xs, y) <- zippedList, (x, val) <- xs, val == False]
    zippedPoints = zip (map (countNeighbors board) points) points
    orderedZippedPoints = filter (\(x, _) -> odd x) zippedPoints ++ filter (\(x, _) -> even x) zippedPoints
    

getBoardTile :: GameBoard -> BoardPosition -> GameBoardTile
getBoardTile board (x, y) = board !! y !! x


validPosition :: GameBoard -> BoardPosition -> Bool
validPosition board pos@(x, y)
  | x < 0                       = False
  | y < 0                       = False
  | y + 1 > length board        = False
  | x + 1 > length (board !! 0) = False
  | getBoardTile board pos      = False
  | otherwise                   = True


gameToWebPath :: GameBoard -> String
gameToWebPath board
  = "&path=" ++ path ++ "&x=" ++ show x ++ "&y=" ++ show y
  where
    ((x, y), pathP) = attemptComplete board
    path = collapsePath pathP

collapsePath :: Path -> String
collapsePath (Node d n) = (show d) ++ collapsePath n
collapsePath End        = ""


attemptComplete :: GameBoard -> (BoardPosition, Path)
attemptComplete board
  = findFirstValidPath startingPoints
  where
    startingPoints = getCandidateStartingPoints board
    solutionsP     = [ (pos, snd $ attemptComplete' (visitPosition board pos) pos) | pos <- startingPoints ]
    solutions      = [ sols | sols@(_, p) <- solutionsP, p /= End ]

    findFirstValidPath :: [BoardPosition] -> (BoardPosition, Path)
    findFirstValidPath [] = error ""
    findFirstValidPath ((pos@(x, y)):ps)
     -- | trace ("Checking pos " ++ show pos) False = error ""
      | valid     = (pos, path)
      | otherwise = findFirstValidPath ps
      where
        (_, path) = attemptComplete' (visitPosition board pos) pos
        valid     = path /= End 

attemptComplete' :: GameBoard -> BoardPosition -> (Bool, Path)
attemptComplete' board pos
 -- | trace ("Attempting " ++ show board ++ " from " ++ show pos) False = error ""
  | boardComplete board   = (True,  End)
  | lp /= pos && lT       = (True, Node L lP)
  | rp /= pos && rT       = (True, Node R rP)
  | up /= pos && uT       = (True, Node U uP)
  | dp /= pos && dT       = (True, Node D dP)
  | otherwise             = (False, End)
  where
    (lb, lp)  = doStep board pos L; (rb, rp) = doStep board pos R
    (ub, up)  = doStep board pos U; (db, dp) = doStep board pos D
    (lT, lP)  = attemptComplete' lb lp
    (rT, rP)  = attemptComplete' rb rp
    (uT, uP)  = attemptComplete' ub up
    (dT, dP)  = attemptComplete' db dp


boardComplete :: GameBoard -> Bool
boardComplete = and . concat


doStep :: GameBoard -> BoardPosition -> Direction -> GameBoardStep
doStep board pos@(x, y) dir
  | validMove = doStep visBoard newPosition dir
  | otherwise = (board, pos)
  where
    visBoard    = visitPosition board newPosition
    validMove   = validPosition board newPosition
    newPosition = case dir of U -> (x, y - 1); D -> (x, y + 1)
                              L -> (x - 1, y); R -> (x + 1, y)


visitPosition :: GameBoard -> BoardPosition -> GameBoard
visitPosition board (x, y)
  = above ++ (left ++ True:right):below
  where
    (above, (row:below)) = splitAt y board
    (left , (  _:right)) = splitAt x row


countNeighbors :: GameBoard -> BoardPosition -> Int
countNeighbors board (x, y)
  = sum [1 | ns <- [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1)], validPosition board ns]

