import Network.HTTP
import Data.List.Split
import Data.List

type GameBoardTile = Bool
type GameBoard     = [[GameBoardTile]]
type GameData      = (Int, Int, GameBoard)
type BoardPosition = (Int, Int)
data Direction     = U | D | L | R

main :: IO ()
main = do
  putStrLn "All data is transmitted over unencrypted HTTP. Please be aware of your network security!"
  putStrLn "Enter username"
  name <- getLine
  putStrLn "Enter password"
  pass <- getLine
  let req = "http://www.hacker.org/coil/index.php?name="
          ++ name ++ "&password=" ++ pass
  response <- ((simpleHTTP (getRequest req)) >>= getResponseBody)
  putStrLn $ show $ parseFlashVars $ getFlashVars response

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
  = [(x, y) | (xs, y) <- zippedList, (x, val) <- xs, val == True]
  where
    zippedList = zip (map (zip [0..]) board) [0..]


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


doStep :: GameBoard -> BoardPosition -> Direction -> GameBoard
doStep board (x, y) dir
  | validMove = board 
  | otherwise = board
  where
    validMove   = validPosition board newPosition
    newPosition = case dir of U -> (x, y - 1); D -> (x, y + 1)
                              L -> (x - 1, y); R -> (x + 1, y)


doLeftStep :: GameBoard -> BoardPosition -> GameBoard
doLeftStep board (x, y)
  | y == 0    = board
  | otherwise = board
  where
    moveValid = True

