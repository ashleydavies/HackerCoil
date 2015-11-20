import Network.HTTP
import Data.List.Split
import Data.List

type GameBoardTile = Bool
type GameBoard     = [[GameBoardTile]]
type GameData      = (Int, Int, GameBoard)

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

{-
splitEvery :: [a] -> Int -> [[a]]
splitEvery [] n = []
splitEvery li n = take n li ++ splitEvery (drop n li) n -}
