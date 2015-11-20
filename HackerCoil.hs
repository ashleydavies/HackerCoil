import Network.HTTP
import Data.List.Split
import Data.List

main :: IO ()
main = do
  putStrLn "All data is transmitted over unencrypted HTTP. Please be aware of your network security!"
  putStrLn "Enter username"
  name <- getLine
  putStrLn "Enter password"
  pass <- getLine
  let req = "http://www.hacker.org/coil/index.php?name="
          ++ name ++ "&password=" ++ pass
  egg <- ((simpleHTTP (getRequest req)) >>= getResponseBody)
  putStrLn egg

getFlashVars :: String -> String
getFlashVars = (flip (!!)) 1 .
                  splitOn "'" .
                  head .
                  filter (isPrefixOf "FlashVars") .
                  map (dropWhile (\x -> x == ' ')) .
                  lines
