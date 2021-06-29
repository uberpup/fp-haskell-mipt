import Data.Char
import System.IO
import Text.Read

-- task 42

getNat :: String -> IO Int
getNat messg = do putStr messg
                  txt <- getLine
                  case readMaybe txt :: Maybe Int of
                      Nothing -> invalid
                      Just n -> if n > 0 then return n else invalid
               where invalid = do putStrLn "Invalid input"
                                  getNat messg

getInt :: IO Int
getInt = do text <- getLine
            case readMaybe text :: Maybe Int of
                Nothing -> do putStrLn "Invalid input"
                              getInt
                Just n -> return n

counter :: IO ()
counter = do n <- getNat "How many numbers?"
             ints <- sequence (replicate n getInt)
             mode <- getNat "Input 1 for sum or other number for product"
             if mode == 1
                then putStrLn ("The total is " ++ show (sum ints))
                else putStrLn ("The total is " ++ show (product ints))
           

-- task 46

getInt2 :: IO (Maybe Int)
getInt2 = do text <- getLine
             case readMaybe text :: Maybe Int of
                 Nothing -> do putStrLn "Invalid input, aborting..."
                               return Nothing
                 Just n -> return (Just n)

int_reader :: IO ()
int_reader = do putStrLn "Input number: "
                val <- getInt2
                if val == Nothing
                    then return ()
                    else int_reader

-- task 43

str_end :: IO ()
str_end = str_reader ""

str_reader :: String -> IO ()
str_reader str = do putStrLn "Input string or 'END' to end this"
                    input <- getLine
                    if input == "END"
                       then putStrLn str
                       else if input > str then str_reader input
                       else str_reader str

-- task 44

inputToLower :: IO()
inputToLower = do eof <- isEOF
                  if not eof then
                     do line <- getLine
                        putStr $ map (\ch -> if isLower ch then (toUpper ch) else (toLower ch)) line
                        inputToLower
                  else
                    return ()
