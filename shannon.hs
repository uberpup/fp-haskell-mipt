import System.Random

shannon :: IO ()
shannon = do str_reader [0.5, 0.5] 0

predict :: [Double] -> Char
predict probs | probs !! 0 >= probs !! 1 = '0'
              | otherwise = '1'

renewProbs :: [Double] -> Char -> Int -> [Double]
renewProbs probs input iters = do if input == '0' then
	                                 do let new_prob = ((probs !! 0) * fromIntegral(iters) / fromIntegral(iters  + 1)) + (1.0 / fromIntegral(iters + 1))
	                                    let old_prob = (probs !! 1) * fromIntegral(iters) / fromIntegral(iters  + 1)
	                                    new_probs <- [new_prob, old_prob]
	                                    return new_probs
	                              else
	                              	do let new_prob = ((probs !! 1) * fromIntegral(iters) / fromIntegral(iters  + 1)) + (1.0 / fromIntegral(iters + 1))
	                                   let old_prob = (probs !! 0) * fromIntegral(iters) / fromIntegral(iters  + 1)
	                                   new_probs <- [old_prob, new_prob]
	                                   return new_probs

str_reader :: [Double] -> Int -> IO ()
str_reader probs iters = do putStrLn "Input 0 or 1"
                            input <- getChar
                            putStrLn ""
                            let prediction = predict probs
                            let iters = iters + 1
                            putStrLn ("Prediction was" ++ " " ++ [prediction])
                            let new_probs = renewProbs probs input iters
                            str_reader probs iters
                            return ()
