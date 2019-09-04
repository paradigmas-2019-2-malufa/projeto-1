--Jokenpo--

import System.Random


optionList = ["pedra", "papel", "tesoura"]

getOption :: Int -> String
getOption x = optionList !! (x-1)



selectRandomElement :: [a] -> IO a
selectRandomElement [] = error "Cannot select an element from an empty list."
selectRandomElement list = randomIntWithinRange >>=
  \r -> return $ list !! r
  where
  randomIntWithinRange = getStdRandom $ randomR (0, length list - 1)