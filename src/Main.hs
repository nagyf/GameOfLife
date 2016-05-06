module Main where

import Control.Concurrent (threadDelay)
import Lib

-- | The entry point of the application
main :: IO ()
main = playGame seed
    where
        playGame :: Universe -> IO ()
        playGame u = do
            mapM_ putStrLn (prettyPrint u)
            putStrLn ""
            threadDelay oneSec
            playGame $ evolve u

-- | Returns 1 second in nanoseconds
oneSec :: Int
oneSec = 500000

seed :: Universe
seed = glider

toad :: Universe
toad = [(4,4), (5,4), (6,4), (3,5), (4,5), (5,5)]

glider :: Universe
glider = [(4,4), (5,5), (5,6), (4,6), (3,6)]

