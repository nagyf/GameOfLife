module Lib (
        Cell,
        Universe,
        evolve,
        prettyPrint
    ) where

import Data.List (nub)

-- | A life form, represented by its coordinates
type Cell = (Int, Int)

-- | The universe of the cells, represents the alive cells
type Universe = [Cell]

-- | Evolve the universe
evolve :: Universe -> Universe
evolve u = nub $ evolveLive u u ++ evolveDead u (deadCells u)
    where
        -- | Return the list of dead cells that can be evolved
        deadCells :: Universe -> Universe
        deadCells = nub . concatMap surroundingCells

        -- | Evolve the live cells
        evolveLive :: Universe -> Universe -> Universe
        evolveLive _ [] = []
        evolveLive orig (c:cs) =
            case length (neighbours c orig) of
                2 -> c:evolveLive orig cs
                3 -> c:evolveLive orig cs
                _ -> evolveLive orig cs

        -- | Evolve the dead cells
        evolveDead :: Universe -> Universe -> Universe
        evolveDead _ [] = []
        evolveDead orig (c:cs) =
            case length (neighbours c orig) of
                3 -> c:evolveDead orig cs
                _ -> evolveDead orig cs

-- | Return the coordinates of the surrounding cells of a cell
surroundingCells :: Cell -> [Cell]
surroundingCells c =
    let scs = surroundings c
    in nub [(x,y) | x <- scs, y <- scs]
    where
        surroundings (x, y) = (+) <$> [0, 1, -1] <*> [x, y]

-- | Return the neighbours of the cell from the universe
neighbours :: Cell -> Universe -> [Cell]
neighbours c = filter (isNeighbour c)
    where
        isNeighbour :: Cell -> Cell -> Bool
        isNeighbour c1 c2 = distance c1 c2 == 1

-- | Calculate the distance of 2 cells
distance :: Cell -> Cell -> Int
distance (c1,c2) (d1,d2) =
    let
        xDistance = abs (c1 - d1)
        yDistance = abs (c2 - d2)
    in
        max xDistance yDistance

-- | Return the bounds of the universe based on the cells in it
bounds :: Universe -> ((Int, Int), (Int, Int))
bounds u =
    let
        xs = map fst u
        ys = map snd u
        lower = (minimum xs, minimum ys)
        upper = (maximum xs, maximum ys)
    in
        (lower, upper)

-- | Pretty print the universe
prettyPrint :: Universe -> [String]
prettyPrint u =
    let (_, (x2,y2)) = bounds u
        rows = atLeast 10 $ y2 + 1
        cols = atLeast 10 $ x2 + 1
    in printUniverse u rows cols
    where
        printUniverse :: Universe -> Int -> Int -> [String]
        printUniverse _ 0 _ = []
        printUniverse uu rows cols =
            printUniverse uu (rows-1) cols ++ [printRow uu rows cols]

        printRow :: Universe -> Int -> Int -> String
        printRow uu rows cols = concat $ putCells uu rows cols

        putCells :: Universe -> Int -> Int -> [String]
        putCells _ _ 0 = []
        putCells uu r c = putCells uu r (c-1) ++ (if (c, r) `elem` uu then [" x "] else [" . "])

atLeast :: Int -> Int -> Int
atLeast m x = if x < m then m else x