import System.IO
import System.Environment (getArgs)
import Data.List (transpose, intercalate, sortBy)
import Data.List.Split (chunksOf)
import Data.Char (isSpace)
import Data.Time (getCurrentTime, NominalDiffTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.IORef (newIORef, readIORef, atomicWriteIORef)
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M

--
-- Run the autofill algorithm
--
main = do
    args <- getArgs
    if ("-h" `elem` args) || ("--help" `elem` args)
    then putStrLn "Usage: autofill [-d dictionary] [gridfile] [-h|--help]"
    else let (dictfile, gridfile) = parse defaultConfig args
        in autofill dictfile gridfile

--
-- The default configuration, stdin as grid input file
--
defaultConfig = ("dictionary.txt", "-")

--
-- Parse command line arguments
-- [-d dictionary]
-- [gridfile]
--
-- No more arguments
parse config [] = config
-- [-d dictionary] Replaces the dictionary file
parse (dict, grid) ("-d":newdict:xs) = parse (newdict,grid) xs
-- [gridfile] any additonal argument replaces the grid file
parse (dict, grid) (newgrid:xs) = parse (dict, newgrid) xs

--
-- The main function
--
-- 1. Reads every file
-- 2. Prepares pre-computed material from the dictionary
-- 3. Prepares the graph from the grid
-- 4. Runs the actual algorithm
--
autofill dictfile gridfile = do
    putStrLn ("Using dict = ["++dictfile++"], Grid = ["++gridfile++"]")
    dictbody <- readFile dictfile
    gridbody <- if "-" == gridfile then getContents else readFile gridfile
    let (width,height,matrix,slots) = readGrid gridbody
    putStrLn ("Grid ("++(show width)++"x"++(show height)++") =")
    printMatrix width matrix
    putStrLn ("Slots ("++(show $ length slots)++") = "
        ++(intercalate ", " (map (showSlot width) slots)))
    let tree = classify $ filter (not . null) $ map trim $ lines dictbody
    a <- getCurrentTime
    putStr "Computing word tree"
    hFlush stdout
    tree `seq` do
        b <- getCurrentTime
        let d = diffUTCTime b a
        let duration = formatTime defaultTimeLocale "%S" d
        putStrLn (" : Done in " ++ duration ++ "s")
    hFlush stdout
    -- TODO try AC-3
    --  Each slot has a domain definition (by default the words of length n)
    --  Apply all fixed letters to restrict the domain
    --  remaining = each pair of (slot1,slot2) or (slot2,slot1) that intersect
    --  while remaining
    --   arc(x,y) = remaining.pop()
    --   arc-reduce arc(x,y)
    -- see https://en.wikipedia.org/wiki/AC-3_algorithm

--
-- Drop space at start and at end
--
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

--
-- Print the slot
--
showSlot width slot = let
    n0 = head slot
    (x0,y0) = (n0 `mod` width, n0 `div` width) 
    n1 = last slot
    (x1,y1) = (n1 `mod` width, n1 `div` width) 
    in ("[" ++ (show x0) ++ "," ++ (show y0) ++
       "->" ++ (show x1) ++ "," ++ (show y1) ++ "]")

--
-- Prints a grid
--
printMatrix width matrix = do
    putStrLn ("┌" ++ (replicate width '─') ++ "┐")
    mapM (\s -> putStrLn ("│" ++ s ++ "│")) (chunksOf width $ V.toList matrix)
    putStrLn ("└" ++ (replicate width '─') ++ "┘")
    hFlush stdout

--
-- Clean n lines of the terminal
--
cleanScreen n = do
    mapM_ (\_ -> putStr "\ESC[F") [1..n]

--
-- Read the grid, returning
--
-- width and height,
-- the flattened vector of size width*height
-- and the list of slots
--
readGrid :: String -> (Int, Int, V.Vector Char, [[Int]])
readGrid txt = let
    m = map asRow (zip [0..] (lines txt))
    asRow (y,l) = map (asCell y) (zip [0..] l)
    asCell y (x, letter) = (x,y,letter)
    xySlots = findSlots (m ++ transpose m)
    width = foldr max 0 (map length m)
    height = length m
    blank = V.replicate (width * height) ' '
    v = (V.//) blank (map 
        (\(x,y,letter) -> (y*width+x, letter))
        (concat m))
    slots = map (map (\(x,y) -> (y*width+x))) xySlots
    in (width,height,v,slots)

--
-- A Slot is a contiguous serie of two or more cells
--
findSlots :: [[(Int,Int,Char)]] -> [[(Int,Int)]]
findSlots [] = []
findSlots (v:others) = findSlots' [] v ++ findSlots others
    where   findSlots' accumulated [] = twoOrMore accumulated
            findSlots' accumulated ((x,y,letter):others) =
                if letter `elem` " #"
                then twoOrMore accumulated ++ findSlots' [] others
                else findSlots' (accumulated++[(x,y)]) others
            twoOrMore (a:b:xs) = [(a:b:xs)]
            twoOrMore _ = []

--
-- Classify by length by letter at position
--
-- The result is a map<length, map<(position,letter), set<words>>>
--
classify :: [String] -> M.Map Int (M.Map (Int,Char) (S.Set String))
classify words = let
    ingest word tree = let
        n = length word
        bypos = M.findWithDefault M.empty n tree
        bypos' = foldr (ingest' word) bypos ((0,'.'):(zip [0..] word))
        in M.insert n bypos' tree
    ingest' word key bypos = let
        set = M.findWithDefault S.empty key bypos
        set' = S.insert word set
        in M.insert key set' bypos
    in foldr ingest M.empty words

