import System.Environment (getArgs)
import Data.List (transpose, intercalate)
import Data.Char (isSpace)
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
    putStrLn ("Dict length = ["++(show (length (lines dictbody)))++"]")
    putStrLn ("Grid = ["++gridbody++"]")
    let (v,slots) = readGrid gridbody
    putStrLn ("Slots = "++(intercalate ", " (map showSlot slots)))
    let tree = classify $ filter (not . null) $ map trim $ lines dictbody
    let fromJust (Just e) = e
    let sevene2 = S.size (fromJust (M.lookup (2,'E') (fromJust (M.lookup 7 tree))))
    let tene9 = S.size (fromJust (M.lookup (9,'E') (fromJust (M.lookup 10 tree))))
    putStrLn ("Expect 1370 = " ++ show sevene2)
    putStrLn ("Expect 6423 = " ++ show tene9)

--
-- Drop space at start and at end
--
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

--
-- Print the slot
--
showSlot slot = let
    (x0,y0,_) = head slot
    (x1,y1,_) = last slot
    in ("[" ++ (show x0) ++ "," ++ (show y0) ++
       "->" ++ (show x1) ++ "," ++ (show y1) ++ "]")

--
-- Read the grid
--
-- As a vector of Char
-- Plus a list of slots
--
readGrid txt = let
    m = map asRow (zip [0..] (lines txt))
    asRow (y,l) = map (asCell y) (zip [0..] l)
    asCell y (x, letter) = (x,y,letter)
    slots = findSlots (m ++ transpose m)
    v = V.fromList (map V.fromList (lines txt))
    in (v,slots)

--
-- A Slot is a contiguous serie of two or more cells
--
findSlots :: [[(Int,Int,Char)]] -> [[(Int,Int,Char)]]
findSlots [] = []
findSlots (v:others) = findSlots' [] v ++ findSlots others
    where   findSlots' accumulated [] = twoOrMore accumulated
            findSlots' accumulated ((x,y,letter):others) = 
                if letter `elem` " #"
                then twoOrMore accumulated ++ findSlots' [] others
                else findSlots' (accumulated++[(x,y,letter)]) others
            twoOrMore (a:b:xs) = [(a:b:xs)]
            twoOrMore _ = []

--
-- Classify by length by letter at position
--
classify :: [String] -> M.Map Int (M.Map (Int,Char) (S.Set String))
classify words = let
    ingest word tree = let
        n = length word
        bypos = case M.lookup n tree of
            Just existing -> existing
            otherwise -> M.empty
        bypos' = foldr (ingest' word) bypos (zip [0..] word)
        in M.insert n bypos' tree
    ingest' word key bypos = let
        set = case M.lookup key bypos of
            Just existing -> existing
            otherwise -> S.empty
        set' = S.insert word set
        in M.insert key set' bypos
    in foldr ingest M.empty words

