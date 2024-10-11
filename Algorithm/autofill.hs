import System.IO
import System.Environment (getArgs)
import Data.List (transpose, intercalate, sortBy)
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
    let (matrix,slots,slotsAt) = readGrid gridbody
    putStrLn ("Slots = "++(intercalate ", " (map showSlot slots)))
    let tree = classify $ filter (not . null) $ map trim $ lines dictbody
    putStr "Computing word tree"
    hFlush stdout
    tree `seq` putStrLn " : Done"
    recurse matrix slots tree slotsAt

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
    (x0,y0) = head slot
    (x1,y1) = last slot
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
    slotsAt = foldr insertSlot M.empty slots
        where   insertSlot slot m = foldr (insertAt slot) m slot 
                insertAt slot (x,y) m = let
                    l = M.findWithDefault [] (x,y) m
                    in M.insert (x,y) (slot:l) m
    in (v,slots,slotsAt)

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

--
-- Recurse until one solution is found
--
-- matrix is the current state of the grid
-- slots is a list of non already filled slots
-- tree is the map<length, map<(position,letter), set<words>>>
-- slotsAt is map<(x,y), [slot]>
--
recurse matrix slots tree slotsAt = let
    annotate slot = foldr gather (slot,[],0,0,[]) (zip [0..] slot)
    gather (i,(x,y)) (original,l,known,missing,rules) =
        let cell = matrix V.! y V.! x
            l' = (x,y,cell):l
            rules' = (i,cell):rules
            in if cell == '.'
                then (original,l',known,missing+1,rules)
                else (original,l',known+1,missing,rules')
    -- slot becomes (slot, [(x,y,letter)..], known, missing, rules)
    annotated = sortBy slotHeuristic $ map annotate slots
    best@(chosen,state,_,_,patterns) = chooseSlot annotated
    candidates = candidatesFor tree (length chosen) patterns
    fromJust (Just e) = e
    sevene2 = S.size (fromJust (M.lookup (2,'E') (fromJust (M.lookup 7 tree))))
    tene9 = S.size (fromJust (M.lookup (9,'E') (fromJust (M.lookup 10 tree))))
    in do
        putStrLn ("Expect 1370 = " ++ show sevene2)
        putStrLn ("Expect 6423 = " ++ show tene9)
        putStrLn ("Chosen = " ++ show best)
        putStrLn ("Candidates = " ++ show (S.toList candidates))

--
-- Sort slots
--
-- Minimize the number of missing letters, but non-zero
-- Maximize the number of known letters
--
-- XXX: Allow tweaking this behaviour through options
--
slotHeuristic (_,_,known_a,missing_a,rules_a) (_,_,known_b,missing_b,rules_b)
    | missing_a == 0 = GT
    | missing_b == 0 = LT
    | missing_a < missing_b = LT
    | missing_a > missing_b = GT
    | known_a > known_b = LT
    | known_a < known_b = GT
    | otherwise = EQ

--
-- Choose a slot
--
-- Take the first, as sorted by the heuristic
--
-- XXX: Allow tweaking this behaviour through options
--
chooseSlot = head

--
-- Find candidates for the patterns
--
candidatesFor :: M.Map Int (M.Map (Int,Char) (S.Set String))
                 -> Int
                 -> [(Int,Char)]
                 -> S.Set String
candidatesFor tree n [] = candidatesFor tree n [(0,'.')]
candidatesFor tree n (p0:ps) = let
    bypos = M.findWithDefault M.empty n tree
    first = M.findWithDefault S.empty p0 bypos
    reduceSet p set = S.intersection set (M.findWithDefault S.empty p bypos)
    in foldr reduceSet first ps

