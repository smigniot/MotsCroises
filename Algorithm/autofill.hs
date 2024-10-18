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
    then putStrLn ("Usage: autofill [-d dictionary] [-v] [-q]"
        ++ "-h|--help] [gridfile]")
    else let (dictfile, gridfile, verbose, silent) = parse defaultConfig args
        in autofill dictfile gridfile (verbose, silent)

--
-- The default configuration, stdin as grid input file
--
defaultConfig = ("dictionary.txt", "-", False, False)

--
-- Parse command line arguments
-- [-d dictionary]
-- [gridfile]
--
-- No more arguments
parse config [] = config
-- [-d dictionary] Replaces the dictionary file
parse (dict, grid, verbose, silent) ("-d":newdict:xs) = parse (newdict,grid,verbose, silent) xs
-- [-v] enable verbose output
parse (dict, grid, verbose, silent) ("-v":xs) = parse (dict,grid,True, silent) xs
-- [-q] disable interactive output
parse (dict, grid, verbose, silent) ("-q":xs) = parse (dict,grid,verbose, True) xs
-- [gridfile] any additonal argument replaces the grid file
parse (dict, grid, verbose, silent) (newgrid:xs) = parse (dict, newgrid, verbose, silent) xs

--
-- The main function
--
-- 1. Reads every file
-- 2. Prepares pre-computed material from the dictionary
-- 3. Prepares the graph from the grid
-- 4. Runs the actual algorithm
--
autofill dictfile gridfile (verbose,silent) = do
    if verbose then putStrLn (
        "Using dict = ["++dictfile++"], Grid = ["++gridfile++"]"
        ) else pure ()
    dictbody <- readFile dictfile
    gridbody <- if "-" == gridfile then getContents else readFile gridfile
    let (width,height,matrix,slots) = readGrid gridbody
    if verbose 
        then do
            putStrLn ("Grid ("++(show width)++"x"++(show height)++") =")
            printMatrix width matrix
            putStrLn ("Slots ("++(show $ length slots)++") = "
                ++(intercalate ", " (map (showSlot width) slots)))
        else pure ()
    let tree = classify $ filter (not . null) $ map trim $ lines dictbody
    tree `seq` do
        if verbose
            then do
                a <- getCurrentTime
                putStr "Computing word tree"
                hFlush stdout
                tree `seq` do
                    b <- getCurrentTime
                    let d = diffUTCTime b a
                    let duration = formatTime defaultTimeLocale "%S" d
                    putStrLn (" : Done in " ++ duration ++ "s")
                hFlush stdout
            else pure ()
    ac3Phase1 (width,height,matrix) slots tree (verbose,silent)

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
-- Hold the dictionary classifier
--
-- A map<length, map<(position,letter), set<words>>>
--
type Tree = M.Map Int (M.Map (Int,Char) (S.Set String))

--
-- Classify by length by letter at position
--
classify :: [String] -> Tree
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
-- isNonLetter
--
isLetter c
    | (('A' <= c) && (c <= 'Z')) = True
    | otherwise = False

--
-- Hold width, height and flattened vector
--
type Matrix = (Int, Int, V.Vector Char)

--
-- Performs unary constraint of AC-3 algorithm
--
-- Each slot has a fixed length, reduce the domain of values
-- Each slot may have fixed letters, reduce the domain of values
--
-- see https://en.wikipedia.org/wiki/AC-3_algorithm
--
ac3Phase1 :: Matrix -> [[Int]] -> Tree -> (Bool,Bool) -> IO()
ac3Phase1 m slots tree options = let
    (width, height, matrix) = m
    variables = map constrain slots
    constrain slot = let
        bypos = M.findWithDefault M.empty (length slot) tree
        keys = filter (isLetter . snd) $ zip [0..] $ map ((V.!) matrix) slot
        rules :: [(Int,Char)]
        rules = if null keys then [(0,'.')] else keys
        getset key = M.findWithDefault S.empty key bypos
        domain = foldr (\key -> S.intersection (getset key))
            (getset (head rules)) (tail rules)
        in (slot, domain)
    in ac3Phase2 m variables tree options

--
-- Hold list of positions in matrix, and set of fitting words
--
type Variable = ([Int], S.Set String)

--
-- Hold a target Variable along with its source entangled Variables
--
type Entangled = ([Int], S.Set String, [Variable])

--
-- Apply AC-3 binary constrains
--
-- For each pair of crossing slots (a,b), a has an influence on b
-- and, vice versa, b has an influence on a.
--
-- see https://en.wikipedia.org/wiki/AC-3_algorithm
--
ac3Phase2 :: Matrix -> [Variable] -> Tree -> (Bool, Bool) -> IO()
ac3Phase2 m' vars tree (verbose,silent) = let
    (width, height, matrix) = m'
    -- Find entanglements
    varsAt = foldr putAt M.empty vars
    putAt var@(positions, _) m = foldr (putAll var) m positions
    putAll var pos m = M.insert pos (var:(M.findWithDefault [] pos m)) m
    crossings = map addCrossings vars
    addCrossings :: Variable -> Entangled
    addCrossings var@(slot, domain) = (slot, domain, findCrossings var)
    findCrossings var@(slot,_) = filter ((/=) var) ( concat (
        map (\pos -> M.findWithDefault [] pos varsAt) slot ))
    -- as per https://en.wikipedia.org/wiki/AC-3_algorithm
    domains = M.fromList vars
    worklist = concat (map asWorks crossings)
    asWorks (target,_,others) = map (\(source,_) -> (source,target)) others
    domains' = ac3Phase3 worklist domains worklist
    reduced = map addReduced crossings
    material = sortBy domainSize (
        map ( \(slot, _, domain, crossings) -> (slot, S.toList domain, (
            map fst crossings ))) reduced )
    domainSize (_,d1,_) (_,d2,_)
        | length d1 < length d2 = LT
        | otherwise = GT
    addReduced (slot, originaldomain, crossings) = let
        domain = M.findWithDefault originaldomain slot domains'
        in (slot, originaldomain, domain, crossings)
    printCrossing (slot, domain, crossings) = let
        t1 = showSlot width slot
        t2 = show $ S.size domain
        t3 = intercalate ", " (map (showSlot width . fst) crossings)
        in putStrLn (t1 ++ " has " ++ t2 ++ " values and crosses " ++ t3)
    printWork (source, target) = let
        t1 = showSlot width target
        t2 = showSlot width source
        in putStrLn (t1 ++ " is impacted by " ++ t2)
    printReduced (slot, originaldomain, domain, crossings) = let
        t1 = showSlot width slot
        t2 = show $ S.size originaldomain
        t3 = show $ S.size domain
        in putStrLn (t1 ++ " had " ++ t2 ++ " values, now " ++ t3)
    in do
        domains' `seq` if verbose
            then do
                mapM_ printCrossing crossings
                putStr "Applying AC-3"
                hFlush stdout
                a <- getCurrentTime
                domains' `seq` do
                    b <- getCurrentTime
                    let d = diffUTCTime b a
                    let duration = formatTime defaultTimeLocale "%S" d
                    putStrLn (" : Done in " ++ duration ++ "s")
                hFlush stdout
                putStrLn "After AC-3"
                mapM_ printReduced reduced
            else pure ()
        startBacktrack m' material (verbose,silent)

--
-- Holds a binary constrinat between 2 slots
--
type Work = ([Int], [Int])

--
-- Holds all variable domains
--
type Domains = M.Map [Int] (S.Set String)

--
-- Take a work in worklist and reduce the slot domain
--
-- A work is (source slot, target slot) where source impacts (crosses) target
-- Find the position pSrc in source where target intersects
-- Find the position pTgt in target where source intersects
-- 
-- So for each word in target find a word in source which fits
-- else remove the word from the target domain
-- If changed, add the works from original where target is here, but not source
--
ac3Phase3 :: [Work] -> Domains -> [Work] -> Domains
ac3Phase3 original domains [] = domains
ac3Phase3 original domains ((source,target):worklist) = let
    position = head $ filter (`elem` target) source
    nSrc = fst . head $ filter ((==) position . snd) (zip [0..] source)
    nTgt = fst . head $ filter ((==) position . snd) (zip [0..] target)
    dSrc = M.findWithDefault S.empty source domains
    dTgt = M.findWithDefault S.empty target domains
    dTgt' = S.filter fitsSource dTgt
    fitsSource targetWord = let
        crossingLetter = targetWord !! nTgt
        hasLetter sourceWord = let
            sourceLetter = sourceWord !! nSrc
            in sourceLetter == crossingLetter
        in any hasLetter dSrc
    changed = dTgt /= dTgt'
    domains' = M.insert target dTgt' domains
    worklist' = (filter impacted original) ++ worklist
    impacted (a,b)
        | a == target = b /= source
        | b == target = a /= source
        | otherwise = False
    in if changed
        then ac3Phase3 original domains' worklist'
        else ac3Phase3 original domains worklist

threshold = 0.250

--
-- TODO: add options to tune the heuristics
--
naiveChooseSlot material (width,height,matrix) = head material
naiveCandidateSort candidates (width,height,matrix) = candidates

betterChooseSlot material (width,height,matrix) = let
    heuristic a@(slota,canda,crossa) b@(slotb,candb,crossb) = let
        worda = map ((V.!) matrix) slota
        wordb = map ((V.!) matrix) slotb
        knowna = length $ filter isLetter worda
        knownb = length $ filter isLetter wordb
        la = length worda
        lb = length wordb
        unknowna = la-knowna
        unknownb = lb-knownb
        ca = length crossa
        cb = length crossb
        result
            | knowna > knownb = LT
            | knowna < knownb = GT
            | (length canda) < (length candb) = LT
            | (length canda) > (length candb) = GT
            | unknowna < unknownb = LT
            | unknowna > unknownb = GT
            | ca > cb = LT
            | ca < cb = GT
            | otherwise = EQ
        in result
    in head $ sortBy heuristic material

--
-- Now backtrack
--
startBacktrack (width, height, matrix) material (verbose,silent) = do
    last <- newIORef =<< getCurrentTime
    solved <- newIORef False
    if not silent then printMatrix width matrix else pure ()
    -- backtrack (width,height,matrix) material (verbose,silent) last solved naiveChooseSlot naiveCandidateSort
    backtrack (width,height,matrix) material (verbose,silent) last solved betterChooseSlot naiveCandidateSort

backtrack (width,height,matrix) [] (verbose,silent) last solved chooseSlot candidateSort = do
    printMatrix width matrix
    atomicWriteIORef solved True
    return ()

backtrack (width,height,matrix) material (verbose,silent) last solved chooseSlot candidateSort = do
    if not silent
    then do
        now <- getCurrentTime
        before <- readIORef last
        let diff = diffUTCTime now before
        if diff > threshold
        then do
            cleanScreen (height+2)
            atomicWriteIORef last now
            printMatrix width matrix
        else pure ()
    else pure ()
    let m0 = chooseSlot material (width,height,matrix)
    let material' = filter ((/=) m0) material
    let (slot, candidates, crossings) = m0
    let actual = filter (isLetter . snd) $ zip [0..] $ map ((V.!) matrix) slot
    let candidates' = candidateSort candidates (width,height,matrix)
    (flip mapM_) candidates' ( \candidate -> do
        let compatible = all (\(i,c) -> (c == (candidate !! i))) actual
        let matrix' = (V.//) matrix (zip slot candidate)
        let blocker = any (hasNoRemaining material (width,height,matrix')) crossings
        if (not compatible) || blocker
        then pure ()
        else do
            isSolved <- readIORef solved
            if isSolved
            then pure ()
            else do
                backtrack (width,height,matrix') (material') (verbose,silent) last solved chooseSlot candidateSort
        )
    
hasNoRemaining :: [([Int],[String],[[Int]])] -> (Int,Int,V.Vector Char) -> [Int] -> Bool
hasNoRemaining material (width,height,matrix) slot = let
    found = filter isSlot material
    (_,candidates,_) = head found
    isSlot (a,_,_) = a == slot
    actual = filter (isLetter . snd) $ zip [0..] $ map ((V.!) matrix) slot
    hasone = any isCompatible candidates
    isCompatible candidate = all (\(i,c) -> (c == (candidate !! i))) actual
    in ((not . null) found) && (not hasone)


