import System.IO
import System.Environment (getArgs)
import Data.List (transpose, intercalate, sortBy, elemIndex, intersect)
import Data.List.Split (chunksOf)
import Data.Char (isSpace)
import Data.Time (getCurrentTime, NominalDiffTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.IORef (newIORef, readIORef, atomicWriteIORef)
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M

--
-- Run the betterfill algorithm
--
main = do
    args <- getArgs
    if ("-h" `elem` args) || ("--help" `elem` args)
    then putStrLn ("Usage: betterfill [-d dictionary] [-q] [gridfile]")
    else let (Config dictfile gridfile silent) = (parse defaultConfig args)
         in betterfill dictfile gridfile silent

--
-- The default configuration, stdin as grid input file
--
data Config = Config {
    dictionaryConfig  :: String
  , gridConfig        :: String
  , silentConfig      :: Bool
}
defaultConfig = Config "dictionary.txt" "-" False

--
-- Parse command line arguments
-- [-d dictionary]
-- [-s]
-- [gridfile]
--
-- No more arguments
parse config [] = config
-- [-d dictionary] Replaces the dictionary file
parse config ("-d":newdict:xs) = parse (config {dictionaryConfig = newdict}) xs
-- [-q] disable interactive output
parse config ("-q":xs) = parse (config {silentConfig = True}) xs
-- [gridfile] any additonal argument replaces the grid file
parse config (newgrid:xs) = parse (config {gridConfig = newgrid}) xs

--
-- The main function
--
-- 1. Reads every file
-- 2. Prepares pre-computed material from the dictionary
-- 3. Runs the actual algorithm
--
betterfill dictfile gridfile silent = do
    dictbody <- readFile dictfile
    gridbody <- if "-" == gridfile then getContents else readFile gridfile
    let (matrix,slots) = readGrid gridbody
        constrained = constrainSlots slots
        in do
            putStrLn $ "Matrix :\n" ++ (intercalate "\n"
                (chunksOf (w matrix) (V.toList (v matrix))))
            putStrLn $ "Constrained : " ++ show constrained

--
-- An efficient grid holder
--
data Matrix = Matrix {
    w :: Int
  , h :: Int
  , v :: V.Vector Char
} deriving(Ord,Eq,Show)

--
-- A slot is a list of cells, by remapped coordinates
--
-- Remapping is coord(x,y) = y*width+x
--
type Slot = [Int]

--
-- Read the grid, returning
--
-- width and height,
-- the flattened vector of size width*height
-- and the list of slots
--
readGrid :: String -> (Matrix, [Slot])
readGrid txt = let
    m = map asRow (zip [0..] (lines txt))
    asRow (y,l) = map (asCell y) (zip [0..] l)
    asCell y (x, letter) = (x,y,letter)
    xySlots = findSlots (m ++ transpose m)
    width = foldr max 0 (map length m)
    height = length m
    blank = V.replicate (width * height) '#'
    v = (V.//) blank (map 
        (\(x,y,letter) -> (y*width+x, letter))
        (concat m))
    slots = map (map (\(x,y) -> (y*width+x))) xySlots
    matrix = Matrix width height v
    in (matrix,slots)

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
-- A crossing happens when slotA at positionA meets slotB at positionB
--
data Crossing = Crossing {
    posA  :: Int
  , slotB :: Slot
  , posB  :: Int
} deriving(Ord,Eq,Show)

--
-- A slot and its crossing
--
type ConstrainedSlot = (Slot, [Crossing])

--
-- Compute slot crossings
--
constrainSlots :: [Slot] -> [ConstrainedSlot]
constrainSlots slots = map (constrainSlot slots) slots
constrainSlot all slot = let
    others = filter ((/=) slot) all
    withpos = map findPos others
    findPos other = let
        common = intersect other slot
        crosses = not (null common)
        fromJust (Just a) = a
        e = head common
        posA = fromJust $ elemIndex e slot
        posB = fromJust $ elemIndex e other
        in if crosses
            then Just (Crossing posA other posB)
            else Nothing
    collect (Just a) l = a:l
    collect Nothing l = l
    in (slot, foldr collect [] withpos)


-- Under refactoring

type Frequencies = M.Map (Char, Int, Int) Float

computeFrequencies :: [String] -> Frequencies
computeFrequencies dict = let
    counts = foldr ingestword M.empty dict
    ingestword word m = foldr (ingestletter (length word)) m (zip [0..] word)
    ingestletter size (position, letter) m = let
        bysize = M.findWithDefault M.empty size m
        bypos = M.findWithDefault M.empty position bysize
        count = M.findWithDefault 0 letter bypos
        in M.insert size (M.insert position (M.insert letter (count+1) bypos) bysize) m
    result = M.fromList (concat (map normalize (M.toList counts)))
    normalize (size,bysize) = concat (map (normalize' size) (M.toList bysize))
    normalize' size (pos,bypos) = let
        total = sum $ M.elems bypos
        bypos' = map (\(letter,n) -> ((letter,pos,size), (fromIntegral n) / (fromIntegral total))) $ M.toList bypos
        in bypos'
    in result

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
type Matrix' = (Int, Int, V.Vector Char)

--
-- Performs unary constraint of AC-3 algorithm
--
-- Each slot has a fixed length, reduce the domain of values
-- Each slot may have fixed letters, reduce the domain of values
--
-- see https://en.wikipedia.org/wiki/AC-3_algorithm
--
ac3Phase1 :: Matrix' -> [[Int]] -> Tree -> (Bool,Bool,Bool,Bool) -> Frequencies -> IO()
ac3Phase1 m slots tree options frequencies = let
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
    in ac3Phase2 m variables tree options frequencies

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
ac3Phase2 :: Matrix' -> [Variable] -> Tree -> (Bool, Bool, Bool, Bool) -> Frequencies -> IO()
ac3Phase2 m' vars tree (verbose,silent,bypassac3,nosort) frequencies = let
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
    domains' = if bypassac3
        then domains
        else ac3Phase3 worklist domains worklist
    reduced = map addReduced crossings
    material = sortBy domainSize (
        map ( \(slot, _, domain, crossings) -> (slot, S.toList domain, (
            map fst crossings ))) reduced )
    domainSize (_,d1,_) (_,d2,_)
        | length d1 < length d2 = LT
        | otherwise = GT
    -- slot SRC at position pSrc meets [slot TGT at position pTgt]
    constraints = mapWithPositions material
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
        constraints `seq` startBacktrack m' material (verbose,silent,nosort) frequencies constraints

mapWithPositions :: [([Int],[String],[[Int]])] -> M.Map [Int] [(Int,[Int],Int)]
mapWithPositions material = let
    paired = map annotatePosition material
    annotatePosition (slot,_,others) = (slot, map (toPos slot) others)
    fromJust (Just a) = a
    toPos slot other = let
        common = head $ filter (`elem` slot) other
        p1 = fromJust (elemIndex common slot)
        p2 = fromJust (elemIndex common other)
        in (p1, other, p2)
    in M.fromList paired

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
naiveCandidateSort frequencies candidates (width,height,matrix) (slot, _, crossings) = candidates

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

betterCandidateSort frequencies constraints candidates (width,height,matrix) (slot, _, crossings) = let
    crossers = M.findWithDefault [] slot constraints
    scoreOf candidate = let
        triplet (p1,other,p2) = ((candidate !! p1), p2, length other)
        triplets = map triplet crossers 
        l = map (\key -> M.findWithDefault 0.0 key frequencies) triplets
        in sum l
    scored = map (\candidate -> (candidate, scoreOf candidate)) candidates
    sorted = sortBy compareScore scored
    compareScore (ca,sa) (cb,sb) = compare sb sa
    in map fst sorted

--
-- Now backtrack
--
startBacktrack (width,height,matrix) material (verbose,silent,nosort) frequencies constraints = do
    last <- newIORef =<< getCurrentTime
    solved <- newIORef False
    let candidatesBySlot = M.fromList (map (\(slot,candidates,_) -> (slot,candidates)) material)
    let sortFunc = if nosort
        then naiveCandidateSort frequencies
        else betterCandidateSort frequencies constraints
    if not silent then printMatrix width matrix else pure ()
    backtrack (width,height,matrix) material (verbose,silent) last solved betterChooseSlot sortFunc candidatesBySlot

{-# SCC backtrack #-}

backtrack (width,height,matrix) [] (verbose,silent) last solved chooseSlot candidateSort candidatesBySlot = do
    printMatrix width matrix
    atomicWriteIORef solved True
    return ()

backtrack (width,height,matrix) material (verbose,silent) last solved chooseSlot candidateSort candidatesBySlot = do
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
    let candidates' = candidateSort candidates (width,height,matrix) m0
    (flip mapM_) candidates' ( \candidate -> do
        let compatible = all (\(i,c) -> (c == (candidate !! i))) actual
        let matrix' = (V.//) matrix (zip slot candidate)
        let blocker = any (hasNoRemaining material candidatesBySlot (width,height,matrix')) crossings
        if (not compatible) || blocker
        then pure ()
        else do
            isSolved <- readIORef solved
            if isSolved
            then pure ()
            else do
                backtrack (width,height,matrix') (material') (verbose,silent) last solved chooseSlot candidateSort candidatesBySlot
        )
    
hasNoRemaining :: [([Int],[String],[[Int]])] -> M.Map [Int] [String] -> (Int,Int,V.Vector Char) -> [Int] -> Bool
hasNoRemaining material candidatesBySlot (width,height,matrix) slot = let
    candidates = M.findWithDefault [] slot candidatesBySlot
    isSlot (a,_,_) = a == slot
    actual = filter (isLetter . snd) $ zip [0..] $ map ((V.!) matrix) slot
    hasone = any isCompatible candidates
    isCompatible candidate = all (\(i,c) -> (c == (candidate !! i))) actual
    in not hasone


