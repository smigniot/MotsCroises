import System.IO
import System.Environment (getArgs)
import Data.List (transpose, intercalate, sortBy, elemIndex, intersect, foldl')
import Data.List.Split (chunksOf)
import Data.Char (isSpace, toUpper)
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
    let (matrix,slots) = readGrid (map toUpper gridbody)
        constrained = constrainSlots slots
        dictionary = map (map toUpper) $ filter (not . null) (
            map trim $ lines dictbody )
        tree = makeTree dictionary
        frequencies = makeFrequencies tree
        in do
            if silent
            then pure ()
            else do
                putStrLn $ "Matrix :\n" ++ (intercalate "\n"
                    (chunksOf (w matrix) (V.toList (v matrix))))
                putStrLn $ "Dictionary : " ++ show (length dictionary)
                putStrLn $ "Slots : " ++ (intercalate ", "
                    (map ((showSlot (w matrix)) . fst) constrained))
                putStrLn ( "Tree : " ++ show (M.size tree) ++
                           " and Frequencies : " ++ show (M.size frequencies))
            startBacktrack matrix constrained dictionary tree frequencies silent

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

--
-- A multilevel word tree
--
-- 1. By word size    (25 max in FR)
-- 2. By position     (25 max)
-- 3. By letter       (26)
-- 4. A set of string (50364 max in FR)
--
type Tree = M.Map (Int, Int, Char) (S.Set String)
makeTree :: [String] -> Tree
makeTree words = let
    result = foldl' ingest M.empty words
    ingest m word = foldl' (ingest' word (length word)) m $ zip [0..] word
    ingest' word n m (pos,letter) = let
        key = (n, pos, letter)
        existing = M.findWithDefault S.empty key m
        in M.insert key (S.insert word existing) m
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
-- A frequency score tree
--
-- 1. By word size    (25 max in FR)
-- 2. By position     (25 max)
-- 3. By letter       (26)
-- 4. Frequency score (0.0 up to 1.0)
--
type Frequencies = M.Map (Int, Int, Char) Float
makeFrequencies :: Tree -> Frequencies
makeFrequencies tree = let
    -- for all (Int,Int,Char)
    --  divide the length of the S.Set String
    --  by the total count of all same (Int,Int)
    pairs = M.toList tree
    totals = foldl' reduceTotal M.empty pairs
    reduceTotal m ((n,pos,letter),s) =
        M.insert (n,pos) ((M.findWithDefault 0 (n,pos) m) + (S.size s)) m
    norm (key@(n,pos,letter),s) = let
        count = S.size s
        total = M.findWithDefault count (n,pos) totals
        frequency = (fromIntegral count) / (fromIntegral total)
        in (key, frequency)
    in M.fromList (map norm pairs)

--
-- Prepare the recursion
--
-- Remove slots already filled at launch time
-- Prepare an IORef for early exit when solved
-- Fire the recursion
--
startBacktrack :: Matrix -> [ConstrainedSlot] -> [String]
                  -> Tree -> Frequencies -> Bool -> IO()
startBacktrack matrix constrained dictionary tree frequencies silent = let
    remaining = filter (not . filled) constrained
    removed = filter filled constrained
    filled (slot, crossings) = all isLetter $ getWord slot matrix
    in do
        if (null removed) || silent
        then pure ()
        else putStrLn ("Removed : " ++ intercalate ", "
                (map ((showSlot (w matrix)) . fst) removed))
        solved <- newIORef False
        backtrack matrix remaining dictionary tree frequencies solved silent


--
-- Perform the recursion
--
-- While any slot is not filled
-- Choose the best slot : maximize known, minimize unknown,
--  maximize candidate length, maximize crossings
-- Compute the candidates with tree, Sort them by frequency score
-- Apply the candidate, recurse
--
backtrack matrix [] dictionary tree frequencies solved silent = do
    atomicWriteIORef solved True
    if silent
    then pure ()
    else putStrLn "Solution :\n"
    putStrLn (intercalate "\n" (chunksOf (w matrix) (V.toList (v matrix))))
backtrack matrix constrained@(c:cs) dictionary tree frequencies solved silent = do
    putStrLn "TODO: implement me"


--
-- Get the current state of a slot in a matrix
--
getWord :: Slot -> Matrix -> String
getWord slot matrix = map ((V.!) (v matrix)) slot

--
-- Belongs to ['A'..'Z']
--
isLetter c
    | (('A' <= c) && (c <= 'Z')) = True
    | otherwise = False


