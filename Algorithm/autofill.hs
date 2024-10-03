import System.Environment (getArgs)

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
    dictbody <- readFile dictfile
    gridbody <- if "-" == gridfile
        then getContents
        else readFile gridfile
    -- TODO: Rest, Neo. The answers are coming.
    putStrLn ("Dict = ["++dictfile++"], Grid = ["++gridfile++"]")
    putStrLn ("Dict length = ["++(show (length (lines dictbody)))++"]")
    putStrLn ("Grid = ["++gridbody++"]")

--
-- Each slot contains
--
-- Coordinates x0,y0,x1,y1
-- A length (abs (x1-x0)) + (abs (y0-y1))
-- Fixed letters e.g.
--  E at zero
--  T at three
-- Intersecting slots e.g.
--  at position two, intersects SlotB at position one
--  at position four, intersects SlotC at position five
-- The list of candidates satisfying both this slot and intersecting slots
--  At "compile time", will be refined at runtime
--
-- 012 F
-- 1   A
-- 2   C
--     A
--   F D
-- ELITE
--   L
--   M
--


