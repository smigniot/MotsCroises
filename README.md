# Crosswords generation

This document describes one possible algorithm to generate a filled crossword grid from a starting template

## Quickstart, in NodeJS

* Ensure you are using NodeJS 22 or above
* From the `Work` directory, run `cat complex.txt | node generate.js`

![Demonstration](Demonstration.gif)

## Quickstart, in Haskell

* Tested with GHC 9.4.8+, Cabal 3.10+
* From the `Algorithm` directory, run `cabal run -- betterfill -d ../dict_fr.txt ../Data/complex.txt`

![Demonstration](Demonstration2.gif)

## The Starting template

A starting template must be provided, containing the following
* It is a CARRIAGE RETURN delimited text file, where each line has the same length
* Each line contains only the following characters :
	* `.` : the DOT character : it represents an empty cell in the crossword grid that the program intents to fill
	* `#` : the HASH character : it represents a black cell in the crossword grid
	* ` ` : the SPACE character : it represents an unused cell, the program will treat them as black cells
	* `A...Z` : Any of the 26 uppercase letters of the usual ASCII alphabet

A quick input template example :
````
#C.....
CHIEN#.
.A.....
.T.....
.S#....
````

A more fancy example - inspired from [A dog's day from Gnome Crosswords](https://blogs.gnome.org/jrb/2021/11/18/introducing-gnome-crosswords/) :
````
  ## ##### ##  
 #HA#.....#..# 
#.I...........#
#.C#.......#..#
#.##.#...#.##.#
## #.#...#.# ##
   #.......#   
   #..###..#   
    #..#..#    
     #####     
````

## The dictionary

This program supports any Latin-derived dictionary, that is any dictionary text file meeting the following conditions :
* It is a text file containing one word per line
* Each line contains only `A...Z` characters

A quick example for French :
````
AAAI
ABAISSA
ABAISSABLE
...
ZYTHONS
ZYTHUM
ZYTHUMS
````

One may generate a dictionary.txt file using `aspell` for any language, by replacing `fr` below with the language codename, e.g. `en` for english :
````
#!/bin/bash

aspell dump master fr \
    | iconv -f utf8 -t ascii//TRANSLIT//IGNORE \
    | tr '[:lower:]' '[:upper:]' \
    | grep -v '-' \
    | grep -v "'" \
    | tr -d "^'\`\"" \
    | grep -v '^.$' \
    | tee >( sort | uniq > dictionary.txt )
````

**_NOTE:_**  This program intentionaly does not ship with any official dictionary, say the Collins or the Larousse, because it allows the user to remove unwanted words - Allowing to create (Ex1) a dictionary tailored for kids - the author usecase, (Ex2) A conjugation-free dictionary or (Ex3) An animals only dictionary (Ex4) A non-nsfw dictionary

## Running the program

Prepare the prerequisites :
* A `dictionary.txt` file in the Work folder
* An input `template.txt` file

Then run `cat template.txt | node generate.js` from `Work` or `cabal run -- betterfill -d ../dict_fr.txt template.txt` from `Algorithm`. Hopefully the program will terminate with a filled grid on standard output.

**_NOTE:_**  The average user can stop reading here. The next chapters describe the algorithm used by this program to try and fill the grid in a reasonable time - give it 200 seconds maximum.

# Crosswords generation algorithm

## 1. Build a pattern tree

Statement of the problem : finding word candidates with fixed letters. For instance in french consider the following (part of a) grid :

<table border="1"><tr>
<td>.</td><td>R</td><td>O</td><td>.</td><td>S</td><td>.</td><td>.</td>
</tr></table>

The following regex `/^.RO.S..$/` applied to the dictionary will perfectly compute the candidate set, including but not limited to CROISER, CROISES, CROISEZ. The problem is :
1. Applying a general regex is **slow**, especially compared to walking character by character and compare by hand (in a sufficiently low level language or environment e.g. C or asm)
2. Scanning the entire dictionary is notoriously **slow**, there are 1755 french words of length 7 with a `R` as a second letter but there are 229353 words in the same dictionary. Let's precompute them.

To speed up candidate words search the following structure helps :
* The structure is a [Map](https://en.wikipedia.org/wiki/Associative_array)
	* The key is the word length N
	* The value is another Map
		* Which key is the one-letter regex, i.e. a fixed letter L at a fixed position P
		* And which value is the [Set](https://en.wikipedia.org/wiki/Set_%28abstract_data_type%29) of words of length N which contain the letter L at position P
````
Map
├──2->Map
│     ├──A.->Set(AC,AH,AI,AL,AN,AS,AU,AV)
│     ├──B.->Set(BD,BU)
│     ├──C.->Set(...)
│     ├──...
│     ├──.Z->Set(DZ,HZ)
├──3->Map
│     ├──A..->Set(ACE,...,AXE)
│     ├──B..->Set(BAC,...,BYE)
│     ├──...
├──...
├──7->Map
│     ├──...
│     ├──.R.....->Set(...,CROISER,CROISES,CROISEZ,...)
│     ├──...
│     ├──..O....->Set(...,CROISER,CROISES,CROISEZ,...)
│     ├──...
│     ├──....S..->Set(...,CROISER,CROISES,CROISEZ,...)
│     ├──...
├──...
├──25->Map
│     ├──A........................->Set(ANTICONSTITUTIONNELLEMENT)
│     ├──...
````

Using this structure, finding a 7 letters word matching `.RO.S..` is
* Finding the 3 sets matching `.R.....`, `..O....` and `....S..`
* Performing the intersection of those sets

This structure allows performant search of words matching patterns - and is a classic memoization

## 2. Enumerate the word slots

In crossword grids, the slots where word are actually placed are defined by
* Any contiguous serie of non-black cells, of length at least two
* In this program, it is any serie of non HASH or SPACE of two or more
	* Thus looking for both DOT or a LETTER

As this program will have to iterate through those slots a lot, let's precompute them - instead of performing this static work every time.

Considering the grid :
````
ALPIN.
R....#
C#.#..
H.....
E...#.
````

The 13 slots are :
````
01  From 0,0 to 5,0 - contains ALPIN.
02  From 0,1 to 4,1 - contains R....
03  From 4,2 to 5,2 - contains ..
04  From 0,3 to 5,3 - contains H.....
05  From 0,4 to 3,4 - contains E...
06  From 0,0 to 0,4 - contains ARCHE
07  From 1,0 to 1,1 - contains L.
08  From 1,3 to 1,4 - contains ..
09  From 2,0 to 2,4 - contains P....
10  From 3,0 to 3,1 - contains I.
11  From 3,3 to 3,4 - contains ..
12  From 4,0 to 4,3 - contains N...
13  From 5,2 to 5,4 - contains ...
````

If the word `RIMEZ` is tried at slot 02, it creates two impossible situations :
* slot 09 now contains `PM...`
* slot 12 now contains `NZ..`

Each time a candidate is tried at a slot, it can be eliminated if it creates a lack of candidates for any of the impacted slot.

## 3. Recursion

Let's state the algorithm like that
1. Start from the (parsed) template grid
2. Find the slots containing at least one blank
3. Choose one of those slots wisely
	1. Maximize the number of known letters - fill slots near to previous
       populated slots
    2. Minimize the number of candidates for the slot
    3. Minimize the number of unknown letters
    4. Maximize length - try long words early in depth search
    5. It's a **heuristic** at this point - the goal being to avoid costly branches
4. For the chose slot, sort candidates
    1. Compute the frequency of each letter in crossing slots
    2. Add all this frequency to get a score
    3. Basically this maximizes the chance of having candidates in crossing slots
    4. It's a **heuristic** at this point - the goal being to avoid costly branches
    
4. Iterate all candidates
	1. Place the candidate in the slot, modifying the grid
	2. Check every crossing slot containing a modified cell of the grid
		1. If some word still can be placed at this crossing slot, skip
		2. Else it's a dead-end, eliminate the candidate
	3. If the candidate has not been eliminated, perform a recursion, at step 2
	4. After recursion, restore the grid by canceling modifications of step 4.1
5. When no slot has at least one empty cell, it's a solution. Claim victory

**_NOTE:_** To iterate the candidate, use the pattern tree described in the previous chapters.

## 4. Haskell version options

`Usage: autofill [-d dictionary] [-b] [-v] [-q] [-n] [-h|--help] [gridfile]`

* `-d dictionary` use another dictionary file than `./dictionary.txt`
* `-b` bypass the AC-3 algorithm. AC-3 speeds a lot by pre-elminiating candidates if the grid is at least 1/3 filled. Disable on a nearly empty grid.
* `-v` be verbose. Expect debugging information
* `-q` be silent. No animation, only output a solution
* `-n` don't sort candidates. Can speed up a little on grids with a high density of black cells
* `-h` output usage help
* `gridfile` a filename or `-` for `stdin`. Defaults to `stdin`

`Usage: betterfill [-d dictionary] [-b] [-v] [-q] [-n] [-h|--help] [gridfile]`

* `-d dictionary` use another dictionary file than `./dictionary.txt`
* `-q` be silent. No animation, only output a solution
* `-h` output usage help
* `gridfile` a filename or `-` for `stdin`. Defaults to `stdin`

