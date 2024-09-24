# Crosswords generation

This document describes one possible algorithm to generate a filled crossword grid from a starting template

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

**_NOTE:_**  This program intentionaly does not ship with any official dictionary, say the Collins or the Larousse, because it allows the user to remove unwanted words - Allowing to create (Ex1) a dictionary tailored for kids - the author usecase, (Ex2) A conjugation-free dictionary or (Ex3) An animals only dictionary

## Running the program

Prepare the prerequisites :
* A `dictionary.txt` file in the current folder
* An input `template.txt` file

Then run `cat template.txt | node generate.js` . Hopefully the program will terminate with a filled grid on standard output.

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

TODO





