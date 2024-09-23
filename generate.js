const fs = require('fs');

/*
 * Read the dictionary
 *
 * One word per line. Split and trim.
 */
const words = fs.readFileSync("dictionary.txt",'utf8')
    .trim()
    .split(/[\r\n]/gm)
    .map(l=>l.trim())
    .filter(l=>l)
    ;
console.debug("Dictionary length :", words.length);

/*
 * Build a Map<Map<Set>>> finder
 *
 * Key 1 : word length
 * Key 2 : letter and position
 * Value : set of words matching
 *
 * Example : the word APOSTROPHE
 * is of length 10
 * and matches A????????? ?P???????? ... ?????????E
 * 
 * It ends in the 10 sets :
 * finder.get(10).get("A0").has("APOSTROPHE") // true
 * finder.get(10).get("P1").has("APOSTROPHE") // true
 * finder.get(10).get("O2").has("APOSTROPHE") // true
 * ...
 * finder.get(10).get("E9").has("APOSTROPHE") // true
 *
 * Other examples :
 * finder.get(14).get("T13").has("ACCESSOIREMENT") // true
 * finder.get(4).get("N2").has("RING") // true
 */
console.time("Time to build the finder");
const finder = words.reduce((bylength,word)=>{
        const n = word.length;
        if(!bylength.has(n)) {
            bylength.set(n,new Map());
        }
        const m = bylength.get(n);
        word.match(/./gm).forEach((letter,position)=>{
            const pattern = `${letter}${position}`;
            if(!m.has(pattern)) {
                m.set(pattern, new Set());
            }
            const s = m.get(pattern);
            s.add(word);
        });
        return bylength;
    },new Map());
console.timeEnd("Time to build the finder");

/*
 * Read the pattern
 *
 * Each LETTER is a fixed letter
 * Each DOT is a space to be filled
 * Each HASH is a black cell
 * Each SPACE is unused, allowing non-square grids
 */
const grid = fs.readFileSync(0,'utf8')
    .trim()
    .split(/[\r\n]/gm)
    .filter(l=>l)
    .map(l=>l.match(/./gm))
    ;
console.debug("Grid", [[]].concat(grid.map(row=>row.join(""))).join("\n"));



