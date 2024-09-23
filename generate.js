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
const template = fs.readFileSync(0,'utf8')
    .split(/[\r\n]/gm)
    .filter(l=>l)
    .map(l=>l.match(/./gm))
    ;
console.debug("Grid", [[]].concat(template.map(row=>row.join(""))).join("\n"));

/*
 * Detect word slots in the grid
 *
 * A word slot is a serie of consecutive non-SPACE, non-DASH two or more cells
 */
const slots = (function() {
    const w = template.reduce((n,row)=>Math.max(n,row.length),0);
    const h = template.length;
    const slots = [];
    for(let x=0; x<w; x++) {
        let current = [];
        function endofserie() {
            if(current.length >= 2) {
                slots.push(current);
            }
            current = [];
        }
        for(let y=0; y<h; y++) {
            const cell = template[y][x];
            if(cell.match(/[ #]/)) {
                endofserie();
            } else if(cell.match(/[A-Z.]/)) {
                current.push({cell,x,y});
            } else {
                console.error(`Grid unknown symbol at ${x},${y}`);
            }
        }
        endofserie()
    }
    // Probably can do smarter than duplication, *but* this is
    // both straightforward, readable and maintainable
    for(let y=0; y<h; y++) {
        let current = [];
        function endofserie() {
            if(current.length >= 2) {
                slots.push(current);
            }
            current = [];
        }
        for(let x=0; x<w; x++) {
            const cell = template[y][x];
            if(cell.match(/[ #]/)) {
                endofserie();
            } else if(cell.match(/[A-Z.]/)) {
                current.push({cell,x,y});
            } else {
                console.error(`Grid unknown symbol at ${x},${y}`);
            }
        }
        endofserie()
    }
    return slots;
})();
console.debug("Word slots :", slots.length);

/*
 * Enumerate all solutions.
 *
 * Starting with the inital grid,
 * - Find the word slot with
 *   1. At least one known letter
 *   1.b. FIXME: should prioritize a candidate under recursion
 *               or dead branches will be tested too many times
 *   2. The min cells to fill, but at least one
 *   3. The max known letters, but at least one
 * - For this slot, find all candidates, Recurse, Backtrack
 *   - When no candidate is found : dead-end, backtrack
 *   - When no slot is found
 *     - If all cells are filled, yield a solution
 *     - Else yield a CCL error, 
 *       https://en.wikipedia.org/wiki/Connected-component_labeling
 */
function recurse(grid) {
    const best = slots.reduce((best,slot)=>{
        // slot is [{x,y}...]
        const {patterns,known,tofill} = slot.reduce((acc,xy,i)=>{
            const {x,y} = xy;
            const value = grid[y][x];
            if(value == '.') {
                acc.tofill++;
            } else {
                acc.patterns.push(`${value}${i}`);
                acc.known++;
            }
            return acc;
        },{patterns:[],known:0,tofill:0});
        if((best==null) || (
                    ((tofill > 0) && (tofill < best.tofill)) || (
                        (tofill == best.tofill) &&
                        (known > best.known)
                    )
                )) {
            best = {patterns,known,tofill,slot};
        }
        return best;
    },null);
    if(best == null) {
        console.log("DBG1", best);
        throw "DBG1";
    } else {
        const {patterns, slot} = best;
        console.log("DBG2", patterns, slot);
        throw "DBG2";
        //const candidates =
    }
}
recurse(template);


