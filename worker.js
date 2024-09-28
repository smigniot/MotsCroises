let FINDER,BYPOSANDLETTER;

onmessage = ((e)=>{
    switch(e.data.type) {
        case "PREPARE":
            const {words} = e.data;
            prepare(words);
            postMessage({type:"PREPARED"});
            break;
        case "SOLVE":
            const {grid} = e.data;
            const template = grid
                .split(/[\r\n]/gm)
                .filter(l=>l)
                .map(l=>l.match(/./gm))
                ;
            solve(template);
            break;
        default:
            console.warn("UNKOWN MESSAGE", e);
            console.warn("UNKOWN MESSAGE DATA", e.data);
            console.warn("UNKOWN MESSAGE AS JSON", JSON.stringify(e.data));
            break;
    }
});

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
function prepare(words) {
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
    FINDER = finder;
    const byposandletter = words.reduce((acc,word)=>{
        word.trim().match(/./gm).forEach((letter,position)=>{
            if(!acc.has(position)) {
                acc.set(position,new Map());
            }
            const ap = acc.get(position);
            if(!ap.has(letter)) {
                ap.set(letter,0);
            }
            ap.set(letter,ap.get(letter)+1);
        });
        return acc;
    },new Map());
    [...byposandletter.keys()].forEach(position=>{
        const m = byposandletter.get(position);
        const total = [...m.values()].reduce((a,b)=>a+b,0);
        [...m.keys()].forEach(letter=>{
            const percentage = m.get(letter)/total;
            m.set(letter, percentage);
        });
    });
    BYPOSANDLETTER = byposandletter;
}

function frequencyScore(slot,slotsat,word) {
    let sum = 0;
    let total = slot.length;
    for(let i=0; i<total; i++) {
        const {x,y} = slot[i];
        const letter = word.charAt(i);
        const other = [...slotsat.get(`${x},${y}`)].find(other=>other!=slot);
        if(other) {
            const positionincross = other.reduce((acc,xy,j)=>{
                if((xy.x == x) && (xy.y == y)) {
                    return j;
                }
                return acc;
            },-1);
            if(positionincross != -1) {
                sum += BYPOSANDLETTER.get(positionincross).get(letter) || 0;
            } else {
                console.warn("ERROR in scoring", slot, word, other, letter, i);
            }
        }
    }
    return sum;
}

function solve(template) {
    /*
     * Detect word slots in the grid
     *
     * A word slot is a serie of consecutive non-SPACE,
     * non-DASH two or more cells
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
    slots.forEach(slot=>{
        const w = template.reduce((n,row)=>Math.max(n,row.length),0);
        const h = template.length;
        const {xs,ys} = slot.reduce((acc,xy)=>{
            const {x,y} = xy;
            acc.xs+=x;
            acc.ys+=y;
            return acc;
        },{xs:0,ys:0});
        const dx = (xs/slot.length)-w/2;
        const dy = (ys/slot.length)-h/2;
        const distance = dx*dx+dy*dy;
        slot.distance = distance;
    });
    console.debug("Word slots :", slots.length);
    const slotsat = slots.reduce((bypos,slot)=>{
        slot.forEach(xy=>{
            const {x,y} = xy;
            const repr = `${x},${y}`;
            if(!bypos.has(repr)) {
                bypos.set(repr,new Set());
            }
            bypos.get(repr).add(slot);
        });
        return bypos;
    },new Map());

    const DBGAT = [0];
    const EVERY = 250;
    const solution = [false];
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
            const sets = FINDER.get(slot.length);
            const candidates = patterns.slice(1).reduce((set,pattern)=>{
                    const other = sets.get(pattern) || new Set();
                    return set.intersection(other);
                },sets.get(patterns[0]) || new Set());
            let better = false;
            if((known > 0) && (tofill > 0)) {
                if(best == null) {
                    better = true;
                } else {
                    if(candidates.size < best.candidates.size) {
                        better = true;
                    } else if(candidates.size == best.candidates.size) {
                        if(tofill < best.tofill) {
                            better = true;
                        } else if(tofill == best.tofill) {
                            if(known > best.known) {
                                better = true;
                            }
                        }
                    }
                }
            }
            if(better) {
                best = {patterns,known,tofill,slot,candidates}
            }
            return best;
        },null);
        if(best == null) {
            console.log(grid.map(row=>row.join("")).join("\n"));
            solution[0] = grid;
            return;
        } else if(best.tofill == 0) {
            console.log(grid.map(row=>row.join("")).join("\n"));
            throw "DBG2";
        } else if(best.patterns.length == 0) {
            console.log(grid.map(row=>row.join("")).join("\n"));
            console.log("DBG3", best);
            throw "DBG3";
        } else {
            const {patterns, slot,candidates} = best;
            const frequentToRare = [...candidates]; 
            frequentToRare.sort((a,b)=>
                frequencyScore(slot,slotsat,b)-frequencyScore(slot,slotsat,a));
            frequentToRare.forEach(candidate=>{
                // Here be memory
                const before = [];
                let impactedSlots = new Set();
                slot.forEach((xy,i)=>{
                    const {x,y} = xy;
                    before.push(grid[y][x]);
                    // Modify
                    grid[y][x] = candidate.charAt(i);
                    impactedSlots = impactedSlots.union(
                        slotsat.get(`${x},${y}`));
                });
                if((Date.now()-DBGAT[0]) > EVERY) {
                    postMessage({type:"PROGRESS",grid:
                        grid.map(row=>row.join("")).join("\n")});
                    DBGAT[0] = Date.now();
                }
                // Check that all impacted slots still
                //        actually can (or do) contain a word
                let isBlocker = false;
                [...impactedSlots].forEach(slot=>{
                    const patterns = slot.reduce((acc,xy,i)=>{
                        const {x,y} = xy;
                        const letter = grid[y][x];
                        if(letter!='.') {
                            acc.push(`${letter}${i}`);
                        }
                        return acc;
                    },[]);
                    const sets = FINDER.get(slot.length);
                    const remaining = patterns.slice(1).reduce((set,pattern)=>{
                            const other = sets.get(pattern) || new Set();
                            return set.intersection(other);
                        },sets.get(patterns[0]) || new Set());
                    if(remaining.size == 0) {
                        isBlocker = true;
                    }
                });
                if(!isBlocker) {
                    recurse(grid);
                    if(solution[0]) {
                        return;
                    }
                }
                slot.forEach((xy,i)=>{
                    const {x,y} = xy;
                    // Restore
                    grid[y][x] = before[i];
                });
            });
        }
    }
    recurse(template);
    const finalGrid = template.map(row=>row.join("")).join("\n");
    postMessage({type:"SOLVED", grid:finalGrid});
}

