const fs = require('fs');

const words = fs.readFileSync("dictionary.txt",'utf8')
    .trim().split(/[\r\n]/gm)
    .map(l=>l.trim()).filter(l=>l)
    ;
console.log("Dict words :", words.length);

console.time("FINDER");
const finder = words.reduce((bylength,word)=>{
        const n = word.length;
        if(!bylength.has(n)) { bylength.set(n,new Map()) }
        const m = bylength.get(n);
        // letter L at position P of length N
        word.match(/./gm).forEach((letter,position)=>{
            const pattern = `${letter}${position}`;
            if(!m.has(pattern)) { m.set(pattern, new Set()) }
            const s = m.get(pattern);
            s.add(word);
        });
        return bylength;
    },new Map());
console.timeEnd("FINDER");


// .RO.S..
// CROISES
let l = 7;
let patterns = ["R1","O2","S4"];

console.time("SETS");
const sets = finder.get(l);
const candidates = patterns.slice(1).reduce((set,pattern)=>{
        return set.intersection(sets.get(pattern));
    },sets.get(patterns[0]));
console.timeEnd("SETS");
console.log("SETS.n", candidates.size);

console.time("FULLSCAN");
const same = words.filter(w=>w.match(/^.RO.S..$/));
console.timeEnd("FULLSCAN");
console.log("FULLSCAN.n", same.length);





