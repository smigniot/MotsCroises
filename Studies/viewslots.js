const fs = require("fs");

const template = fs.readFileSync("failcase.txt","utf-8")
    .trim().split(/[\r\n]+/gm)
    .map(l=>l.trim()).filter(l=>l)
    .map(l=>l.match(/./gm))
    ;
console.log("Template", "\n" + template.map(l=>l.join("")).join("\n"));

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
})().map((cells,i)=>{
    return {cells,i};
});
console.log(`Slots(${slots.length})`, slots.map(slot=>{
    const {cells} = slot;
    const {x:x0,y:y0} = cells[0];
    const {x:x1,y:y1} = cells[cells.length-1];
    return `${x0},${y0}->${x1},${y1}`;
}).join(", "));

const slotsAt = slots.reduce((acc,slot)=>{
    const {cells,i} = slot;
    cells.forEach(cell=>{
        const {x,y} = cell;
        const k = `${x},${y}`;
        acc[k] = acc[k] || [];
        acc[k].push(slot);
    });
    return acc;
},{})

result = [];
result.push("graph {")
result.push("\tlayout=fdp")
const edges = new Set();
const entanglements = new Map();
slots.forEach(slot=>{
    const {cells,i} = slot;
    const {x:x0,y:y0} = cells[0];
    const {x:x1,y:y1} = cells[cells.length-1];
    const lbl = `${x0},${y0}->${x1},${y1}`;
    result.push(`\tslot${i} [label="Slot #${i}\\n${lbl}"];`);
    const crossings = [];
    entanglements.set(slot, crossings);
    cells.reduce((set,cell)=>{
        const {x,y} = cell;
        const k = `${x},${y}`;
        return set.concat(slotsAt[`${x},${y}`]);
    },[]).forEach(crossing=>{
        if(crossing != slot) {
            const paired =  [slot.i, crossing.i];
            paired.sort((a,b)=>a-b);
            const edge = `\tslot${paired[0]} -> slot${paired[1]};`;
            edges.add(edge);
            crossings.push(crossing);
        }
    });
});
[...edges].forEach(edge=>{
    result.push(edge);
});
result.push("}")

fs.writeFileSync("graph.txt", result.join("\n"));

// Most entangled
const pairs = [...entanglements.entries()];
pairs.sort((a,b)=>b[1].length-a[1].length);
console.log("Entanglements", pairs.map(pair=>{
    const [slot,crossings] = pair;
    const n = crossings.length;
    const {cells} = slot;
    const {x:x0,y:y0} = cells[0];
    const {x:x1,y:y1} = cells[cells.length-1];
    return `${x0},${y0}->${x1},${y1}:${n}`;
}).join(", "));
const filler = [...pairs].reverse();
console.log(filler);

const copy = template.map(row=>[...row]);
filler.forEach(pair=>{
    const [slot,crossings] = pair;
    const n = crossings.length.toString(16).toUpperCase();;
    const {cells} = slot;
    cells.forEach(xy=>{
        const {x,y} = xy;
        copy[y][x] = n;
    });
});
console.log("Constraints", "\n" + copy.map(l=>l.join("")).join("\n"));


