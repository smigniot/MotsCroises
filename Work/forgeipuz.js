const fs = require('fs');

const solution = fs.readFileSync(0,'utf8')
    .split(/[\r\n]/gm)
    .filter(l=>l)
    .map(l=>l.match(/./gm))
    ;

const h = solution.length;
const w = solution[0].length;

let clueNumber = 0;
const out = [];
const across = [];
const down = [];
for(let y=0; y<h; y++) {
    const row = [];
    out.push(row);
    for(let x=0; x<w; x++) {
        const cell = solution[y][x];
        const isClue = (('A' <= cell) && (cell <= 'Z')) && (
                (y == 0)
             || (x == 0)
             || (solution[y-1][x] == '#')
             || (solution[y][x-1] == '#')
            );
        const isAcross = isClue && (
                (x == 0)
             || (solution[y][x-1] == '#')
            );
        const isDown = isClue && (
                (y == 0)
             || (solution[y-1][x] == '#')
            );
        const ele = isClue?(++clueNumber):(
            (cell == '#')?'#':(
            0
        ));
        if(isDown) {
            down.push([clueNumber,""])
        }
        if(isAcross) {
            across.push([clueNumber,""])
        }
        row.push(ele);
    }
}
console.log(out);
console.log(across);
console.log(down);


