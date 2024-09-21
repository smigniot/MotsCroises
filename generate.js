const fs = require('fs');

const words = fs.readFileSync("dictionary.txt",'utf8')
    .trim().split(/[\r\n]/gm)
    .map(l=>l.trim()).filter(l=>l)
    ;
const payload = fs.readFileSync(process.argv[2],'utf8')
    ;
const {w:width,h:height,m:grid} = payload
    .trim().split(/[\r\n]/gm)
    .filter(l=>l.trim())
    .reduce((acc,line)=>{
        acc.h++;
        acc.w = Math.max(acc.w,line.length);
        acc.m.push(line.match(/./gm));
        return acc;
    },{h:0,w:0,m:[]});

console.log("W", words);
console.log("WxH", width, height);
console.log("GRID", grid);


