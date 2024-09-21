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

console.log("WORDS", words.slice(0,8).join(" "), "...");
console.log("WIDTHxHEIGHT", width, height);
console.log("GRID", [""].concat(grid.map(r=>r.join(""))).join("\n "));

console.time("GADDAG");
const gaddag = words.reduce((root,word)=>{
    const n = word.length;
    for(let i=1; i<n; i++) {
        const rev = (word.slice(0,i).match(/./gm) || []).reverse().join("");
        const suffix = word.slice(i);
        const revxy = rev+(suffix?"+":"")+suffix+"#";
        revxy.match(/./gm).reduce((current,letter)=>{
            if(! current[letter]) {
                current[letter] = {};
            }
            return current[letter];
        },root);
    }
    return root;
},{});
console.timeEnd("GADDAG");


