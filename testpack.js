const fs = require('fs');
const dawg = require('dawg-lookup');

const words = fs.readFileSync("dictionary.txt",'utf8')
    .trim().split(/[\r\n]/gm)
    .map(l=>l.trim()).filter(l=>l)
    ;
console.log("Dict words :", words.length);

const trie = new dawg.PTrie(fs.readFileSync("dawg.fr.txt",'utf8'));
console.log("Dawg words :", trie.completions("").length);

