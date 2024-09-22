const fs = require('fs');
const dawg = require('dawg-lookup');

const words = fs.readFileSync("dictionary.txt",'utf8')
    .trim().split(/[\r\n]/gm)
    .map(l=>l.trim()).filter(l=>l)
    ;

const trie = new dawg.Trie(words.join(" "));
const packed = trie.pack();

fs.writeFileSync("dawg.fr.txt", JSON.stringify(packed));


