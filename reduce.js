const fs = require('fs');

/*
 * Read the dictionary
 *
 * One word per line. Split and trim.
 */
const words = fs.readFileSync("dict_en.txt",'utf8')
    .trim()
    .split(/[\r\n]/gm)
    .map(l=>l.trim())
    .filter(l=>l)
    ;
console.debug("Dictionary length :", words.length);

const pairs = Object.entries(words.reduce((acc,word)=>{
    word.trim().match(/./gm).forEach(letter=>{
        acc[letter] = (acc[letter] || 0)+1;
    });
    return acc;
},{}));
pairs.sort((p1,p2)=>p2[1]-p1[1]);

console.debug("Cardinalities", pairs);

const k=13;
const reduced = Object.values(pairs.reduce((acc,pair,i)=>{
    const [letter,count] = pair;
    const group = i%k;
    acc[group] = acc[group] || [];
    acc[group].push(letter);
    return acc;
},{}));
console.debug("Reduced to", k, "groups", reduced);

const mapping = reduced.reduce((m,group)=>{
    const single = group[0];
    group.forEach(l=>{
        m[l] = single;
    });
    return m;
},{});

["STOP", "ELITE", "FILM", "FACADE"].forEach(word=>{
    const transformed = word.match(/./gm).map(l=>mapping[l]).join("");
    console.log(word, "becomes", transformed);
});
const full = [...words.reduce((set,word)=>{
    const transformed = word.match(/./gm).map(l=>mapping[l]).join("");
    set.add(transformed);
    return set;
},new Set())];
full.sort();
const out = `dict_k${k}.txt`;
fs.writeFileSync(out, full.join("\n"));
console.log(out,"written");

