let FINDER,BYPOSANDLETTER;

onmessage = ((e)=>{
    switch(e.data.type) {
        case "PREPARE":
            const {words} = e.data;
            prepare(words);
            postMessage({type:"PREPARED"});
            break;
        default:
            console.warn("UNKOWN MESSAGE", e);
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

