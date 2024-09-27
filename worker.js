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

