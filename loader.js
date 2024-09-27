//vim: set ts=4 sw=4 sts=4 expandtab foldmethod=marker :
(function(){window.addEventListener("load",function(){
    /*
     * Read the dictionary
     *
     * One word per line. Split and trim.
     */
    console.debug("Loading dictionary");
    fetch("dict_fr.txt")
        .then(r=>r.text())
        .then(dict=>{
            const words = dict.trim()
                .split(/[\r\n]/gm)
                .map(l=>l.trim())
                .filter(l=>l)
                ;
            console.debug("Loaded dictionary");
            dictionaryReceived(words);
        });
})})();

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
function dictionaryReceived(words) {
    const worker = new Worker("worker.js");
    worker.onmessage = ((e)=>{
        switch(e.data.type) {
            case "PREPARED":
                console.debug("Prepared");
                // TODO
                break;
            default:
                console.warn("UNKOWN MESSAGE", e);
                break;
        }
    });
    worker.postMessage({type:"PREPARE", words});
}

