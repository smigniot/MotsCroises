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

function dictionaryReceived(words) {
    const worker = new Worker("worker.js");
    worker.onmessage = ((e)=>{
        switch(e.data.type) {
            case "PREPARED":
                console.debug("Prepared");
                enableEditor();
                break;
            default:
                console.warn("UNKOWN MESSAGE", e);
                break;
        }
    });
    worker.postMessage({type:"PREPARE", words});
}

/*
 * Enable editor
 *
 * At this point, the editor is under building
 * After building, messages can flow to the worker
 */
function enableEditor() {
    const root = document.querySelector(".grid");
    const wi = document.querySelector("#grid_width");
    const hi = document.querySelector("#grid_height");
    const li = document.querySelector("#grid_layout");
    li.onchange = function() {
        console.log("LAYOUT", li.value);
    }
    wi.onchange = function() {
        whChanged();
    }
    hi.onchange = function() {
        whChanged();
    }
    function whChanged() {
        const w = (+wi.value) || 6;
        const h = (+hi.value) || 6;
        console.log("SIZE", `${w}x${h}`);
    }

}

