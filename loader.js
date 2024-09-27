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
const seq = (n)=>[...Array(n).keys()];
function enableEditor() {
    const select = document.querySelector("#grid_layout");
    select.disabled = false;
    select.addEventListener("change",function() {
        switch(select.value) {
            case "5x5":
            case "6x6":
            case "10x10":
            case "15x15":
                const [w,h] = select.value.split(/x/gm).map(s=>+s);
                applyLayout(seq(h).map(_=>
                        seq(w).map(_=>".").join("")
                    ).join("\n"));
                break;
            case "Dog":
                applyLayout([
                    "  ## ##### ##  ",
                    " #..#.....#..# ",
                    "#.............#",
                    "#..#.......#..#",
                    "#.##.#...#.##.#",
                    "## #.#...#.# ##",
                    "   #.......#   ",
                    "   #..###..#   ",
                    "    #..#..#    ",
                    "     #####     ",
                ].join("\n"));
                break;
            case "Cat":
                applyLayout([
                    "##        ##",
                    "#.#      # #",
                    "#..#....#..#",
                    "#...####...#",
                    "#..........#",
                    "#..#....#..#",
                    "#.#.#..#.#.#",
                    "#..........#",
                    " #...##...# ",
                    "  #......#  ",
                    "   ######   ",
                ].join("\n"));
                break;
            case "American":
                applyLayout([
                    "....#....#.....",
                    "....#....#.....",
                    "....#....#.....",
                    "...........#...",
                    "###...#........",
                    "........#....##",
                    ".........##....",
                    "...#...#...#...",
                    "....##.........",
                    "##....#........",
                    "........#...###",
                    "...#...........",
                    ".....#....#....",
                    ".....#....#....",
                    ".....#....#....",
                ].join("\n"));
                break;
        }
    });
}
function applyLayout(layout) {
    const root = document.querySelector(".grid");
    while(root.firstChild){
        root.removeChild(root.firstChild);
    }
    const table = root.appendChild(document.createElement("table"));
    table.className = "grid";
    table.border = "1";
    layout.split(/[\r\n]+/gm).forEach(row=>{
        const tr = table.appendChild(document.createElement("tr"));
        row.match(/./gm).forEach(cell=>{
            const td = tr.appendChild(document.createElement("td"));
            td.textContent = cell;
            if(cell == '.') {
                td.textContent = "";
            } else if(cell == '#') {
                td.className = "black";
            }
        });
    });
    console.log("Applying layout", layout);
}

