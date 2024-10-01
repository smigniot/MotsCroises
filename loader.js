//vim: set ts=4 sw=4 sts=4 expandtab foldmethod=marker :
(function(){window.addEventListener("load",function(){
    const select = document.querySelector("#grid_layout");
    select.disabled = "disabled";

    const params = new URLSearchParams(document.location.search);
    const lang = params.get("lang") || "fr";
    if(lang == 'en') {
        [["save", "Save"],
         ["loadtext", "Load"],
         ["solve","Solve"]].forEach(pair=>{
            const [id,text] = pair;
            document.querySelector("#"+id).textContent = text;
        });
    }

    /*
     * Read the dictionary
     *
     * One word per line. Split and trim.
     */
    console.debug("Loading dictionary");
    document.querySelector("#prog").value = 2;
    fetch(`dict_${lang}.txt`)
        .then(r=>r.text())
        .then(dict=>{
            document.querySelector("#prog").value = 3;
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
                document.querySelector("#prog").value = 4;
                console.debug("Prepared");
                setTimeout(()=>{
                    const e = document.querySelector("#prog");
                    e.parentNode.removeChild(e);
                },250);
                enableEditor(worker);
                break;
            case "PROGRESS":
            case "SOLVED":
                applyLayout(e.data.grid);
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
function enableEditor(worker) {
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
                    "#.#      #.#",
                    "#..#    #..#",
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
            case "Mouse":
                applyLayout([
                    "        ####      ",
                    "       ######     ",
                    "       ######     ",
                    "       ######     ",
                    "     ########     ",
                    "    #.#.#### ###  ",
                    "   #.....######## ",
                    "## ##.#..#########",
                    "###.#.#..#########",
                    " #..#.#..#########",
                    " #.........###### ",
                    "  #.....##.# ###  ",
                    "   #...## #       ",
                    "    ###..#        ",
                    "     ####         ",
                ].join("\n"));
                break;
        }
    });
    document.querySelector("#save").addEventListener("click",()=>{
        const grid = getTextGrid();
        const uri = "data:application/octet-stream,"+encodeURIComponent(grid),
            day = new Date().toISOString().split("T")[0].replace(/-/g,''),
            title = (document.title || "grid")+"_"+day+".txt",
            a = document.createElement("a");
        a.setAttribute("href", uri);
        a.setAttribute("download", title);
        document.body.appendChild(a);
        try { a.click(); } catch(err) {}
        document.body.removeChild(a);
    });
    document.querySelector("#load").querySelector("input")
            .addEventListener("change",(e)=>{
        const file = document.querySelector("#load")
            .querySelector("input")
            .files[0];
        if(file) {
            const reader = new FileReader();
            reader.readAsText(file, "UTF-8");
            reader.onload = (e)=>{
                const content = e.target.result;
                console.log("LOADED", content);
                applyLayout(content);
            }
        }
    });
    document.querySelector("#load").addEventListener("click",()=>{
        document.querySelector("#load").querySelector("input").click();
    });
    enableSolve(worker);
}
function getTextGrid() {
    return grid = [...document.querySelector(".grid table").rows]
        .map(row=>[...row.cells].map(td=>
            td.querySelector("input").value.toUpperCase()
        ).join("")).join("\n");
}
function enableSolve(worker) {
    document.querySelector("#solve").style.display = "inline-block";
    document.querySelector("#solve").addEventListener("click",()=>{
        const grid = getTextGrid();
        worker.postMessage({type:"SOLVE",grid});
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
            const input = td.appendChild(document.createElement("input"));
            input.setAttribute("type","text");
            input.setAttribute("maxlength","1");
            input.value = cell;
            applyStyle(td);
            input.addEventListener("focus",()=>{
                input.select();
            });
            input.addEventListener("change",()=>{
                applyStyle(td);
            });
        });
    });
    console.log("Applying layout", layout);
}

function applyStyle(td) {
    const cell = td.querySelector("input").value;
    
    if(cell == '.') {
        td.className = "blank";
    } else if(cell == '#') {
        td.className = "black";
    } else if(cell == ' ') {
        td.className = "empty";
    } else {
        td.className = '';
    }
}


