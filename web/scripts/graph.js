import("/scripts/solver.bc.js");

function renderGraph({ verticesNumber, edges }) {
    if (GRAPH.savedVert === undefined)
    GRAPH.savedVert = null;

    for (node of GRAPH.nodes())
        GRAPH.removeNode(node);
    
    for (let i = 1; i <= verticesNumber; i++) {
        GRAPH.addNode(i, {
            id: i,
            r: 16,
            fill: "#10232F",
            stroke: "#FFFFFF",
            strokeWidth: "2px"
        });
    }
    for (const edge of edges) {
        GRAPH.addEdge(edge[0], edge[1], {
            fill: "#FFFFFF"
        });
    }

    jsnx.draw(GRAPH, {
        element: ".graph-wraper",
        withLabels: true,
        nodeAttr: {
            id: d => d.data.id,
            r: d => d.data.r,
            fill: d => d.data.fill,
            stroke: d => d.data.stroke,
            "stroke-width": d => d.data.strokeWidth
        },
        edgeStyle: {
            fill: d => d.data.fill
        }
    });

    for (const nodeID of GRAPH.nodes()) {
        const node = document.getElementById(nodeID);
        node.style = "";

        node.addEventListener("dblclick", ({ target }) => {
            if (GRAPH.savedVert === null) {
                GRAPH.savedVert = target;
                target.style.stroke = "#959595";
            } else if (GRAPH.savedVert === target.id) {
                GRAPH.savedVert = null;
                target.style.stroke = "#FFFFFF";
            } else {
                GRAPH.savedVert.style.stroke = "#FFFFFF";

                for (const edge of GRAPH.edges()) {
                    if (cmpEdges([GRAPH.savedVert.id, target.id], edge)) {
                        GRAPH.savedVert = null;
                        return;
                    }
                }
                
                let event = new Event("input");
                INPUT_TEXTAREA.value += `\n${GRAPH.savedVert.id} ${target.id}`;
                INPUT_TEXTAREA.dispatchEvent(event);

                GRAPH.savedVert = null;
            }
        });
    }
}

function colorGraph(colors) {
    if (!colors.length) {
        REPLY_TEXTAREA.value = "UNSAT";
        return;
    }

    REPLY_TEXTAREA.value = "SAT\n";
    for (let i = 1; i <= colors.length; i++) {
        document.getElementById(i).style.fill = colors[i - 1];
        REPLY_TEXTAREA.value += colors[i - 1].slice(0, 1).toUpperCase();
        if (i !== colors.length) REPLY_TEXTAREA.value += " ";
    }
}

function solveGraph(data) {
    console.log(data);
    console.log(solver.solve(data));


    return [0, 1, 2];
}