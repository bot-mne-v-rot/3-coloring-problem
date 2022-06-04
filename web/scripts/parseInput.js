function graphObjComp(obj1, obj2) {
    if (obj1.verticesNumber === obj2.verticesNumber) {
        if (obj1.edges.length === obj2.edges.length) {
            const arr1 = obj1.edges;
            const arr2 = obj2.edges;
            for (let i = 0; i < arr1.length; i++) {
                if (!(arr1[i][0] == arr2[i][0] && arr1[i][1] == arr2[i][1]))
                    return false;
            }
        } else {
            return false;
        }
    } else {
        return false;
    }

    return true;
}

function parseLines(str) {
    return str.split("\n").map(elem => {
        if (elem.indexOf("#") !== -1)
            elem = elem.slice(0, elem.indexOf("#"));
        elem = elem.trim();

        return elem.split()
                   .filter(elem => elem !== "")
                   .join(" ");
    });
}

function checkErrors(inputLines) {
    let exitFlag = 0;

    if (inputLines === "") exitFlag = 1;

    let verticesNumber = null;
    let verticesFlag = 0;
    for (let i = 0; i < inputLines.length; i++) {
        let line = inputLines[i];
        if (line === "") continue;

        if (verticesFlag === 0) {
            const num = line.split(" ");
            if (num.length > 1) {
                printInReply(`Line ${i + 1}: Too many arguments specifying vertices quantity are given.`);
                exitFlag = 1;
            } else if (isNaN(Number(num)) || Number(num) < 1) {
                printInReply(`Line ${i + 1}: Vertices quantity should be a natural number.`);
                exitFlag = 1;
            } else {
                verticesNumber = Number(num);
            }

            if (exitFlag === 1) return [exitFlag, verticesNumber];;

            verticesFlag = 1;
            continue;
        }

        let edge = line.split(" ").map(elem => Number(elem));
        if (edge.length == 1) {
            printInReply(`Line ${i + 1}: Too few arguments specifying edge are given.`);
            exitFlag = 1;
        } else if (edge.length > 2) {
            printInReply(`Line ${i + 1}: Too many arguments specifying edge are given.`);
            exitFlag = 1;
        } else if (isNaN(edge[0]) || isNaN(edge[1]) || !elementOf(0, verticesNumber + 1, edge[0]) || !elementOf(0, verticesNumber + 1, edge[1])) {
            printInReply(`Line ${i + 1}: Arguments should be a natural numbers not more than ${verticesNumber}.`);
            exitFlag = 1;
        }
    }

    return [exitFlag, verticesNumber];
}

function parseInput() {
    if (parseInput.prevObj === undefined) {
        parseInput.prevObj = {
            verticesNumber: null,
            edges: []
        }
    }

    REPLY_TEXTAREA.value = "";
    for (const node of GRAPH.nodes())
        document.getElementById(node).style.fill = "";

    let inputLines = parseLines(INPUT_TEXTAREA.value);
    const resultObj = {
        verticesNumber: null,
        edges: []
    }

    let [exitFlag, verticesNumber] = checkErrors(inputLines);

    if (!exitFlag) {
        resultObj.verticesNumber = verticesNumber;
        resultObj.edges = inputLines.filter(elem => elem !== "")
                                    .splice(1)
                                    .map(elem => elem.split(" ").map(elem => Number(elem)));
        
        if (!graphObjComp(parseInput.prevObj, resultObj)) {
            if (resultObj.verticesNumber !== null) {
                SOLVE_BUTTON.disabled = false;
            } else {
                SOLVE_BUTTON.disabled = true;
            }
            parseInput.prevObj = resultObj;
            renderGraph(resultObj);
        }
    } else {
        SOLVE_BUTTON.disabled = true;
    }
}