INPUT_TEXTAREA.addEventListener("input", ({ target }) => {
    if (target.lineCounter === undefined)
        target.lineCounter = 1;

    editLines(target);

    console.log();
    parseInput();
});

SOLVE_BUTTON.addEventListener("click", () => {
    let inputLines = parseLines(INPUT_TEXTAREA.value).filter(elem => elem !== "");
    const resultObj = {
        verticesNumber: null,
        edges: null
    }

    resultObj.verticesNumber = Array(Number(inputLines[0])).fill(0).map((item, i) => i + 1);
    resultObj.edges = inputLines.splice(1).map(elem => elem.split(" ").map(elem => Number(elem)));

    const response = solveGraph(resultObj).map(elem => {
        if (elem === 0) return "red";
        if (elem === 1) return "green";
        if (elem === 2) return "blue";
    });

    colorGraph(response);
});