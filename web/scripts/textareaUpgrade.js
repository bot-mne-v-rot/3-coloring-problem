function createLineNumber(value) {
    const div = document.createElement("div");
    div.classList.add("line-number");
    div.innerHTML = value;

    return div;
}

function editLines(elem) {
    const content = elem.value;

    let line_counter = 1;
    for (let i = content.indexOf("\n"); i !== -1; i = content.indexOf("\n", i + 1))
        ++line_counter;

    if (elem.lineCounter < line_counter) {
        const linesWraper = elem.previousElementSibling;
        for (let i = elem.lineCounter + 1; i <= line_counter; i++)
            linesWraper.appendChild(createLineNumber(i));
        elem.lineCounter = line_counter;
    } else if (elem.lineCounter > line_counter) {
        const linesWraper = elem.previousElementSibling;
        for (let i = line_counter + 1; i <= elem.lineCounter; i++)
            linesWraper.querySelector(".line-number:last-of-type").remove();
        elem.lineCounter = line_counter;
    }
}