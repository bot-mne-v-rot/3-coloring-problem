const INPUT_TEXTAREA = document.querySelector(".input-data-textarea");
const REPLY_TEXTAREA = document.querySelector(".response-textarea");
const SOLVE_BUTTON = document.querySelector(".solve-button");

const GRAPH = new jsnx.Graph();

function printInReply(msg) {
    REPLY_TEXTAREA.value += msg + "\n";
}

function elementOf(start, end, value) {
    return (start < value && value < end) ? true : false;
}

function cmpEdges(arr1, arr2) {
    if (arr1[0] == arr2[0] && arr1[1] == arr2[1] || arr1[0] == arr2[1] && arr1[1] == arr2[0]) {
        return true;
    }

    return false;
}