const socket = io.connect("http://localhost:8080", {transports: ['websocket']});
socket.on('fire', addProjectile);

const keyStates = {
    "w": false,
    "a": false,
    "s": false,
    "d": false
};

function setState(key, toSet){
    if(keyStates[key] !== toSet){
        keyStates[key] = toSet;

        updatePlayerVelocity();
    }
}

function handleEvent(event, toSet){
    if(event.key === "w" || event.key === "ArrowUp"){
        setState("w", toSet);
    }else if(event.key === "a" || event.key === "ArrowLeft"){
        setState("a", toSet);
    }else if(event.key === "s" || event.key === "ArrowDown"){
        setState("s", toSet);
    }else if(event.key === "d" || event.key === "ArrowRight"){
        setState("d", toSet);
    }
}

document.addEventListener("keydown", function (event) {
    handleEvent(event, true);
});

document.addEventListener("keyup", function (event) {
    handleEvent(event, false);
});



