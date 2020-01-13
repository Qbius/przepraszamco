const protocol = location.protocol === 'http:' ? 'ws:' : 'wss:';
var ws = new WebSocket(protocol + '//' + location.host + '/websocket');
ws.onopen = () => ws.send("A winc to tok");
ws.onmessage = (event) => console.log(event.data);

var timeoutRef;

function fenezuela() {
	$('#questionBox').val("");
	$('#answerBox').text(Math.random() < 0.5 ? "No." : "Yes.");
	if (timeoutRef) {
		clearTimeout(timeoutRef);
	}
	timeoutRef = setTimeout(() => $('#answerBox').text(""), 1000);
}
