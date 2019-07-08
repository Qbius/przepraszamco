var ws = new WebSocket("ws://przepraszam.co/websocket");
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
