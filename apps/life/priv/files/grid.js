const width = window.innerWidth;
const height = window.innerHeight - 15;
const cell_size = 20;

const process_window_size = pixel_count => [pixel_count % 40, pixel_count - (pixel_count % 40)];
const [h_border_size, h_window_size] = process_window_size(width);
const [v_border_size, v_window_size] = process_window_size(height);

let enable_drawing = true;

let canvas = document.createElement("CANVAS");
canvas.width = h_window_size;
canvas.height = v_window_size;
canvas.style = "width: " + h_window_size.toString() + "px; height: " + v_window_size.toString() + "px; border-left: " + (h_border_size / 2).toString() + "px solid; border-right: " + (h_border_size / 2).toString() + "px solid;";

let c = canvas.getContext('2d');
c.strokeStyle = "#EEEEEE"
const draw_line = (x1, y1, x2, y2) => {
    c.beginPath();
    c.moveTo(x1, y1);
    c.lineTo(x2, y2);
    c.stroke(); 
}
const draw_vertical_line = x => draw_line(x, 0, x, v_window_size - 1);
const draw_horizontal_line = y => draw_line(0, y, h_window_size - 1, y);
const draw_array_of_lines = (window_size, draw_line_f) => {
    draw_line_f(0);
    for (let i = cell_size; i < window_size; i += cell_size) {
        draw_line_f(i - 1);
        draw_line_f(i);
    }
    draw_line_f(window_size - 1);
}

draw_array_of_lines(h_window_size, draw_vertical_line);
draw_array_of_lines(v_window_size, draw_horizontal_line);
    
const get_cell_corner = (x, y) => [x / cell_size, y / cell_size].map(Math.floor);
    
let black_cells = {};
const switch_cell = (x, y) => {
    const is_black = black_cells[[x, y].toString()];
        
    c.fillStyle = is_black ? "#FFFFFF" : "#000000";
    c.fillRect(x * cell_size + 1, y * cell_size + 1, cell_size - 2, cell_size - 2); 
    if (is_black) {
        delete black_cells[[x, y].toString()];
    }
    else {
        black_cells[[x, y].toString()] = [x, y];
    }
}
    
const switch_by_coords = (raw_x, raw_y, ignore_set = {}) => {
    if (!enable_drawing) return;
    const x = raw_x + document.scrollingElement.scrollLeft - h_border_size / 2;
    const y = raw_y + document.scrollingElement.scrollTop;
    const [corner_x, corner_y] = get_cell_corner(x, y);
    if (!ignore_set[[corner_x, corner_y].toString()] && x >= 0 && x < h_window_size && y < v_window_size) { 
        switch_cell(corner_x, corner_y);
    }
}

const reset_all = () => {
    Object.values(black_cells).forEach(([x, y]) => switch_cell(x, y));
    black_cells = {};
}

canvas.addEventListener("mousedown", ({button: btn, clientX: raw_x, clientY: raw_y}) => {
    if (btn !== 0) return;
    switch_by_coords(raw_x, raw_y);
});
    
let mouse_clicked = false;
document.addEventListener("mousedown", _ => mouse_clicked = true);
document.addEventListener("mouseup", _ => mouse_clicked = false);
    
canvas.addEventListener("mousemove", ({clientX: raw_x, clientY: raw_y}) => {
    if (!mouse_clicked) return;
    switch_by_coords(raw_x, raw_y, black_cells);
});

let ws;
const play_pause = () => {
    if (enable_drawing && Object.keys(black_cells).length > 0) {
	enable_drawing = false;
        ws = new WebSocket("ws://przepraszam.co/websocket");
        ws.onopen = () => ws.send(JSON.stringify({
	    "width": (h_window_size / cell_size), 
	    "height": (v_window_size / cell_size),
	    "cells": Object.values(black_cells)
	}));
        ws.onmessage = ({data: json}) => {
	    if (json === "How' you doin'?") return;
	    reset_all();
	    const {cells: new_cells} = JSON.parse(json);
	    new_cells.forEach(([x, y]) => switch_cell(x, y));
	    setTimeout(() => ws.send(JSON.stringify({"get": "next"})), 100);
	};
    }
    else {
	ws.close();
        enable_drawing = true;
    }
};
    
document.addEventListener("keypress", ({key: key}) => {
    if (key === ' ') play_pause();
});

document.body.appendChild(canvas);

