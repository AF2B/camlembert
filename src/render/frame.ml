type t = { buffer : Buffer.t }

let create buffer = { buffer }
let area frame = Buffer.area frame.buffer
let buffer frame = frame.buffer
let render_widget frame widget ~area = widget area frame.buffer
