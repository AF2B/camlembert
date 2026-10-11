type t =
  { buffer : Buffer.t
  ; mutable cursor : Position.t option
  }

let create buffer = { buffer; cursor = None }
let area frame = Buffer.area frame.buffer
let buffer frame = frame.buffer
let render_widget frame widget ~area = widget area frame.buffer

let render_stateful_widget frame widget ~area state =
  widget area frame.buffer state
;;

let set_cursor frame position = frame.cursor <- Some position
let cursor frame = frame.cursor
