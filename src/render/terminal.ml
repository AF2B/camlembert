type t =
  { backend : Backend.t
  ; mutable current : Buffer.t
  ; mutable previous : Buffer.t
  ; mutable cursor_shown : bool
  }

let create backend =
  let area = backend.Backend.size () in
  { backend
  ; current = Buffer.create area
  ; previous = Buffer.create area
  ; cursor_shown = false
  }
;;

let area terminal = Buffer.area terminal.current

let clear terminal =
  terminal.backend.Backend.clear ();
  Buffer.reset terminal.previous
;;

let resize terminal new_area =
  terminal.backend.Backend.clear ();
  terminal.current <- Buffer.create new_area;
  terminal.previous <- Buffer.create new_area
;;

let autoresize terminal =
  let screen_area = terminal.backend.Backend.size () in
  if not (Rect.equal screen_area (area terminal))
  then resize terminal screen_area
;;

let send_changes terminal =
  let changes =
    Buffer.diff ~previous:terminal.previous ~next:terminal.current
  in
  terminal.backend.Backend.draw changes
;;

(* Drawing moves the cursor, so a visible cursor is put back after every
   frame. A hidden one only needs to be hidden once. *)
let update_cursor terminal frame =
  match Frame.cursor frame with
  | Some _ as position ->
    terminal.backend.Backend.set_cursor position;
    terminal.cursor_shown <- true
  | None ->
    if terminal.cursor_shown
    then (
      terminal.backend.Backend.set_cursor None;
      terminal.cursor_shown <- false)
;;

(* The finished frame becomes the previous one; the next frame starts blank. *)
let swap_buffers terminal =
  Buffer.reset terminal.previous;
  let finished_frame = terminal.current in
  terminal.current <- terminal.previous;
  terminal.previous <- finished_frame
;;

let draw terminal render_frame =
  autoresize terminal;
  let frame = Frame.create terminal.current in
  render_frame frame;
  send_changes terminal;
  update_cursor terminal frame;
  terminal.backend.Backend.flush ();
  swap_buffers terminal
;;
