type t =
  { backend : Backend.t
  ; mutable current : Buffer.t
  ; mutable previous : Buffer.t
  }

let create backend =
  let area = backend.Backend.size () in
  { backend; current = Buffer.create area; previous = Buffer.create area }
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
  terminal.backend.Backend.draw changes;
  terminal.backend.Backend.flush ()
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
  render_frame (Frame.create terminal.current);
  send_changes terminal;
  swap_buffers terminal
;;
