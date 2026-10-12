type viewport =
  | Fullscreen
  | Inline of int

type t =
  { backend : Backend.t
  ; viewport : viewport
  ; mutable screen : Rect.t
  ; mutable current : Buffer.t
  ; mutable previous : Buffer.t
  ; mutable cursor_shown : bool
  ; mutable cursor_row : int
  }

let minimum_inline_height = Int.one
let area terminal = Buffer.area terminal.current

let top_left region =
  { Position.column = region.Rect.column; row = region.Rect.row }
;;

(* Finds the first row of [height] rows that start at [wanted_top], scrolling
   the screen just enough for them not to go past its bottom. *)
let make_room backend ~screen ~height ~wanted_top =
  let missing = Int.max Int.zero (wanted_top + height - screen.Rect.height) in
  if missing > Int.zero then backend.Backend.scroll_up missing;
  wanted_top - missing
;;

(* The rows of an inline viewport that would start at [wanted_top]. *)
let inline_area backend ~screen ~height ~wanted_top =
  let height = Int.min height screen.Rect.height in
  let top =
    make_room backend ~screen ~height ~wanted_top:(Int.max Int.zero wanted_top)
  in
  Rect.make ~column:screen.Rect.column ~row:top ~width:screen.Rect.width ~height
;;

(* A cursor in the middle of a line leaves that line alone. *)
let first_free_row { Position.column; row } =
  if column > Int.zero then succ row else row
;;

let create ?(viewport = Fullscreen) backend =
  let screen = backend.Backend.size () in
  let area =
    match viewport with
    | Fullscreen -> screen
    | Inline height ->
      if height < minimum_inline_height
      then
        invalid_arg "Terminal.create: an inline viewport needs at least one row";
      let wanted_top = first_free_row (backend.Backend.cursor_position ()) in
      let area = inline_area backend ~screen ~height ~wanted_top in
      backend.Backend.clear_from (top_left area);
      area
  in
  { backend
  ; viewport
  ; screen
  ; current = Buffer.create area
  ; previous = Buffer.create area
  ; cursor_shown = false
  ; cursor_row = area.Rect.row
  }
;;

let clear terminal =
  let area = area terminal in
  terminal.backend.Backend.clear_from (top_left area);
  terminal.cursor_row <- area.Rect.row;
  Buffer.reset terminal.previous
;;

(* The viewport of a resized screen. An inline one is placed from the cursor,
   which the terminal moved along with the text, keeping the cursor on the row
   of the viewport where it was left. *)
let resized_area terminal screen =
  match terminal.viewport with
  | Fullscreen -> screen
  | Inline height ->
    let previous_area = area terminal in
    let offset =
      Int.max
        Int.zero
        (Int.min
           (pred previous_area.Rect.height)
           (terminal.cursor_row - previous_area.Rect.row))
    in
    let cursor = terminal.backend.Backend.cursor_position () in
    inline_area
      terminal.backend
      ~screen
      ~height
      ~wanted_top:(cursor.Position.row - offset)
;;

let resize terminal screen =
  let area = resized_area terminal screen in
  terminal.backend.Backend.clear_from (top_left area);
  terminal.screen <- screen;
  terminal.current <- Buffer.create area;
  terminal.previous <- Buffer.create area;
  terminal.cursor_row <- area.Rect.row
;;

let autoresize terminal =
  let screen = terminal.backend.Backend.size () in
  if not (Rect.equal screen terminal.screen) then resize terminal screen
;;

let send_changes terminal =
  let changes =
    Buffer.diff ~previous:terminal.previous ~next:terminal.current
  in
  terminal.backend.Backend.draw changes
;;

(* Drawing moves the cursor. An inline viewport needs to know where it was
   left to find itself again after a resize, so a hidden cursor waits on the
   first row of the viewport. *)
let park_cursor terminal =
  match terminal.viewport with
  | Fullscreen -> ()
  | Inline _ ->
    let area = area terminal in
    terminal.backend.Backend.move_cursor (top_left area);
    terminal.cursor_row <- area.Rect.row
;;

(* Drawing moves the cursor, so a visible cursor is put back after every
   frame. A hidden one only needs to be hidden once. *)
let update_cursor terminal frame =
  match Frame.cursor frame with
  | Some position as shown ->
    terminal.backend.Backend.set_cursor shown;
    terminal.cursor_shown <- true;
    terminal.cursor_row <- position.Position.row
  | None ->
    if terminal.cursor_shown
    then (
      terminal.backend.Backend.set_cursor None;
      terminal.cursor_shown <- false);
    park_cursor terminal
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

let finish terminal =
  match terminal.viewport with
  | Fullscreen -> ()
  | Inline _ ->
    let backend = terminal.backend in
    let screen_height = (backend.Backend.size ()).Rect.height in
    let below = Rect.bottom (area terminal) in
    if below >= screen_height then backend.Backend.scroll_up Int.one;
    backend.Backend.move_cursor
      { Position.column = terminal.screen.Rect.column
      ; row = Int.min below (pred screen_height)
      };
    backend.Backend.flush ()
;;
