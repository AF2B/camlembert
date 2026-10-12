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
  ; mutable cursor_request : Position.t option
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
  ; cursor_request = None
  }
;;

(* A shown cursor is where the last frame asked; a hidden one waits on the first
   row of the viewport. *)
let cursor_row terminal =
  match terminal.cursor_request with
  | Some position -> position.Position.row
  | None -> (area terminal).Rect.row
;;

(* Blanking from the first cell of the viewport leaves the cursor there. *)
let clear terminal =
  terminal.backend.Backend.clear_from (top_left (area terminal));
  terminal.cursor_request <- None;
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
           (cursor_row terminal - previous_area.Rect.row))
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
  terminal.cursor_request <- None
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
  | Inline _ -> terminal.backend.Backend.move_cursor (top_left (area terminal))
;;

(* Drawing moves the cursor, so a visible cursor is put back after every
   frame. A hidden one only needs to be hidden once. *)
let place_cursor terminal request =
  terminal.cursor_request <- request;
  match request with
  | Some _ ->
    terminal.backend.Backend.set_cursor request;
    terminal.cursor_shown <- true
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
  place_cursor terminal (Frame.cursor frame);
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

let move_rows (changes : Buffer.change list) ~by =
  List.map
    (fun (change : Buffer.change) -> { change with row = change.row + by })
    changes
;;

(* The cells of rows [first] up to [first + count], moved to start at [at_row]. *)
let rows_at (changes : Buffer.change list) ~first ~count ~at_row =
  List.filter
    (fun (change : Buffer.change) ->
       change.row >= first && change.row < first + count)
    changes
  |> fun selected -> move_rows selected ~by:(at_row - first)
;;

(* Prints the cells in the rows the viewport starts on and scrolls the screen
   just enough to put the viewport below them, as many times as the rows need.
   Rows that scroll off the top are the history. Returns the viewport where it
   ends up, with nothing drawn in it. *)
let rec print_rows backend ~screen_height ~changes ~remaining ~printed viewport =
  let chunk = Int.min remaining (screen_height - viewport.Rect.row) in
  if remaining <= Int.zero || chunk <= Int.zero
  then viewport
  else (
    backend.Backend.clear_from (top_left viewport);
    backend.Backend.draw
      (rows_at changes ~first:printed ~count:chunk ~at_row:viewport.Rect.row);
    let scroll =
      Int.max
        Int.zero
        (viewport.Rect.row + chunk + viewport.Rect.height - screen_height)
    in
    if scroll > Int.zero then backend.Backend.scroll_up scroll;
    print_rows
      backend
      ~screen_height
      ~changes
      ~remaining:(remaining - chunk)
      ~printed:(printed + chunk)
      { viewport with Rect.row = viewport.Rect.row + chunk - scroll })
;;

(* Shows the last frame again where the viewport moved to. *)
let repaint terminal ~from_area ~to_area =
  let shift = to_area.Rect.row - from_area.Rect.row in
  let changes =
    Buffer.diff ~previous:(Buffer.create from_area) ~next:terminal.previous
    |> move_rows ~by:shift
  in
  terminal.backend.Backend.draw changes;
  let repainted = Buffer.create to_area in
  Buffer.apply repainted changes;
  terminal.previous <- repainted;
  terminal.current <- Buffer.create to_area;
  place_cursor
    terminal
    (Option.map
       (fun (position : Position.t) ->
          { position with row = position.row + shift })
       terminal.cursor_request)
;;

let insert_before terminal ~height render_lines =
  if height < Int.zero
  then invalid_arg "Terminal.insert_before: height cannot be negative";
  (match terminal.viewport with
   | Inline _ -> ()
   | Fullscreen ->
     invalid_arg
       "Terminal.insert_before: only an inline viewport has room above it");
  if height > Int.zero
  then (
    autoresize terminal;
    let from_area = area terminal in
    let lines =
      Buffer.create
        (Rect.make
           ~column:from_area.Rect.column
           ~row:Int.zero
           ~width:from_area.Rect.width
           ~height)
    in
    render_lines (Frame.create lines);
    let changes =
      Buffer.diff ~previous:(Buffer.create (Buffer.area lines)) ~next:lines
    in
    let to_area =
      print_rows
        terminal.backend
        ~screen_height:terminal.screen.Rect.height
        ~changes
        ~remaining:height
        ~printed:Int.zero
        from_area
    in
    repaint terminal ~from_area ~to_area;
    terminal.backend.Backend.flush ())
;;
