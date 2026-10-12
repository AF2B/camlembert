type t =
  { mutable screen : Buffer.t
  ; mutable history : string list
  ; mutable drawn_cells : int
  ; mutable position : Position.t
  ; mutable cursor_visible : bool
  }

let origin = 0
let start_of_row = 0
let first_row = 0

let blank_screen ~width ~height =
  Buffer.create (Rect.make ~column:origin ~row:origin ~width ~height)
;;

let create ~width ~height =
  { screen = blank_screen ~width ~height
  ; history = []
  ; drawn_cells = Int.zero
  ; position = { Position.column = origin; row = origin }
  ; cursor_visible = false
  }
;;

let width_of test_backend = (Buffer.area test_backend.screen).Rect.width
let height_of test_backend = (Buffer.area test_backend.screen).Rect.height

(* The cursor never leaves the screen: after the last column it waits there. *)
let clamped test_backend { Position.column; row } =
  { Position.column =
      Int.max start_of_row (Int.min (pred (width_of test_backend)) column)
  ; row = Int.max first_row (Int.min (pred (height_of test_backend)) row)
  }
;;

let move test_backend position =
  test_backend.position <- clamped test_backend position
;;

(* Drawing leaves the cursor after the last cell written. *)
let draw test_backend changes =
  Buffer.apply test_backend.screen changes;
  test_backend.drawn_cells <- test_backend.drawn_cells + List.length changes;
  match List.rev changes with
  | [] -> ()
  | { Buffer.column; row; cell } :: _ ->
    move test_backend { Position.column = column + Cell.width cell; row }
;;

let clear_from test_backend ({ Position.column; row } as position) =
  let width = width_of test_backend in
  let height = height_of test_backend in
  Buffer.clear
    test_backend.screen
    (Rect.make
       ~column
       ~row
       ~width:(Int.max Int.zero (width - column))
       ~height:1);
  Buffer.clear
    test_backend.screen
    (Rect.make
       ~column:origin
       ~row:(succ row)
       ~width
       ~height:(Int.max Int.zero (height - succ row)));
  move test_backend position
;;

(* Everything moves up and the rows that leave are kept. The cursor ends on the
   last row, as it does when line feeds are what scrolls a real terminal. *)
let scroll_up test_backend rows =
  let rows = Int.min rows (height_of test_backend) in
  if rows > Int.zero
  then (
    let leaving =
      List.filteri
        (fun index _ -> index < rows)
        (Buffer.to_lines test_backend.screen)
    in
    test_backend.history <- test_backend.history @ leaving;
    let blank = Buffer.create (Buffer.area test_backend.screen) in
    let moved_up =
      Buffer.diff ~previous:blank ~next:test_backend.screen
      |> List.filter_map (fun (change : Buffer.change) ->
        if change.row >= rows
        then Some { change with Buffer.row = change.row - rows }
        else None)
    in
    Buffer.apply blank moved_up;
    test_backend.screen <- blank;
    move
      test_backend
      { test_backend.position with
        Position.row = pred (height_of test_backend)
      })
;;

let set_cursor test_backend = function
  | Some position ->
    move test_backend position;
    test_backend.cursor_visible <- true
  | None -> test_backend.cursor_visible <- false
;;

let backend test_backend =
  { Backend.size = (fun () -> Buffer.area test_backend.screen)
  ; cursor_position = (fun () -> test_backend.position)
  ; draw = draw test_backend
  ; clear_from = clear_from test_backend
  ; scroll_up = scroll_up test_backend
  ; move_cursor = move test_backend
  ; set_cursor = set_cursor test_backend
  ; flush = ignore
  }
;;

let screen test_backend = test_backend.screen
let to_lines test_backend = Buffer.to_lines test_backend.screen
let history test_backend = test_backend.history
let drawn_cells test_backend = test_backend.drawn_cells
let cursor_position test_backend = test_backend.position

let cursor test_backend =
  if test_backend.cursor_visible then Some test_backend.position else None
;;

let print test_backend text =
  let { Position.column; row } = test_backend.position in
  Buffer.set_string
    test_backend.screen
    ~column
    ~row
    ~max_width:(width_of test_backend - column)
    ~style:Style.default
    text;
  move test_backend { Position.column = column + Grapheme.width text; row }
;;

let line_feed test_backend =
  let { Position.column; row } = test_backend.position in
  if row = pred (height_of test_backend)
  then scroll_up test_backend Int.one
  else move test_backend { Position.column; row = succ row }
;;

let carriage_return test_backend =
  move
    test_backend
    { test_backend.position with Position.column = start_of_row }
;;

let print_line test_backend text =
  print test_backend text;
  line_feed test_backend;
  carriage_return test_backend
;;

let resize test_backend ~width ~height =
  test_backend.screen <- blank_screen ~width ~height;
  move test_backend test_backend.position
;;
