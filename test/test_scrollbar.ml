module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Rect = Camlembert.Rect
module Scrollbar = Camlembert.Scrollbar
module Style = Camlembert.Style

let origin = 0
let track_length = 6
let strip_width = 3
let content_length = 12
let viewport_length = 6
let halfway = 3
let thumb_cells = track_length * viewport_length / content_length
let blank = " "

(* Halfway through, the thumb would start at row 1.5, which rounds up. *)
let thumb_start_halfway = 2
let furthest = 6
let far_beyond = 100
let huge_content = 1000
let one_row = 1
let track = "\u{2502}"
let thumb = "\u{2588}"
let negative_value = -1
let max_track_length = 20
let max_content_length = 60
let max_position = 80
let red_thumb = Style.make ~foreground:Color.Red ()
let blue_track = Style.make ~foreground:Color.Blue ()
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render bar ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Scrollbar.widget bar region buffer;
  buffer
;;

let vertical ?(side = Block.Right) ~content ~viewport ~position () =
  Scrollbar.make
    ~side
    ~content_length:content
    ~viewport_length:viewport
    ~position
    ()
;;

(* The symbols of one column, from top to bottom. *)
let column_symbols buffer ~column ~height =
  List.init height (fun row -> (Buffer.get buffer ~column ~row).Cell.symbol)
;;

let row_symbols buffer ~row ~width =
  List.init width (fun column -> (Buffer.get buffer ~column ~row).Cell.symbol)
;;

let repeat symbol count = List.init count (Fun.const symbol)

let test_the_thumb_follows_the_position () =
  let symbols_at position =
    column_symbols
      (render
         (vertical
            ~content:content_length
            ~viewport:viewport_length
            ~position
            ())
         ~width:strip_width
         ~height:track_length)
      ~column:(pred strip_width)
      ~height:track_length
  in
  Alcotest.(check (list string))
    "top"
    (repeat thumb thumb_cells @ repeat track (track_length - thumb_cells))
    (symbols_at Int.zero);
  Alcotest.(check (list string))
    "middle"
    (repeat track thumb_start_halfway
     @ repeat thumb thumb_cells
     @ repeat track (track_length - thumb_start_halfway - thumb_cells))
    (symbols_at halfway);
  Alcotest.(check (list string))
    "bottom"
    (repeat track (track_length - thumb_cells) @ repeat thumb thumb_cells)
    (symbols_at furthest);
  Alcotest.(check (list string))
    "beyond the end counts as the end"
    (symbols_at furthest)
    (symbols_at far_beyond)
;;

let test_nothing_to_scroll_fills_the_track_with_the_thumb () =
  let buffer =
    render
      (vertical
         ~content:viewport_length
         ~viewport:viewport_length
         ~position:Int.zero
         ())
      ~width:strip_width
      ~height:track_length
  in
  Alcotest.(check (list string))
    "all thumb"
    (repeat thumb track_length)
    (column_symbols buffer ~column:(pred strip_width) ~height:track_length)
;;

let test_the_thumb_is_never_shorter_than_one_cell () =
  let buffer =
    render
      (vertical ~content:huge_content ~viewport:Int.one ~position:Int.zero ())
      ~width:strip_width
      ~height:track_length
  in
  Alcotest.(check (list string))
    "one cell"
    (thumb :: repeat track (pred track_length))
    (column_symbols buffer ~column:(pred strip_width) ~height:track_length)
;;

let test_each_side_has_its_own_edge () =
  let content = content_length
  and viewport = viewport_length in
  let at side =
    render
      (vertical ~side ~content ~viewport ~position:Int.zero ())
      ~width:track_length
      ~height:track_length
  in
  let first = origin
  and last = pred track_length in
  Alcotest.(check (list string))
    "left"
    (repeat thumb thumb_cells @ repeat track (track_length - thumb_cells))
    (column_symbols (at Block.Left) ~column:first ~height:track_length);
  Alcotest.(check (list string))
    "nothing in the middle of a vertical bar"
    (repeat blank track_length)
    (column_symbols (at Block.Left) ~column:halfway ~height:track_length);
  Alcotest.(check (list string))
    "top"
    (repeat thumb thumb_cells @ repeat track (track_length - thumb_cells))
    (row_symbols (at Block.Top) ~row:first ~width:track_length);
  Alcotest.(check (list string))
    "bottom"
    (repeat thumb thumb_cells @ repeat track (track_length - thumb_cells))
    (row_symbols (at Block.Bottom) ~row:last ~width:track_length)
;;

let test_symbols_and_styles_can_be_changed () =
  let bar =
    Scrollbar.make
      ~track_symbol:"."
      ~thumb_symbol:"#"
      ~track_style:blue_track
      ~thumb_style:red_thumb
      ~side:Block.Right
      ~content_length
      ~viewport_length
      ~position:Int.zero
      ()
  in
  let buffer = render bar ~width:one_row ~height:track_length in
  let thumb_cell = Buffer.get buffer ~column:origin ~row:origin in
  let track_cell = Buffer.get buffer ~column:origin ~row:(pred track_length) in
  Alcotest.(check (pair string bool))
    "thumb"
    ("#", true)
    (thumb_cell.Cell.symbol, Style.equal red_thumb thumb_cell.Cell.style);
  Alcotest.(check (pair string bool))
    "track"
    (".", true)
    (track_cell.Cell.symbol, Style.equal blue_track track_cell.Cell.style)
;;

let test_negative_lengths_and_positions_are_rejected () =
  let rejects ~content ~viewport ~position =
    Alcotest.check_raises
      "negative"
      (Invalid_argument "Scrollbar.make: negative length or position")
      (fun () -> ignore (vertical ~content ~viewport ~position ()))
  in
  rejects ~content:negative_value ~viewport:viewport_length ~position:Int.zero;
  rejects ~content:content_length ~viewport:negative_value ~position:Int.zero;
  rejects
    ~content:content_length
    ~viewport:viewport_length
    ~position:negative_value
;;

let strip_symbols ~content ~viewport ~position ~height =
  column_symbols
    (render (vertical ~content ~viewport ~position ()) ~width:Int.one ~height)
    ~column:origin
    ~height
;;

let scrollbar_case =
  QCheck2.Gen.(
    quad
      (int_range Int.zero max_content_length)
      (int_range Int.zero max_content_length)
      (int_range Int.zero max_position)
      (int_range Int.one max_track_length))
;;

let print_scrollbar_case = QCheck2.Print.(quad int int int int)

(* The first and the last position of the thumb, if any. *)
let thumb_span symbols =
  let positions =
    List.filteri (fun _ symbol -> symbol = thumb) symbols |> List.length
  in
  let first = List.find_index (( = ) thumb) symbols in
  first, positions
;;

let property_the_thumb_is_one_run_that_fits_the_track =
  QCheck2.Test.make
    ~name:"the thumb is one run of at least one cell inside the track"
    ~print:print_scrollbar_case
    scrollbar_case
    (fun (content, viewport, position, height) ->
       let symbols = strip_symbols ~content ~viewport ~position ~height in
       match thumb_span symbols with
       | None, _ -> false
       | Some first, length ->
         length >= Int.one
         && length <= height
         && List.for_all
              (fun index -> List.nth symbols index = thumb)
              (List.init length (fun offset -> first + offset)))
;;

let property_a_further_position_never_moves_the_thumb_back =
  QCheck2.Test.make
    ~name:"scrolling further never moves the thumb back"
    ~print:print_scrollbar_case
    scrollbar_case
    (fun (content, viewport, position, height) ->
       let first_cell_at position =
         fst (thumb_span (strip_symbols ~content ~viewport ~position ~height))
       in
       match first_cell_at position, first_cell_at (succ position) with
       | Some earlier, Some later -> earlier <= later
       | _ -> false)
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a scrollbar only changes cells inside its area"
    ~print:QCheck2.Print.(pair Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(pair Generators.rect Generators.edits)
    (fun (region, edits) ->
       Support.leaves_the_outside_untouched
         (Scrollbar.widget
            (vertical
               ~content:content_length
               ~viewport:viewport_length
               ~position:halfway
               ()))
         ~region
         ~edits)
;;

let () =
  Alcotest.run
    "Scrollbar"
    [ ( "thumb"
      , [ Alcotest.test_case
            "position"
            `Quick
            test_the_thumb_follows_the_position
        ; Alcotest.test_case
            "nothing to scroll"
            `Quick
            test_nothing_to_scroll_fills_the_track_with_the_thumb
        ; Alcotest.test_case
            "shortest"
            `Quick
            test_the_thumb_is_never_shorter_than_one_cell
        ] )
    ; ( "bar"
      , [ Alcotest.test_case "sides" `Quick test_each_side_has_its_own_edge
        ; Alcotest.test_case
            "symbols and styles"
            `Quick
            test_symbols_and_styles_can_be_changed
        ; Alcotest.test_case
            "negative values"
            `Quick
            test_negative_lengths_and_positions_are_rejected
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_the_thumb_is_one_run_that_fits_the_track
          ; property_a_further_position_never_moves_the_thumb_back
          ; property_drawing_stays_inside_the_area
          ] )
    ]
;;
