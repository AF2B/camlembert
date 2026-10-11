module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Canvas = Camlembert.Canvas
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Line = Camlembert.Line
module Marker = Camlembert.Marker
module Rect = Camlembert.Rect
module Style = Camlembert.Style

let origin = 0
let square_size = 4
let last_coordinate = 3.0
let unit_coordinate = 1.0
let second_row_up = 2.0
let label_canvas_right = 4.0
let label_canvas_top = 2.0
let near_the_right_edge = 3.0
let second_row_from_the_bottom = 1.0
let nowhere_in_the_canvas = 9.0
let nearest_center = 3.0
let furthest_center = 7.0
let smallest_radius = 0.5
let largest_radius = 2.5
let lowest_coordinate = 0.0
let beyond_the_bounds = 5.0
let far_beyond_the_bounds = 1_000_000.0
let braille_width = 2
let one_cell = 1
let label_canvas_width = 5
let label_canvas_height = 3
let framed_size = 6
let red = Style.make ~foreground:Color.Red ()
let blue = Style.make ~foreground:Color.Blue ()
let full = "\u{2588}"
let upper_half = "\u{2580}"
let lower_half = "\u{2584}"
let bounds = lowest_coordinate, last_coordinate
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render canvas ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Canvas.widget canvas region buffer;
  buffer
;;

let lines_of canvas ~width ~height =
  Buffer.to_lines (render canvas ~width ~height)
;;

(* A canvas where every dot is a cell, so the coordinates of the shapes are the
   columns and the rows counted from the lower left corner. *)
let blocks ?labels shapes =
  Canvas.make
    ?labels
    ~marker:Marker.Block
    ~x_bounds:bounds
    ~y_bounds:bounds
    shapes
;;

let square_lines canvas = lines_of canvas ~width:square_size ~height:square_size
let point x y = x, y

let test_points_are_drawn_where_their_coordinates_say () =
  let canvas =
    blocks
      [ Canvas.Points
          { coordinates =
              [ point lowest_coordinate lowest_coordinate
              ; point last_coordinate last_coordinate
              ]
          ; style = Style.default
          }
      ]
  in
  Alcotest.(check (list string))
    "corners"
    [ "   " ^ full; "    "; "    "; full ^ "   " ]
    (square_lines canvas)
;;

let test_a_horizontal_segment_fills_a_row () =
  let canvas =
    blocks
      [ Canvas.Segment
          { x1 = lowest_coordinate
          ; y1 = lowest_coordinate
          ; x2 = last_coordinate
          ; y2 = lowest_coordinate
          ; style = Style.default
          }
      ]
  in
  Alcotest.(check (list string))
    "bottom row"
    [ "    "
    ; "    "
    ; "    "
    ; String.concat "" (List.init square_size (Fun.const full))
    ]
    (square_lines canvas)
;;

let test_a_diagonal_segment () =
  let canvas =
    blocks
      [ Canvas.Segment
          { x1 = lowest_coordinate
          ; y1 = lowest_coordinate
          ; x2 = last_coordinate
          ; y2 = last_coordinate
          ; style = Style.default
          }
      ]
  in
  Alcotest.(check (list string))
    "diagonal"
    [ "   " ^ full; "  " ^ full ^ " "; " " ^ full ^ "  "; full ^ "   " ]
    (square_lines canvas)
;;

let test_a_rectangle_is_an_outline () =
  let canvas =
    blocks
      [ Canvas.Rectangle
          { x = lowest_coordinate
          ; y = lowest_coordinate
          ; width = last_coordinate
          ; height = last_coordinate
          ; style = Style.default
          }
      ]
  in
  let side = full ^ "  " ^ full in
  let whole = String.concat "" (List.init square_size (Fun.const full)) in
  Alcotest.(check (list string))
    "outline"
    [ whole; side; side; whole ]
    (square_lines canvas)
;;

let test_segments_are_cut_to_the_bounds () =
  let row = second_row_from_the_bottom in
  let canvas =
    blocks
      [ Canvas.Segment
          { x1 = Float.neg far_beyond_the_bounds
          ; y1 = row
          ; x2 = far_beyond_the_bounds
          ; y2 = row
          ; style = Style.default
          }
      ]
  in
  Alcotest.(check (list string))
    "the part inside"
    [ "    "
    ; "    "
    ; String.concat "" (List.init square_size (Fun.const full))
    ; "    "
    ]
    (square_lines canvas)
;;

let test_what_is_outside_of_the_bounds_is_left_out () =
  let canvas =
    blocks
      [ Canvas.Points
          { coordinates = [ point beyond_the_bounds beyond_the_bounds ]
          ; style = Style.default
          }
      ; Canvas.Segment
          { x1 = beyond_the_bounds
          ; y1 = lowest_coordinate
          ; x2 = far_beyond_the_bounds
          ; y2 = last_coordinate
          ; style = Style.default
          }
      ]
  in
  Alcotest.(check (list string))
    "nothing"
    (List.init square_size (fun _ -> String.make square_size ' '))
    (square_lines canvas)
;;

let test_braille_markers_pack_eight_dots_in_a_cell () =
  let canvas shapes = Canvas.make ~x_bounds:bounds ~y_bounds:bounds shapes in
  Alcotest.(check (list string))
    "a line along the bottom"
    [ "\u{28C0}\u{28C0}" ]
    (lines_of
       (canvas
          [ Canvas.Segment
              { x1 = lowest_coordinate
              ; y1 = lowest_coordinate
              ; x2 = last_coordinate
              ; y2 = lowest_coordinate
              ; style = Style.default
              }
          ])
       ~width:braille_width
       ~height:one_cell);
  Alcotest.(check (list string))
    "a diagonal"
    [ "\u{2860}\u{280A}" ]
    (lines_of
       (canvas
          [ Canvas.Segment
              { x1 = lowest_coordinate
              ; y1 = lowest_coordinate
              ; x2 = last_coordinate
              ; y2 = last_coordinate
              ; style = Style.default
              }
          ])
       ~width:braille_width
       ~height:one_cell)
;;

let test_half_blocks_split_a_cell_in_two () =
  let canvas coordinates =
    Canvas.make
      ~marker:Marker.Half_block
      ~x_bounds:(lowest_coordinate, unit_coordinate)
      ~y_bounds:(lowest_coordinate, unit_coordinate)
      [ Canvas.Points { coordinates; style = Style.default } ]
  in
  let lines coordinates =
    lines_of (canvas coordinates) ~width:one_cell ~height:one_cell
  in
  Alcotest.(check (list string))
    "top"
    [ upper_half ]
    (lines [ point unit_coordinate unit_coordinate ]);
  Alcotest.(check (list string))
    "bottom"
    [ lower_half ]
    (lines [ point unit_coordinate lowest_coordinate ]);
  Alcotest.(check (list string))
    "both"
    [ full ]
    (lines
       [ point unit_coordinate unit_coordinate
       ; point unit_coordinate lowest_coordinate
       ])
;;

let test_shapes_take_the_style_of_their_dots_and_later_shapes_win () =
  let at_origin style =
    Canvas.Points
      { coordinates = [ point lowest_coordinate lowest_coordinate ]; style }
  in
  let style_at_origin canvas =
    (Buffer.get
       (render canvas ~width:square_size ~height:square_size)
       ~column:origin
       ~row:(pred square_size))
      .Cell.style
  in
  Alcotest.(check bool)
    "red"
    true
    (Style.equal red (style_at_origin (blocks [ at_origin red ])));
  Alcotest.(check bool)
    "the later one wins"
    true
    (Style.equal
       blue
       (style_at_origin (blocks [ at_origin red; at_origin blue ])))
;;

let test_labels_are_written_over_the_shapes () =
  let canvas =
    Canvas.make
      ~marker:Marker.Block
      ~labels:
        [ { Canvas.x = lowest_coordinate
          ; y = second_row_up
          ; text = Line.of_string "ab"
          }
        ; { Canvas.x = label_canvas_right
          ; y = lowest_coordinate
          ; text = Line.of_string "Z"
          }
        ; { Canvas.x = near_the_right_edge
          ; y = second_row_from_the_bottom
          ; text = Line.of_string "xyz"
          }
        ; { Canvas.x = nowhere_in_the_canvas
          ; y = nowhere_in_the_canvas
          ; text = Line.of_string "gone"
          }
        ]
      ~x_bounds:(lowest_coordinate, label_canvas_right)
      ~y_bounds:(lowest_coordinate, label_canvas_top)
      []
  in
  Alcotest.(check (list string))
    "labels"
    [ "ab   "; "   xy"; "    Z" ]
    (lines_of canvas ~width:label_canvas_width ~height:label_canvas_height)
;;

let test_a_block_frames_the_canvas () =
  let block = Block.make ~borders:Block.all_sides () in
  let canvas =
    Canvas.make
      ~block
      ~marker:Marker.Block
      ~x_bounds:bounds
      ~y_bounds:bounds
      [ Canvas.Points
          { coordinates = [ point lowest_coordinate lowest_coordinate ]
          ; style = Style.default
          }
      ]
  in
  let lines = lines_of canvas ~width:framed_size ~height:framed_size in
  Alcotest.(check string)
    "inside the frame"
    ("\u{2502}" ^ full ^ "   \u{2502}")
    (List.nth lines (pred (pred framed_size)))
;;

let test_a_circle_larger_than_any_terminal_does_not_take_forever () =
  let canvas =
    blocks
      [ Canvas.Circle
          { x = unit_coordinate
          ; y = unit_coordinate
          ; radius = far_beyond_the_bounds *. far_beyond_the_bounds
          ; style = Style.default
          }
      ]
  in
  Alcotest.(check (list string))
    "nothing of it reaches the canvas"
    (List.init square_size (fun _ -> String.make square_size ' '))
    (square_lines canvas)
;;

let test_invalid_bounds_are_rejected () =
  let rejects message ~x_bounds ~y_bounds =
    Alcotest.check_raises
      message
      (Invalid_argument
         "Canvas.make: bounds must be finite, the lowest one first")
      (fun () -> ignore (Canvas.make ~x_bounds ~y_bounds []))
  in
  rejects
    "empty range"
    ~x_bounds:(unit_coordinate, unit_coordinate)
    ~y_bounds:bounds;
  rejects
    "reversed"
    ~x_bounds:bounds
    ~y_bounds:(last_coordinate, lowest_coordinate);
  rejects "not a number" ~x_bounds:(Float.nan, unit_coordinate) ~y_bounds:bounds;
  rejects
    "infinite"
    ~x_bounds:bounds
    ~y_bounds:(lowest_coordinate, Float.infinity)
;;

let max_cells = 12
let max_coordinate = 10.0
let canvas_bounds = lowest_coordinate, max_coordinate
let markers = [ Marker.Braille; Marker.Half_block; Marker.Block; Marker.Dot ]
let coordinate = QCheck2.Gen.float_range lowest_coordinate max_coordinate
let marker = QCheck2.Gen.oneof_list markers

let print_marker = function
  | Marker.Braille -> "Braille"
  | Marker.Half_block -> "Half_block"
  | Marker.Block -> "Block"
  | Marker.Dot -> "Dot"
;;

let painted_cells buffer =
  Buffer.to_lines buffer
  |> List.concat_map (fun line ->
    List.filter
      (fun cluster -> not (String.equal cluster " "))
      (Camlembert.Grapheme.clusters line))
  |> List.length
;;

let property_a_point_inside_the_bounds_is_always_drawn =
  QCheck2.Test.make
    ~name:"a point inside the bounds is always drawn"
    ~print:QCheck2.Print.(quad float float print_marker (pair int int))
    QCheck2.Gen.(
      quad
        coordinate
        coordinate
        marker
        (pair (int_range Int.one max_cells) (int_range Int.one max_cells)))
    (fun (x, y, marker, (width, height)) ->
       let canvas =
         Canvas.make
           ~marker
           ~x_bounds:canvas_bounds
           ~y_bounds:canvas_bounds
           [ Canvas.Points { coordinates = [ x, y ]; style = Style.default } ]
       in
       painted_cells (render canvas ~width ~height) = one_cell)
;;

let property_segments_anywhere_are_drawn_without_failing_and_inside =
  QCheck2.Test.make
    ~name:"segments, even far outside, are drawn inside the area"
    ~print:
      QCheck2.Print.(
        pair
          (quad float float float float)
          (pair Generators.print_rect Generators.print_edits))
    QCheck2.Gen.(
      pair
        (quad
           (float_range (Float.neg far_beyond_the_bounds) far_beyond_the_bounds)
           (float_range (Float.neg far_beyond_the_bounds) far_beyond_the_bounds)
           (float_range (Float.neg far_beyond_the_bounds) far_beyond_the_bounds)
           (float_range (Float.neg far_beyond_the_bounds) far_beyond_the_bounds))
        (pair Generators.rect Generators.edits))
    (fun ((x1, y1, x2, y2), (region, edits)) ->
       let canvas =
         Canvas.make
           ~x_bounds:canvas_bounds
           ~y_bounds:canvas_bounds
           [ Canvas.Segment { x1; y1; x2; y2; style = Style.default }
           ; Canvas.Circle
               { x = x1; y = y1; radius = Float.abs x2; style = Style.default }
           ]
       in
       Support.leaves_the_outside_untouched
         (Canvas.widget canvas)
         ~region
         ~edits)
;;

let property_a_circle_stays_inside_its_bounding_box =
  QCheck2.Test.make
    ~name:"a circle only paints cells of its bounding box"
    ~print:QCheck2.Print.(triple float float float)
    QCheck2.Gen.(
      triple
        (float_range nearest_center furthest_center)
        (float_range nearest_center furthest_center)
        (float_range smallest_radius largest_radius))
    (fun (x, y, radius) ->
       let size = max_cells in
       let canvas =
         Canvas.make
           ~marker:Marker.Block
           ~x_bounds:canvas_bounds
           ~y_bounds:canvas_bounds
           [ Canvas.Circle { x; y; radius; style = Style.default } ]
       in
       let buffer = render canvas ~width:size ~height:size in
       let cells_per_unit = Float.of_int (pred size) /. max_coordinate in
       let allowed_low value =
         Float.to_int (Float.floor ((value -. radius) *. cells_per_unit))
       in
       let allowed_high value =
         Float.to_int (Float.ceil ((value +. radius) *. cells_per_unit))
       in
       List.for_all
         (fun row ->
            List.for_all
              (fun column ->
                 let painted =
                   not
                     (String.equal
                        (Buffer.get buffer ~column ~row).Cell.symbol
                        " ")
                 in
                 let world_row = pred size - row in
                 (not painted)
                 || (column >= allowed_low x
                     && column <= allowed_high x
                     && world_row >= allowed_low y
                     && world_row <= allowed_high y))
              (List.init size Fun.id))
         (List.init size Fun.id))
;;

let () =
  Alcotest.run
    "Canvas"
    [ ( "shapes"
      , [ Alcotest.test_case
            "points"
            `Quick
            test_points_are_drawn_where_their_coordinates_say
        ; Alcotest.test_case
            "horizontal segment"
            `Quick
            test_a_horizontal_segment_fills_a_row
        ; Alcotest.test_case "diagonal segment" `Quick test_a_diagonal_segment
        ; Alcotest.test_case "rectangle" `Quick test_a_rectangle_is_an_outline
        ; Alcotest.test_case
            "segments are cut"
            `Quick
            test_segments_are_cut_to_the_bounds
        ; Alcotest.test_case
            "outside of the bounds"
            `Quick
            test_what_is_outside_of_the_bounds_is_left_out
        ] )
    ; ( "markers"
      , [ Alcotest.test_case
            "braille"
            `Quick
            test_braille_markers_pack_eight_dots_in_a_cell
        ; Alcotest.test_case
            "half blocks"
            `Quick
            test_half_blocks_split_a_cell_in_two
        ] )
    ; ( "styles, labels and frames"
      , [ Alcotest.test_case
            "styles"
            `Quick
            test_shapes_take_the_style_of_their_dots_and_later_shapes_win
        ; Alcotest.test_case
            "labels"
            `Quick
            test_labels_are_written_over_the_shapes
        ; Alcotest.test_case "block" `Quick test_a_block_frames_the_canvas
        ; Alcotest.test_case
            "huge circle"
            `Quick
            test_a_circle_larger_than_any_terminal_does_not_take_forever
        ; Alcotest.test_case
            "invalid bounds"
            `Quick
            test_invalid_bounds_are_rejected
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_a_point_inside_the_bounds_is_always_drawn
          ; property_segments_anywhere_are_drawn_without_failing_and_inside
          ; property_a_circle_stays_inside_its_bounding_box
          ] )
    ]
;;
