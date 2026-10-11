module Alignment = Camlembert.Alignment
module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Line = Camlembert.Line
module Rect = Camlembert.Rect
module Style = Camlembert.Style

let origin = 0
let oversize_factor = 2
let halving_divisor = 2
let box_width = 6
let box_height = 3
let title_box_width = 10
let thin_box_width = 6
let border_thickness = 1
let padding_left = 1
let padding_right = 2
let padding_top = 3
let padding_bottom = 4
let padded_size = 14
let uniform_padding_size = 2
let negative_size = -1
let title_text = "Hi"
let footer_text = "Ok"
let long_title_text = "abcdefgh"
let blank_character = ' '
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height
let rect_testable = Alcotest.testable Rect.pp Rect.equal

let render block ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Block.widget block region buffer;
  buffer
;;

let lines_of block ~width ~height =
  Buffer.to_lines (render block ~width ~height)
;;

let blank_line = String.make box_width blank_character

let titled ?(alignment = Alignment.Left) text =
  Block.make
    ~borders:Block.all_sides
    ~title:(Line.of_string text)
    ~title_alignment:alignment
    ()
;;

let test_plain_borders_on_all_sides () =
  Alcotest.(check (list string))
    "plain"
    [ "┌────┐"; "│    │"; "└────┘" ]
    (lines_of
       (Block.make ~borders:Block.all_sides ())
       ~width:box_width
       ~height:box_height)
;;

let test_border_types () =
  let lines_with border_type =
    lines_of
      (Block.make ~borders:Block.all_sides ~border_type ())
      ~width:box_width
      ~height:box_height
  in
  Alcotest.(check (list string))
    "rounded"
    [ "╭────╮"; "│    │"; "╰────╯" ]
    (lines_with Block.Rounded);
  Alcotest.(check (list string))
    "double"
    [ "╔════╗"; "║    ║"; "╚════╝" ]
    (lines_with Block.Double);
  Alcotest.(check (list string))
    "thick"
    [ "┏━━━━┓"; "┃    ┃"; "┗━━━━┛" ]
    (lines_with Block.Thick)
;;

let test_horizontal_borders_only () =
  Alcotest.(check (list string))
    "top and bottom"
    [ "──────"; blank_line; "──────" ]
    (lines_of
       (Block.make ~borders:[ Block.Top; Block.Bottom ] ())
       ~width:box_width
       ~height:box_height)
;;

let test_vertical_borders_only () =
  Alcotest.(check (list string))
    "left and right"
    [ "│    │"; "│    │"; "│    │" ]
    (lines_of
       (Block.make ~borders:[ Block.Left; Block.Right ] ())
       ~width:box_width
       ~height:box_height)
;;

let test_corner_only_where_two_sides_meet () =
  Alcotest.(check (list string))
    "top and left"
    [ "┌─────"; "│     "; "│     " ]
    (lines_of
       (Block.make ~borders:[ Block.Top; Block.Left ] ())
       ~width:box_width
       ~height:box_height)
;;

let test_no_borders_draws_nothing () =
  Alcotest.(check (list string))
    "blank"
    [ blank_line; blank_line; blank_line ]
    (lines_of (Block.make ()) ~width:box_width ~height:box_height)
;;

let test_title_alignments () =
  let lines_with alignment =
    List.hd
      (lines_of
         (titled ~alignment title_text)
         ~width:title_box_width
         ~height:box_height)
  in
  Alcotest.(check string) "left" "┌Hi──────┐" (lines_with Alignment.Left);
  Alcotest.(check string) "center" "┌───Hi───┐" (lines_with Alignment.Center);
  Alcotest.(check string) "right" "┌──────Hi┐" (lines_with Alignment.Right)
;;

let test_title_is_cut_between_the_corners () =
  Alcotest.(check string)
    "cut"
    "┌abcd┐"
    (List.hd
       (lines_of
          (titled long_title_text)
          ~width:thin_box_width
          ~height:box_height))
;;

let test_title_without_top_border_takes_the_first_row () =
  let block =
    Block.make ~borders:[ Block.Left ] ~title:(Line.of_string title_text) ()
  in
  Alcotest.(check (list string))
    "title row"
    [ "│Hi   "; "│     " ]
    (lines_of block ~width:box_width ~height:(pred box_height))
;;

let test_footer_alignments () =
  let lines_with alignment =
    List.nth
      (lines_of
         (Block.make
            ~borders:Block.all_sides
            ~footer:(Line.of_string title_text)
            ~footer_alignment:alignment
            ())
         ~width:title_box_width
         ~height:box_height)
      (pred box_height)
  in
  Alcotest.(check string) "left" "└Hi──────┘" (lines_with Alignment.Left);
  Alcotest.(check string) "center" "└───Hi───┘" (lines_with Alignment.Center);
  Alcotest.(check string) "right" "└──────Hi┘" (lines_with Alignment.Right)
;;

let test_footer_without_bottom_border_takes_the_last_row () =
  let block =
    Block.make ~borders:[ Block.Left ] ~footer:(Line.of_string title_text) ()
  in
  Alcotest.(check (list string))
    "footer row"
    [ "│     "; "│Hi   " ]
    (lines_of block ~width:box_width ~height:(pred box_height))
;;

let test_title_and_footer_together () =
  let block =
    Block.make
      ~borders:Block.all_sides
      ~title:(Line.of_string title_text)
      ~footer:(Line.of_string footer_text)
      ~footer_alignment:Alignment.Right
      ()
  in
  Alcotest.(check (list string))
    "both"
    [ "┌Hi──┐"; "│    │"; "└──Ok┘" ]
    (lines_of block ~width:box_width ~height:box_height)
;;

let test_inner_area_removes_the_borders () =
  let region = area ~width:box_width ~height:box_height in
  let inner_of borders = Block.inner (Block.make ~borders ()) region in
  let shrunk ~top ~bottom ~left ~right =
    Rect.shrink region ~top ~bottom ~left ~right
  in
  let none = Int.zero in
  Alcotest.check
    rect_testable
    "all sides"
    (shrunk
       ~top:border_thickness
       ~bottom:border_thickness
       ~left:border_thickness
       ~right:border_thickness)
    (inner_of Block.all_sides);
  Alcotest.check
    rect_testable
    "top and left"
    (shrunk
       ~top:border_thickness
       ~bottom:none
       ~left:border_thickness
       ~right:none)
    (inner_of [ Block.Top; Block.Left ]);
  Alcotest.check rect_testable "no borders" region (inner_of [])
;;

let test_title_reserves_the_top_row () =
  let region = area ~width:box_width ~height:box_height in
  let block = Block.make ~title:(Line.of_string title_text) () in
  Alcotest.check
    rect_testable
    "top row reserved"
    (Rect.shrink
       region
       ~top:border_thickness
       ~bottom:Int.zero
       ~left:Int.zero
       ~right:Int.zero)
    (Block.inner block region)
;;

let test_footer_reserves_the_bottom_row () =
  let region = area ~width:box_width ~height:box_height in
  let block = Block.make ~footer:(Line.of_string footer_text) () in
  Alcotest.check
    rect_testable
    "bottom row reserved"
    (Rect.shrink
       region
       ~top:Int.zero
       ~bottom:border_thickness
       ~left:Int.zero
       ~right:Int.zero)
    (Block.inner block region)
;;

let test_padding_keeps_the_content_away_from_the_borders () =
  let region = area ~width:padded_size ~height:padded_size in
  let padding =
    { Block.left = padding_left
    ; right = padding_right
    ; top = padding_top
    ; bottom = padding_bottom
    }
  in
  Alcotest.check
    rect_testable
    "with borders"
    (Rect.shrink
       region
       ~top:(border_thickness + padding_top)
       ~bottom:(border_thickness + padding_bottom)
       ~left:(border_thickness + padding_left)
       ~right:(border_thickness + padding_right))
    (Block.inner (Block.make ~borders:Block.all_sides ~padding ()) region);
  Alcotest.check
    rect_testable
    "without borders"
    (Rect.shrink
       region
       ~top:padding_top
       ~bottom:padding_bottom
       ~left:padding_left
       ~right:padding_right)
    (Block.inner (Block.make ~padding ()) region)
;;

let test_uniform_padding_is_the_same_on_every_side () =
  let region = area ~width:padded_size ~height:padded_size in
  Alcotest.check
    rect_testable
    "uniform"
    (Rect.shrink
       region
       ~top:uniform_padding_size
       ~bottom:uniform_padding_size
       ~left:uniform_padding_size
       ~right:uniform_padding_size)
    (Block.inner
       (Block.make ~padding:(Block.uniform_padding uniform_padding_size) ())
       region);
  Alcotest.check
    rect_testable
    "none"
    region
    (Block.inner (Block.make ~padding:Block.no_padding ()) region)
;;

let test_padding_cannot_be_negative () =
  Alcotest.check_raises
    "negative"
    (Invalid_argument "Block.make: padding cannot be negative")
    (fun () ->
       ignore
         (Block.make
            ~padding:{ Block.no_padding with Block.left = negative_size }
            ()))
;;

let test_styles_apply_to_the_right_cells () =
  let background = Style.make ~background:Color.Blue () in
  let border_color = Style.make ~foreground:Color.Red () in
  let block =
    Block.make
      ~borders:Block.all_sides
      ~style:background
      ~border_style:border_color
      ()
  in
  let buffer = render block ~width:box_width ~height:box_height in
  let style_at ~column ~row = (Buffer.get buffer ~column ~row).Cell.style in
  let middle = box_width / halving_divisor in
  Alcotest.(check bool)
    "border has both"
    true
    (Style.equal
       (Style.patch background border_color)
       (style_at ~column:origin ~row:origin));
  Alcotest.(check bool)
    "interior has the background only"
    true
    (Style.equal background (style_at ~column:middle ~row:border_thickness))
;;

let test_empty_area_draws_nothing () =
  let empty = area ~width:Int.zero ~height:Int.zero in
  let buffer = Buffer.create (area ~width:box_width ~height:box_height) in
  Block.widget (titled title_text) empty buffer;
  Alcotest.(check (list string))
    "blank"
    [ blank_line; blank_line; blank_line ]
    (Buffer.to_lines buffer)
;;

let test_block_larger_than_the_buffer_is_clipped () =
  let buffer = Buffer.create (area ~width:box_width ~height:box_height) in
  let oversized =
    area
      ~width:(oversize_factor * box_width)
      ~height:(oversize_factor * box_height)
  in
  Block.widget (titled title_text) oversized buffer;
  Alcotest.(check string)
    "first row"
    "┌Hi───"
    (List.hd (Buffer.to_lines buffer))
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a block only changes cells inside its area"
    ~print:
      QCheck2.Print.(
        triple
          Generators.print_block_parameters
          Generators.print_rect
          Generators.print_edits)
    QCheck2.Gen.(
      triple Generators.block_parameters Generators.rect Generators.edits)
    (fun (parameters, region, edits) ->
       let untouched = Support.build_buffer edits in
       let drawn = Support.build_buffer edits in
       Block.widget (Generators.block_of_parameters parameters) region drawn;
       Support.same_cells_outside region untouched drawn)
;;

let property_inner_area_lies_inside_the_area =
  QCheck2.Test.make
    ~name:"the inner area lies inside the block area"
    ~print:
      QCheck2.Print.(
        pair Generators.print_block_parameters Generators.print_rect)
    QCheck2.Gen.(pair Generators.block_parameters Generators.rect)
    (fun (parameters, region) ->
       let inner =
         Block.inner (Generators.block_of_parameters parameters) region
       in
       Rect.equal (Rect.intersection region inner) inner)
;;

let () =
  Alcotest.run
    "Block"
    [ ( "borders"
      , [ Alcotest.test_case "all sides" `Quick test_plain_borders_on_all_sides
        ; Alcotest.test_case "border types" `Quick test_border_types
        ; Alcotest.test_case
            "horizontal only"
            `Quick
            test_horizontal_borders_only
        ; Alcotest.test_case "vertical only" `Quick test_vertical_borders_only
        ; Alcotest.test_case
            "corners need two sides"
            `Quick
            test_corner_only_where_two_sides_meet
        ; Alcotest.test_case "no borders" `Quick test_no_borders_draws_nothing
        ] )
    ; ( "title"
      , [ Alcotest.test_case "alignments" `Quick test_title_alignments
        ; Alcotest.test_case "cut" `Quick test_title_is_cut_between_the_corners
        ; Alcotest.test_case
            "without top border"
            `Quick
            test_title_without_top_border_takes_the_first_row
        ] )
    ; ( "footer"
      , [ Alcotest.test_case "alignments" `Quick test_footer_alignments
        ; Alcotest.test_case
            "without bottom border"
            `Quick
            test_footer_without_bottom_border_takes_the_last_row
        ; Alcotest.test_case
            "with a title"
            `Quick
            test_title_and_footer_together
        ] )
    ; ( "inner"
      , [ Alcotest.test_case
            "removes borders"
            `Quick
            test_inner_area_removes_the_borders
        ; Alcotest.test_case
            "reserves the title row"
            `Quick
            test_title_reserves_the_top_row
        ; Alcotest.test_case
            "reserves the footer row"
            `Quick
            test_footer_reserves_the_bottom_row
        ; Alcotest.test_case
            "padding"
            `Quick
            test_padding_keeps_the_content_away_from_the_borders
        ; Alcotest.test_case
            "uniform padding"
            `Quick
            test_uniform_padding_is_the_same_on_every_side
        ; Alcotest.test_case
            "negative padding"
            `Quick
            test_padding_cannot_be_negative
        ] )
    ; ( "styles and clipping"
      , [ Alcotest.test_case
            "styles"
            `Quick
            test_styles_apply_to_the_right_cells
        ; Alcotest.test_case "empty area" `Quick test_empty_area_draws_nothing
        ; Alcotest.test_case
            "larger than the buffer"
            `Quick
            test_block_larger_than_the_buffer_is_clipped
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_drawing_stays_inside_the_area
          ; property_inner_area_lies_inside_the_area
          ] )
    ]
;;
