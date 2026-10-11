module Grapheme = Camlembert.Grapheme
module Marker = Camlembert.Marker

let braille_columns = 2
let braille_rows = 4
let first_column = 0
let second_column = 1
let first_row = 0
let second_row = 1
let last_row = 3
let blank_braille = "\u{2800}"
let upper_left_dot = "\u{2801}"
let lower_right_dot = "\u{2880}"
let all_dots = "\u{28FF}"
let bottom_line = "\u{28C0}"
let top_line = "\u{2809}"
let upper_half = "\u{2580}"
let lower_half = "\u{2584}"
let full = "\u{2588}"
let bullet = "\u{2022}"
let one_dot = 1
let two_rows = 2
let one_column = 1
let braille_first = 0x2800
let braille_last = 0x28FF

let only ~column ~row ~painted_column ~painted_row =
  column = painted_column && row = painted_row
;;

let everywhere ~column:_ ~row:_ = true
let nowhere ~column:_ ~row:_ = false

let test_dots_per_cell () =
  Alcotest.(check (pair int int))
    "braille"
    (braille_columns, braille_rows)
    (Marker.dots_per_cell Marker.Braille);
  Alcotest.(check (pair int int))
    "half block"
    (one_dot, two_rows)
    (Marker.dots_per_cell Marker.Half_block);
  Alcotest.(check (pair int int))
    "block"
    (one_dot, one_dot)
    (Marker.dots_per_cell Marker.Block);
  Alcotest.(check (pair int int))
    "dot"
    (one_dot, one_dot)
    (Marker.dots_per_cell Marker.Dot)
;;

let test_braille_patterns () =
  let symbol painted = Marker.symbol Marker.Braille ~painted in
  Alcotest.(check string) "no dot" blank_braille (symbol nowhere);
  Alcotest.(check string)
    "upper left dot"
    upper_left_dot
    (symbol (only ~painted_column:first_column ~painted_row:first_row));
  Alcotest.(check string)
    "lower right dot"
    lower_right_dot
    (symbol (only ~painted_column:second_column ~painted_row:last_row));
  Alcotest.(check string) "every dot" all_dots (symbol everywhere);
  Alcotest.(check string)
    "a line along the bottom"
    bottom_line
    (symbol (fun ~column:_ ~row -> row = last_row));
  Alcotest.(check string)
    "a line along the top"
    top_line
    (symbol (fun ~column:_ ~row ->
       row = first_row || (row = second_row && false)))
;;

let test_half_blocks () =
  let symbol ~top ~bottom =
    Marker.symbol Marker.Half_block ~painted:(fun ~column:_ ~row ->
      if row = first_row then top else bottom)
  in
  Alcotest.(check string) "none" " " (symbol ~top:false ~bottom:false);
  Alcotest.(check string) "top" upper_half (symbol ~top:true ~bottom:false);
  Alcotest.(check string) "bottom" lower_half (symbol ~top:false ~bottom:true);
  Alcotest.(check string) "both" full (symbol ~top:true ~bottom:true)
;;

let test_single_dot_markers_ignore_which_dots_are_painted () =
  Alcotest.(check string)
    "block"
    full
    (Marker.symbol Marker.Block ~painted:nowhere);
  Alcotest.(check string)
    "dot"
    bullet
    (Marker.symbol Marker.Dot ~painted:nowhere)
;;

let dot_positions =
  List.concat_map
    (fun column -> List.init braille_rows (fun row -> column, row))
    (List.init braille_columns Fun.id)
;;

let dot_subset =
  QCheck2.Gen.(
    map
      (fun flags ->
         List.filteri (fun index _ -> List.nth flags index) dot_positions)
      (list_size (return (List.length dot_positions)) bool))
;;

let painted_in subset ~column ~row = List.mem (column, row) subset

let property_a_braille_symbol_is_one_narrow_character_of_the_block =
  QCheck2.Test.make
    ~name:
      "a Braille symbol is one character of the Braille block, one column wide"
    ~print:QCheck2.Print.(list (pair int int))
    dot_subset
    (fun subset ->
       let symbol = Marker.symbol Marker.Braille ~painted:(painted_in subset) in
       let scalar =
         Uchar.to_int
           (Uchar.utf_decode_uchar (String.get_utf_8_uchar symbol Int.zero))
       in
       scalar >= braille_first
       && scalar <= braille_last
       && Grapheme.width symbol = one_column)
;;

let property_different_dots_give_different_symbols =
  QCheck2.Test.make
    ~name:"different sets of dots give different symbols"
    ~print:QCheck2.Print.(pair (list (pair int int)) (list (pair int int)))
    QCheck2.Gen.(pair dot_subset dot_subset)
    (fun (first, second) ->
       let symbol subset =
         Marker.symbol Marker.Braille ~painted:(painted_in subset)
       in
       String.equal (symbol first) (symbol second)
       = (List.sort compare first = List.sort compare second))
;;

let () =
  Alcotest.run
    "Marker"
    [ ( "symbols"
      , [ Alcotest.test_case "dots per cell" `Quick test_dots_per_cell
        ; Alcotest.test_case "braille" `Quick test_braille_patterns
        ; Alcotest.test_case "half blocks" `Quick test_half_blocks
        ; Alcotest.test_case
            "single dot markers"
            `Quick
            test_single_dot_markers_ignore_which_dots_are_painted
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_a_braille_symbol_is_one_narrow_character_of_the_block
          ; property_different_dots_give_different_symbols
          ] )
    ]
;;
