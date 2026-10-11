module Block = Camlembert.Block
module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Constraint = Camlembert.Constraint
module Grapheme = Camlembert.Grapheme
module Line = Camlembert.Line
module Rect = Camlembert.Rect
module Selection = Camlembert.Selection
module Style = Camlembert.Style
module Table = Camlembert.Table

let origin = 0
let table_width = 12
let table_height = 4
let header_and_two_rows = 3
let framed_height = 6
let name_column = 6
let age_column = 3
let wide_gap = 3
let first_row = 0
let second_row = 1
let last_person = 3
let symbol = "> "
let gap = " "
let highlight = Style.make ~foreground:Color.Yellow ()
let cell text = Line.of_string text
let widths = [ Constraint.Length name_column; Constraint.Length age_column ]

let people =
  List.map
    (fun (name, age) -> [ cell name; cell age ])
    [ "Ann", "30"; "Bob", "41"; "Cyd", "7"; "Dee", "52" ]
;;

let header = [ cell "Name"; cell "Age" ]
let area ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

let render table selection ~width ~height =
  let region = area ~width ~height in
  let buffer = Buffer.create region in
  Table.widget table region buffer selection;
  buffer
;;

let lines_of table selection ~width ~height =
  Buffer.to_lines (render table selection ~width ~height)
;;

let selection_on position =
  let selection = Selection.create () in
  Selection.select selection position;
  selection
;;

let pad text ~width = text ^ String.make (width - Grapheme.width text) ' '

(* A row of the screen made of cells put in columns of the given widths. *)
let row_of cells ~columns ~spacing ~total_width =
  let separator = String.make spacing ' ' in
  pad
    (String.concat
       separator
       (List.map2 (fun text width -> pad text ~width) cells columns))
    ~width:total_width
;;

let standard_row cells =
  row_of
    cells
    ~columns:[ name_column; age_column ]
    ~spacing:1
    ~total_width:table_width
;;

let test_cells_are_lined_up_in_columns () =
  Alcotest.(check (list string))
    "rows"
    [ standard_row [ "Ann"; "30" ]
    ; standard_row [ "Bob"; "41" ]
    ; standard_row [ "Cyd"; "7" ]
    ; standard_row [ "Dee"; "52" ]
    ]
    (lines_of
       (Table.make ~widths people)
       (Selection.create ())
       ~width:table_width
       ~height:table_height)
;;

let test_the_header_goes_on_top () =
  Alcotest.(check (list string))
    "rows"
    [ standard_row [ "Name"; "Age" ]
    ; standard_row [ "Ann"; "30" ]
    ; standard_row [ "Bob"; "41" ]
    ; standard_row [ "Cyd"; "7" ]
    ]
    (lines_of
       (Table.make ~header ~widths people)
       (Selection.create ())
       ~width:table_width
       ~height:table_height)
;;

let test_the_spacing_between_columns_can_change () =
  Alcotest.(check string)
    "wide gap"
    (row_of
       [ "Ann"; "30" ]
       ~columns:[ name_column; age_column ]
       ~spacing:wide_gap
       ~total_width:table_width)
    (List.hd
       (lines_of
          (Table.make ~column_spacing:wide_gap ~widths people)
          (Selection.create ())
          ~width:table_width
          ~height:Int.one))
;;

let test_a_fill_column_takes_the_room_that_is_left () =
  let fill_widths =
    [ Constraint.Length name_column; Constraint.Fill Int.one ]
  in
  let filling_column = table_width - name_column - String.length gap in
  Alcotest.(check string)
    "row"
    (row_of
       [ "Ann"; "30" ]
       ~columns:[ name_column; filling_column ]
       ~spacing:1
       ~total_width:table_width)
    (List.hd
       (lines_of
          (Table.make ~widths:fill_widths people)
          (Selection.create ())
          ~width:table_width
          ~height:Int.one))
;;

let test_the_symbol_and_the_highlight_mark_the_selected_row () =
  let table =
    Table.make
      ~highlight_style:highlight
      ~highlight_symbol:symbol
      ~widths
      people
  in
  let buffer =
    render
      table
      (selection_on (Some second_row))
      ~width:table_width
      ~height:table_height
  in
  let lines = Buffer.to_lines buffer in
  let last_column = pred table_width in
  Alcotest.(check string)
    "selected row starts with the symbol"
    (pad (symbol ^ "Bob    41") ~width:table_width)
    (List.nth lines second_row);
  Alcotest.(check string)
    "other rows leave room for it"
    (pad "  Ann    30" ~width:table_width)
    (List.nth lines first_row);
  Alcotest.(check bool)
    "highlight reaches the end of the row"
    true
    (Style.equal
       highlight
       (Buffer.get buffer ~column:last_column ~row:second_row).Cell.style)
;;

let test_the_header_stays_while_the_rows_scroll () =
  let selection = selection_on (Some last_person) in
  Alcotest.(check (list string))
    "header and the last two rows"
    [ standard_row [ "Name"; "Age" ]
    ; standard_row [ "Cyd"; "7" ]
    ; standard_row [ "Dee"; "52" ]
    ]
    (lines_of
       (Table.make ~header ~widths people)
       selection
       ~width:table_width
       ~height:header_and_two_rows)
;;

let test_long_cells_are_cut_to_their_column () =
  let rows = [ [ cell "Alexander"; cell "30" ] ] in
  Alcotest.(check string)
    "cut"
    (standard_row [ "Alexan"; "30" ])
    (List.hd
       (lines_of
          (Table.make ~widths rows)
          (Selection.create ())
          ~width:table_width
          ~height:Int.one))
;;

let test_missing_cells_are_blank_and_extra_cells_are_ignored () =
  let rows = [ [ cell "Ann" ]; [ cell "Bob"; cell "41"; cell "extra" ] ] in
  Alcotest.(check (list string))
    "rows"
    [ standard_row [ "Ann"; "" ]; standard_row [ "Bob"; "41" ] ]
    (lines_of
       (Table.make ~widths rows)
       (Selection.create ())
       ~width:table_width
       ~height:(succ Int.one))
;;

let test_a_block_frames_the_table () =
  let block = Block.make ~borders:Block.all_sides () in
  let lines =
    lines_of
      (Table.make ~block ~header ~widths people)
      (Selection.create ())
      ~width:(table_width + 2)
      ~height:framed_height
  in
  Alcotest.(check (list string))
    "framed"
    [ "┌" ^ String.concat "" (List.init table_width (fun _ -> "─")) ^ "┐"
    ; "│" ^ standard_row [ "Name"; "Age" ] ^ "│"
    ; "│" ^ standard_row [ "Ann"; "30" ] ^ "│"
    ; "│" ^ standard_row [ "Bob"; "41" ] ^ "│"
    ; "│" ^ standard_row [ "Cyd"; "7" ] ^ "│"
    ; "└" ^ String.concat "" (List.init table_width (fun _ -> "─")) ^ "┘"
    ]
    lines
;;

let property_drawing_stays_inside_the_area =
  QCheck2.Test.make
    ~name:"a table only changes cells inside its area"
    ~print:
      QCheck2.Print.(
        triple (option int) Generators.print_rect Generators.print_edits)
    QCheck2.Gen.(
      triple
        (option (int_range Int.zero last_person))
        Generators.rect
        Generators.edits)
    (fun (selected, region, edits) ->
       let table = Table.make ~header ~highlight_symbol:symbol ~widths people in
       Support.leaves_the_outside_untouched
         (fun region buffer ->
            Table.widget table region buffer (selection_on selected))
         ~region
         ~edits)
;;

let () =
  Alcotest.run
    "Table"
    [ ( "columns"
      , [ Alcotest.test_case
            "lined up"
            `Quick
            test_cells_are_lined_up_in_columns
        ; Alcotest.test_case "header" `Quick test_the_header_goes_on_top
        ; Alcotest.test_case
            "spacing"
            `Quick
            test_the_spacing_between_columns_can_change
        ; Alcotest.test_case
            "fill column"
            `Quick
            test_a_fill_column_takes_the_room_that_is_left
        ; Alcotest.test_case
            "cut"
            `Quick
            test_long_cells_are_cut_to_their_column
        ; Alcotest.test_case
            "missing and extra cells"
            `Quick
            test_missing_cells_are_blank_and_extra_cells_are_ignored
        ; Alcotest.test_case "block" `Quick test_a_block_frames_the_table
        ] )
    ; ( "selection"
      , [ Alcotest.test_case
            "symbol and highlight"
            `Quick
            test_the_symbol_and_the_highlight_mark_the_selected_row
        ; Alcotest.test_case
            "header stays"
            `Quick
            test_the_header_stays_while_the_rows_scroll
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest property_drawing_stays_inside_the_area ] )
    ]
;;
