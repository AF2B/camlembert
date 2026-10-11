module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Rect = Camlembert.Rect
module Style = Camlembert.Style

let origin = 0
let screen_width = 6
let screen_height = 2
let second_row = 1
let second_column = 1
let narrow_limit = 2
let late_column = 4
let narrow_screen_width = 3
let blank_character = ' '
let blank_row = String.make screen_width blank_character

let screen =
  Rect.make ~column:origin ~row:origin ~width:screen_width ~height:screen_height
;;

let single_cell =
  Rect.make ~column:origin ~row:origin ~width:Int.one ~height:Int.one
;;

let oversized_region =
  Rect.make
    ~column:origin
    ~row:origin
    ~width:(2 * screen_width)
    ~height:(2 * screen_height)
;;

let blue_background = Style.make ~background:Color.Blue ()
let red_foreground = Style.make ~foreground:Color.Red ()

let change_testable =
  Alcotest.testable
    (fun formatter (change : Buffer.change) ->
       Format.fprintf
         formatter
         "(%d, %d, %a)"
         change.Buffer.column
         change.Buffer.row
         Cell.pp
         change.Buffer.cell)
    (fun (first : Buffer.change) (second : Buffer.change) ->
       first.Buffer.column = second.Buffer.column
       && first.Buffer.row = second.Buffer.row
       && Cell.equal first.Buffer.cell second.Buffer.cell)
;;

let change ~column ~row symbol : Buffer.change =
  { column; row; cell = Cell.make symbol }
;;

let write buffer ~column ~row text =
  Buffer.set_string
    buffer
    ~column
    ~row
    ~max_width:screen_width
    ~style:Style.default
    text
;;

(* Builds a buffer from rows of text, one row per list element. *)
let buffer_of_lines lines =
  let buffer = Buffer.create screen in
  List.iteri (fun row line -> write buffer ~column:origin ~row line) lines;
  buffer
;;

let first_line buffer = List.hd (Buffer.to_lines buffer)

let test_new_buffer_is_blank () =
  Alcotest.(check (list string))
    "blank rows"
    [ blank_row; blank_row ]
    (Buffer.to_lines (Buffer.create screen))
;;

let test_set_string_writes_at_the_position () =
  let buffer = Buffer.create screen in
  write buffer ~column:second_column ~row:origin "hi";
  Alcotest.(check string) "first row" " hi   " (first_line buffer)
;;

let test_set_string_stops_at_max_width () =
  let buffer = Buffer.create screen in
  Buffer.set_string
    buffer
    ~column:origin
    ~row:origin
    ~max_width:narrow_limit
    ~style:Style.default
    "abcdef";
  Alcotest.(check string) "clipped" "ab    " (first_line buffer)
;;

let test_set_string_stops_at_the_buffer_edge () =
  let buffer = Buffer.create screen in
  write buffer ~column:late_column ~row:origin "abcdef";
  Alcotest.(check string) "clipped" "    ab" (first_line buffer)
;;

let test_set_string_ignores_rows_outside_the_buffer () =
  let buffer = Buffer.create screen in
  write buffer ~column:origin ~row:screen_height "abc";
  Alcotest.(check (list string))
    "untouched"
    [ blank_row; blank_row ]
    (Buffer.to_lines buffer)
;;

let test_wide_characters_cover_two_cells () =
  let buffer = Buffer.create screen in
  write buffer ~column:origin ~row:origin "\u{6F22}\u{5B57}";
  Alcotest.(check string) "row text" "\u{6F22}\u{5B57}  " (first_line buffer);
  Alcotest.(check bool)
    "covered cell is blank"
    true
    (Cell.equal
       Cell.empty
       (Buffer.get buffer ~column:second_column ~row:origin))
;;

let test_wide_character_that_does_not_fit_is_not_written () =
  let narrow =
    Rect.make
      ~column:origin
      ~row:origin
      ~width:narrow_screen_width
      ~height:Int.one
  in
  let buffer = Buffer.create narrow in
  Buffer.set_string
    buffer
    ~column:origin
    ~row:origin
    ~max_width:narrow_screen_width
    ~style:Style.default
    "ab\u{6F22}";
  Alcotest.(check (list string))
    "no half character"
    [ "ab " ]
    (Buffer.to_lines buffer)
;;

let test_control_characters_are_skipped () =
  let buffer = Buffer.create screen in
  write buffer ~column:origin ~row:origin "a\nb";
  Alcotest.(check string) "skipped" "ab    " (first_line buffer)
;;

let test_set_string_layers_the_style_over_existing_cells () =
  let buffer = Buffer.create screen in
  Buffer.set_style buffer screen blue_background;
  Buffer.set_string
    buffer
    ~column:origin
    ~row:origin
    ~max_width:screen_width
    ~style:red_foreground
    "x";
  let expected = Style.patch blue_background red_foreground in
  Alcotest.(check bool)
    "both colors"
    true
    (Style.equal
       expected
       (Buffer.get buffer ~column:origin ~row:origin).Cell.style)
;;

let test_set_style_only_touches_the_region () =
  let buffer = Buffer.create screen in
  Buffer.set_style buffer single_cell blue_background;
  let styled = Buffer.get buffer ~column:origin ~row:origin in
  let untouched = Buffer.get buffer ~column:second_column ~row:origin in
  Alcotest.(check bool)
    "inside"
    true
    (Style.equal blue_background styled.Cell.style);
  Alcotest.(check bool) "outside" true (Cell.equal Cell.empty untouched)
;;

let test_set_style_ignores_the_part_outside_the_buffer () =
  let buffer = Buffer.create screen in
  Buffer.set_style buffer oversized_region blue_background;
  let last_cell =
    Buffer.get buffer ~column:(pred screen_width) ~row:(pred screen_height)
  in
  Alcotest.(check bool)
    "last cell"
    true
    (Style.equal blue_background last_cell.Cell.style)
;;

let test_reset_blanks_every_cell () =
  let buffer = buffer_of_lines [ "abc"; "def" ] in
  Buffer.reset buffer;
  Alcotest.(check (list string))
    "blank rows"
    [ blank_row; blank_row ]
    (Buffer.to_lines buffer)
;;

let test_get_outside_the_buffer_raises () =
  Alcotest.check_raises
    "outside"
    (Invalid_argument "Buffer.get: position outside of the buffer area")
    (fun () ->
       ignore
         (Buffer.get (Buffer.create screen) ~column:screen_width ~row:origin))
;;

let test_diff_of_identical_buffers_is_empty () =
  let lines = [ "abc"; "def" ] in
  Alcotest.(check (list change_testable))
    "no changes"
    []
    (Buffer.diff
       ~previous:(buffer_of_lines lines)
       ~next:(buffer_of_lines lines))
;;

let test_diff_reports_changed_cells_row_by_row () =
  let previous = buffer_of_lines [ "abc"; "def" ] in
  let next = buffer_of_lines [ "aXc"; "dYf" ] in
  Alcotest.(check (list change_testable))
    "changes"
    [ change ~column:second_column ~row:origin "X"
    ; change ~column:second_column ~row:second_row "Y"
    ]
    (Buffer.diff ~previous ~next)
;;

let test_diff_skips_cells_hidden_by_a_wide_character () =
  let previous = buffer_of_lines [ "ab" ] in
  let next = buffer_of_lines [ "\u{6F22}" ] in
  Alcotest.(check (list change_testable))
    "single change"
    [ change ~column:origin ~row:origin "\u{6F22}" ]
    (Buffer.diff ~previous ~next)
;;

let test_diff_redraws_cells_freed_by_a_vanished_wide_character () =
  let previous = buffer_of_lines [ "\u{6F22}" ] in
  let next = buffer_of_lines [ "a " ] in
  Alcotest.(check (list change_testable))
    "both cells"
    [ change ~column:origin ~row:origin "a"
    ; change ~column:second_column ~row:origin " "
    ]
    (Buffer.diff ~previous ~next)
;;

let test_diff_rejects_buffers_of_different_areas () =
  let small =
    Rect.make ~column:origin ~row:origin ~width:Int.one ~height:Int.one
  in
  Alcotest.check_raises
    "different areas"
    (Invalid_argument "Buffer.diff: buffers cover different areas")
    (fun () ->
       ignore
         (Buffer.diff
            ~previous:(Buffer.create small)
            ~next:(Buffer.create screen)))
;;

let test_apply_shows_wide_characters_like_a_terminal () =
  let buffer = buffer_of_lines [ "ab" ] in
  Buffer.apply buffer [ change ~column:origin ~row:origin "\u{6F22}" ];
  Alcotest.(check string) "wide character" "\u{6F22}    " (first_line buffer)
;;

let test_apply_rejects_changes_outside_the_buffer () =
  Alcotest.check_raises
    "outside"
    (Invalid_argument "Buffer.apply: change outside of the buffer area")
    (fun () ->
       Buffer.apply
         (Buffer.create screen)
         [ change ~column:screen_width ~row:origin "x" ])
;;

let build_buffer edits =
  let buffer = Buffer.create Generators.screen in
  List.iter
    (fun { Generators.column; row; symbol; style } ->
       Buffer.set_string
         buffer
         ~column
         ~row
         ~max_width:Generators.screen_width
         ~style
         symbol)
    edits;
  buffer
;;

let same_cells first second =
  let same = ref true in
  Rect.iter_positions Generators.screen (fun ~column ~row ->
    let first_cell = Buffer.get first ~column ~row in
    let second_cell = Buffer.get second ~column ~row in
    if not (Cell.equal first_cell second_cell) then same := false);
  !same
;;

let rec strictly_increasing = function
  | first :: (second :: _ as rest) ->
    compare first second < 0 && strictly_increasing rest
  | _ -> true
;;

let property_applying_the_diff_gives_the_next_frame =
  QCheck2.Test.make
    ~name:"applying the diff to the previous frame gives the next frame"
    ~print:QCheck2.Print.(pair Generators.print_edits Generators.print_edits)
    QCheck2.Gen.(pair Generators.edits Generators.edits)
    (fun (previous_edits, next_edits) ->
       let screen_now = build_buffer previous_edits in
       let changes =
         Buffer.diff
           ~previous:(build_buffer previous_edits)
           ~next:(build_buffer next_edits)
       in
       Buffer.apply screen_now changes;
       same_cells screen_now (build_buffer next_edits))
;;

let property_identical_frames_have_no_diff =
  QCheck2.Test.make
    ~name:"identical frames have an empty diff"
    ~print:Generators.print_edits
    Generators.edits
    (fun edits ->
       Buffer.diff ~previous:(build_buffer edits) ~next:(build_buffer edits)
       = [])
;;

let property_changes_come_row_by_row =
  QCheck2.Test.make
    ~name:"changes come in row-major order"
    ~print:QCheck2.Print.(pair Generators.print_edits Generators.print_edits)
    QCheck2.Gen.(pair Generators.edits Generators.edits)
    (fun (previous_edits, next_edits) ->
       Buffer.diff
         ~previous:(build_buffer previous_edits)
         ~next:(build_buffer next_edits)
       |> List.map (fun (change : Buffer.change) ->
         change.Buffer.row, change.Buffer.column)
       |> strictly_increasing)
;;

let () =
  Alcotest.run
    "Buffer"
    [ ( "create"
      , [ Alcotest.test_case "blank" `Quick test_new_buffer_is_blank
        ; Alcotest.test_case
            "get outside"
            `Quick
            test_get_outside_the_buffer_raises
        ; Alcotest.test_case "reset" `Quick test_reset_blanks_every_cell
        ] )
    ; ( "set_string"
      , [ Alcotest.test_case
            "writes at the position"
            `Quick
            test_set_string_writes_at_the_position
        ; Alcotest.test_case
            "max width"
            `Quick
            test_set_string_stops_at_max_width
        ; Alcotest.test_case
            "buffer edge"
            `Quick
            test_set_string_stops_at_the_buffer_edge
        ; Alcotest.test_case
            "row outside"
            `Quick
            test_set_string_ignores_rows_outside_the_buffer
        ; Alcotest.test_case
            "wide characters"
            `Quick
            test_wide_characters_cover_two_cells
        ; Alcotest.test_case
            "wide character that does not fit"
            `Quick
            test_wide_character_that_does_not_fit_is_not_written
        ; Alcotest.test_case
            "control characters"
            `Quick
            test_control_characters_are_skipped
        ; Alcotest.test_case
            "layers styles"
            `Quick
            test_set_string_layers_the_style_over_existing_cells
        ] )
    ; ( "set_style"
      , [ Alcotest.test_case
            "only the region"
            `Quick
            test_set_style_only_touches_the_region
        ; Alcotest.test_case
            "ignores what is outside"
            `Quick
            test_set_style_ignores_the_part_outside_the_buffer
        ] )
    ; ( "diff"
      , [ Alcotest.test_case
            "identical buffers"
            `Quick
            test_diff_of_identical_buffers_is_empty
        ; Alcotest.test_case
            "row by row"
            `Quick
            test_diff_reports_changed_cells_row_by_row
        ; Alcotest.test_case
            "hidden by a wide character"
            `Quick
            test_diff_skips_cells_hidden_by_a_wide_character
        ; Alcotest.test_case
            "freed by a wide character"
            `Quick
            test_diff_redraws_cells_freed_by_a_vanished_wide_character
        ; Alcotest.test_case
            "different areas"
            `Quick
            test_diff_rejects_buffers_of_different_areas
        ] )
    ; ( "apply"
      , [ Alcotest.test_case
            "wide characters"
            `Quick
            test_apply_shows_wide_characters_like_a_terminal
        ; Alcotest.test_case
            "outside the buffer"
            `Quick
            test_apply_rejects_changes_outside_the_buffer
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_applying_the_diff_gives_the_next_frame
          ; property_identical_frames_have_no_diff
          ; property_changes_come_row_by_row
          ] )
    ]
;;
