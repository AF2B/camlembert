module Selection = Camlembert.Selection

let item_count = 5
let last_row = 4
let visible_rows = 3
let first_row = 0
let middle_row = 2
let far_below = 40
let far_above = -3
let max_item_count = 20
let max_visible_rows = 8
let max_operation_count = 12

let state_with ?(selected = None) () =
  let selection = Selection.create () in
  Selection.select selection selected;
  selection
;;

let test_a_new_state_has_nothing_selected_and_no_scrolling () =
  let selection = Selection.create () in
  Alcotest.(check (option int)) "selected" None (Selection.selected selection);
  Alcotest.(check int) "offset" Int.zero (Selection.offset selection)
;;

let test_select_stores_the_position () =
  let selection = Selection.create () in
  Selection.select selection (Some middle_row);
  Alcotest.(check (option int))
    "selected"
    (Some middle_row)
    (Selection.selected selection);
  Selection.select selection None;
  Alcotest.(check (option int)) "cleared" None (Selection.selected selection)
;;

let test_select_next_starts_at_the_first_row_and_stops_at_the_last () =
  let selection = Selection.create () in
  Selection.select_next selection ~item_count;
  Alcotest.(check (option int))
    "first"
    (Some first_row)
    (Selection.selected selection);
  Selection.select_next selection ~item_count;
  Alcotest.(check (option int))
    "second"
    (Some (succ first_row))
    (Selection.selected selection);
  Selection.select selection (Some last_row);
  Selection.select_next selection ~item_count;
  Alcotest.(check (option int))
    "stays on the last"
    (Some last_row)
    (Selection.selected selection)
;;

let test_select_previous_starts_at_the_last_row_and_stops_at_the_first () =
  let selection = Selection.create () in
  Selection.select_previous selection ~item_count;
  Alcotest.(check (option int))
    "last"
    (Some last_row)
    (Selection.selected selection);
  Selection.select_previous selection ~item_count;
  Alcotest.(check (option int))
    "before the last"
    (Some (pred last_row))
    (Selection.selected selection);
  Selection.select selection (Some first_row);
  Selection.select_previous selection ~item_count;
  Alcotest.(check (option int))
    "stays on the first"
    (Some first_row)
    (Selection.selected selection)
;;

let test_moving_in_an_empty_list_selects_nothing () =
  let selection = state_with ~selected:(Some middle_row) () in
  Selection.select_next selection ~item_count:Int.zero;
  Alcotest.(check (option int)) "next" None (Selection.selected selection);
  Selection.select_previous selection ~item_count:Int.zero;
  Alcotest.(check (option int)) "previous" None (Selection.selected selection)
;;

let test_reveal_brings_the_selection_into_the_list () =
  let beyond = state_with ~selected:(Some far_below) () in
  Selection.reveal beyond ~item_count ~visible_rows;
  Alcotest.(check (option int))
    "past the end"
    (Some last_row)
    (Selection.selected beyond);
  let before = state_with ~selected:(Some far_above) () in
  Selection.reveal before ~item_count ~visible_rows;
  Alcotest.(check (option int))
    "before the start"
    (Some first_row)
    (Selection.selected before);
  let empty = state_with ~selected:(Some middle_row) () in
  Selection.reveal empty ~item_count:Int.zero ~visible_rows;
  Alcotest.(check (option int)) "empty list" None (Selection.selected empty)
;;

let test_reveal_scrolls_down_to_a_selection_below_the_view () =
  let selection = state_with ~selected:(Some last_row) () in
  Selection.reveal selection ~item_count ~visible_rows;
  Alcotest.(check int)
    "offset"
    (last_row - visible_rows + Int.one)
    (Selection.offset selection)
;;

let test_reveal_scrolls_up_to_a_selection_above_the_view () =
  let selection = state_with ~selected:(Some last_row) () in
  Selection.reveal selection ~item_count ~visible_rows;
  Selection.select selection (Some first_row);
  Selection.reveal selection ~item_count ~visible_rows;
  Alcotest.(check int) "offset" Int.zero (Selection.offset selection)
;;

let test_reveal_keeps_the_scrolling_while_the_selection_stays_in_view () =
  let selection = state_with ~selected:(Some last_row) () in
  Selection.reveal selection ~item_count ~visible_rows;
  let scrolled = Selection.offset selection in
  Selection.select selection (Some (pred last_row));
  Selection.reveal selection ~item_count ~visible_rows;
  Alcotest.(check int) "unchanged" scrolled (Selection.offset selection)
;;

let test_reveal_never_leaves_blank_rows_below_the_list () =
  let selection = state_with ~selected:(Some last_row) () in
  Selection.reveal selection ~item_count ~visible_rows;
  Selection.reveal selection ~item_count:(pred item_count) ~visible_rows;
  Alcotest.(check int)
    "offset follows the shorter list"
    (pred item_count - visible_rows)
    (Selection.offset selection)
;;

let test_views_below_one_row_count_as_one () =
  let selection = state_with ~selected:(Some middle_row) () in
  Selection.reveal selection ~item_count ~visible_rows:Int.zero;
  Alcotest.(check int) "offset" middle_row (Selection.offset selection)
;;

type operation =
  | Next
  | Previous
  | Select of int option
  | Reveal

let print_operation = function
  | Next -> "Next"
  | Previous -> "Previous"
  | Select position ->
    Printf.sprintf "Select %s" (QCheck2.Print.(option int) position)
  | Reveal -> "Reveal"
;;

let operation =
  QCheck2.Gen.(
    oneof
      [ return Next
      ; return Previous
      ; map
          (fun position -> Select position)
          (option (int_range far_above far_below))
      ; return Reveal
      ])
;;

let apply selection ~item_count ~visible_rows = function
  | Next -> Selection.select_next selection ~item_count
  | Previous -> Selection.select_previous selection ~item_count
  | Select position -> Selection.select selection position
  | Reveal -> Selection.reveal selection ~item_count ~visible_rows
;;

let property_a_revealed_selection_is_always_in_the_list_and_in_view =
  QCheck2.Test.make
    ~name:"after a reveal the selection is in the list and in view"
    ~print:QCheck2.Print.(triple int int (list print_operation))
    QCheck2.Gen.(
      triple
        (int_range Int.zero max_item_count)
        (int_range Int.one max_visible_rows)
        (list_size (int_range Int.zero max_operation_count) operation))
    (fun (item_count, visible_rows, operations) ->
       let selection = Selection.create () in
       List.iter (apply selection ~item_count ~visible_rows) operations;
       Selection.reveal selection ~item_count ~visible_rows;
       let offset = Selection.offset selection in
       let highest_offset = Int.max Int.zero (item_count - visible_rows) in
       let offset_is_valid = offset >= Int.zero && offset <= highest_offset in
       let selection_is_valid =
         match Selection.selected selection with
         | None -> true
         | Some position ->
           position >= Int.zero
           && position < item_count
           && position >= offset
           && position < offset + visible_rows
       in
       offset_is_valid && selection_is_valid)
;;

let () =
  Alcotest.run
    "Selection"
    [ ( "state"
      , [ Alcotest.test_case
            "new state"
            `Quick
            test_a_new_state_has_nothing_selected_and_no_scrolling
        ; Alcotest.test_case "select" `Quick test_select_stores_the_position
        ] )
    ; ( "moves"
      , [ Alcotest.test_case
            "next"
            `Quick
            test_select_next_starts_at_the_first_row_and_stops_at_the_last
        ; Alcotest.test_case
            "previous"
            `Quick
            test_select_previous_starts_at_the_last_row_and_stops_at_the_first
        ; Alcotest.test_case
            "empty list"
            `Quick
            test_moving_in_an_empty_list_selects_nothing
        ] )
    ; ( "reveal"
      , [ Alcotest.test_case
            "into the list"
            `Quick
            test_reveal_brings_the_selection_into_the_list
        ; Alcotest.test_case
            "scroll down"
            `Quick
            test_reveal_scrolls_down_to_a_selection_below_the_view
        ; Alcotest.test_case
            "scroll up"
            `Quick
            test_reveal_scrolls_up_to_a_selection_above_the_view
        ; Alcotest.test_case
            "keeps scrolling"
            `Quick
            test_reveal_keeps_the_scrolling_while_the_selection_stays_in_view
        ; Alcotest.test_case
            "no blank rows"
            `Quick
            test_reveal_never_leaves_blank_rows_below_the_list
        ; Alcotest.test_case
            "tiny views"
            `Quick
            test_views_below_one_row_count_as_one
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest
            property_a_revealed_selection_is_always_in_the_list_and_in_view
        ] )
    ]
;;
