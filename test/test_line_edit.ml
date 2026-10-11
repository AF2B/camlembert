module Event = Camlembert.Event
module Grapheme = Camlembert.Grapheme
module Line_edit = Camlembert.Line_edit

let greeting = "hello"
let wide_text = "\u{6F22}\u{5B57}"
let accented_letter = "e\u{0301}"
let narrow_field = 5
let wide_field = 3
let long_text = "abcdefgh"
let scrolled_view = "efgh"
let first_view = "abcde"
let max_field_width = 10
let origin = 0
let two_ideographs = 2
let two_characters = 2
let columns_of_an_ideograph = 2
let moved_back = 2
let inserted = "XY"
let replacement = "bye"
let terminal_columns = 80
let terminal_rows = 24
let kept_before = String.length greeting - moved_back
let text_before_cursor = String.sub greeting Int.zero kept_before
let text_after_cursor = String.sub greeting kept_before moved_back
let max_operation_count = 15
let held_control = { Event.no_modifiers with control = true }
let held_alt = { Event.no_modifiers with alt = true }
let typed text = Event.key_press (Event.Char text)
let pressed key = Event.key_press key

let with_control letter =
  Event.key_press ~modifiers:held_control (Event.Char letter)
;;

let edit_with ?(cursor_at = None) text =
  let edit = Line_edit.create ~text () in
  Option.iter
    (fun position ->
       Line_edit.move_to_start edit;
       List.iter
         (fun () -> Line_edit.move_right edit)
         (List.init position (Fun.const ())))
    cursor_at;
  edit
;;

let test_a_new_line_has_its_cursor_at_the_end () =
  let edit = Line_edit.create ~text:greeting () in
  Alcotest.(check string) "text" greeting (Line_edit.text edit);
  Alcotest.(check int) "cursor" (String.length greeting) (Line_edit.cursor edit);
  Alcotest.(check string)
    "empty by default"
    ""
    (Line_edit.text (Line_edit.create ()))
;;

let test_insert_writes_at_the_cursor () =
  let edit = Line_edit.create ~text:greeting () in
  List.iter
    (fun () -> Line_edit.move_left edit)
    (List.init moved_back (Fun.const ()));
  Line_edit.insert edit inserted;
  Alcotest.(check string)
    "text"
    (text_before_cursor ^ inserted ^ text_after_cursor)
    (Line_edit.text edit);
  Alcotest.(check int)
    "cursor after the new text"
    (kept_before + String.length inserted)
    (Line_edit.cursor edit)
;;

let test_characters_that_take_no_column_are_dropped () =
  let edit = Line_edit.create () in
  Line_edit.insert edit "a\nb\tc";
  Alcotest.(check string) "text" "abc" (Line_edit.text edit)
;;

let test_what_the_user_sees_as_one_character_is_one_position () =
  let edit = Line_edit.create ~text:wide_text () in
  Alcotest.(check int) "two ideographs" two_ideographs (Line_edit.cursor edit);
  Line_edit.delete_before edit;
  Alcotest.(check string) "one removed" "\u{6F22}" (Line_edit.text edit);
  let accented = Line_edit.create ~text:(accented_letter ^ "x") () in
  Alcotest.(check int)
    "letter with its accent, and x"
    2
    (Line_edit.cursor accented);
  Line_edit.move_left accented;
  Line_edit.delete_before accented;
  Alcotest.(check string)
    "accented letter removed whole"
    "x"
    (Line_edit.text accented)
;;

let test_deleting_at_the_edges_does_nothing () =
  let edit = Line_edit.create () in
  Line_edit.delete_before edit;
  Line_edit.delete_after edit;
  Alcotest.(check string) "empty stays empty" "" (Line_edit.text edit);
  let edit = Line_edit.create ~text:greeting () in
  Line_edit.delete_after edit;
  Alcotest.(check string)
    "nothing after the cursor"
    greeting
    (Line_edit.text edit);
  Line_edit.move_to_start edit;
  Line_edit.delete_before edit;
  Alcotest.(check string)
    "nothing before the cursor"
    greeting
    (Line_edit.text edit)
;;

let test_deletes_work_on_either_side_of_the_cursor () =
  let edit = edit_with ~cursor_at:(Some two_characters) greeting in
  Line_edit.delete_after edit;
  Alcotest.(check string) "after" "helo" (Line_edit.text edit);
  Line_edit.delete_before edit;
  Alcotest.(check string) "before" "hlo" (Line_edit.text edit);
  Line_edit.delete_to_end edit;
  Alcotest.(check string) "to the end" "h" (Line_edit.text edit);
  Line_edit.insert edit "ey";
  Line_edit.move_left edit;
  Line_edit.delete_to_start edit;
  Alcotest.(check string) "to the start" "y" (Line_edit.text edit)
;;

let test_the_cursor_stops_at_both_ends () =
  let edit = Line_edit.create ~text:greeting () in
  Line_edit.move_right edit;
  Alcotest.(check int) "end" (String.length greeting) (Line_edit.cursor edit);
  Line_edit.move_to_start edit;
  Line_edit.move_left edit;
  Alcotest.(check int) "start" Int.zero (Line_edit.cursor edit);
  Line_edit.move_to_end edit;
  Alcotest.(check int)
    "moved to the end"
    (String.length greeting)
    (Line_edit.cursor edit)
;;

let test_set_text_and_clear () =
  let edit = Line_edit.create ~text:greeting () in
  Line_edit.set_text edit replacement;
  Alcotest.(check (pair string int))
    "replaced"
    (replacement, String.length replacement)
    (Line_edit.text edit, Line_edit.cursor edit);
  Line_edit.clear edit;
  Alcotest.(check (pair string int))
    "cleared"
    ("", Int.zero)
    (Line_edit.text edit, Line_edit.cursor edit)
;;

let test_events_that_edit_are_applied () =
  let edit = Line_edit.create () in
  let apply event =
    Alcotest.(check bool) "consumed" true (Line_edit.handle edit event)
  in
  apply (typed "a");
  apply (typed "b");
  apply (typed "c");
  apply (pressed Event.Left);
  apply (typed "X");
  Alcotest.(check string) "typing in the middle" "abXc" (Line_edit.text edit);
  apply (pressed Event.Backspace);
  Alcotest.(check string) "backspace" "abc" (Line_edit.text edit);
  apply (pressed Event.Home);
  apply (pressed Event.Delete);
  Alcotest.(check string) "delete" "bc" (Line_edit.text edit);
  apply (pressed Event.End);
  apply (pressed Event.Right);
  apply (Event.Paste "def");
  Alcotest.(check string) "paste" "bcdef" (Line_edit.text edit)
;;

let test_readline_shortcuts () =
  let edit = Line_edit.create ~text:greeting () in
  let apply letter =
    Alcotest.(check bool)
      "consumed"
      true
      (Line_edit.handle edit (with_control letter))
  in
  apply "a";
  Alcotest.(check int) "ctrl+a" Int.zero (Line_edit.cursor edit);
  apply "e";
  Alcotest.(check int) "ctrl+e" (String.length greeting) (Line_edit.cursor edit);
  apply "b";
  apply "h";
  Alcotest.(check string) "ctrl+b then ctrl+h" "helo" (Line_edit.text edit);
  apply "a";
  apply "f";
  apply "d";
  Alcotest.(check string) "ctrl+f then ctrl+d" "hlo" (Line_edit.text edit);
  apply "k";
  Alcotest.(check string) "ctrl+k" "h" (Line_edit.text edit);
  apply "u";
  Alcotest.(check string) "ctrl+u" "" (Line_edit.text edit)
;;

let test_other_events_are_left_to_the_application () =
  let edit = Line_edit.create ~text:greeting () in
  let ignored =
    [ pressed Event.Enter
    ; pressed Event.Tab
    ; pressed Event.Escape
    ; pressed Event.Up
    ; pressed Event.Down
    ; pressed (Event.Function Int.one)
    ; Event.key_press ~modifiers:held_alt (Event.Char "x")
    ; with_control "c"
    ; Event.Resize { width = terminal_columns; height = terminal_rows }
    ; Event.Mouse
        { action = Event.Scroll_up
        ; position = { Camlembert.Position.column = origin; row = origin }
        ; modifiers = Event.no_modifiers
        }
    ]
  in
  List.iter
    (fun event ->
       Alcotest.(check bool) "not consumed" false (Line_edit.handle edit event))
    ignored;
  Alcotest.(check string) "text untouched" greeting (Line_edit.text edit)
;;

let test_the_view_follows_the_cursor () =
  let edit = Line_edit.create ~text:long_text () in
  Line_edit.reveal edit ~width:narrow_field;
  Alcotest.(check (pair string int))
    "at the end"
    (scrolled_view, String.length scrolled_view)
    (Line_edit.visible edit ~width:narrow_field, Line_edit.cursor_column edit);
  Line_edit.move_left edit;
  Line_edit.move_left edit;
  Line_edit.reveal edit ~width:narrow_field;
  Alcotest.(check (pair string int))
    "the view stays while the cursor is in it"
    (scrolled_view, moved_back)
    (Line_edit.visible edit ~width:narrow_field, Line_edit.cursor_column edit);
  Line_edit.move_to_start edit;
  Line_edit.reveal edit ~width:narrow_field;
  Alcotest.(check (pair string int))
    "back at the start"
    (first_view, Int.zero)
    (Line_edit.visible edit ~width:narrow_field, Line_edit.cursor_column edit)
;;

let test_wide_characters_are_never_cut_by_the_view () =
  let edit = Line_edit.create ~text:(wide_text ^ "\u{6F22}") () in
  Line_edit.reveal edit ~width:wide_field;
  Alcotest.(check (pair string int))
    "last ideograph only"
    ("\u{6F22}", columns_of_an_ideograph)
    (Line_edit.visible edit ~width:wide_field, Line_edit.cursor_column edit)
;;

type operation =
  | Insert of string
  | Delete_before
  | Delete_after
  | Left
  | Right
  | Start
  | End
  | Kill_to_start
  | Kill_to_end
  | Set_text of string

let print_operation = function
  | Insert text -> Printf.sprintf "Insert %S" text
  | Set_text text -> Printf.sprintf "Set_text %S" text
  | Delete_before -> "Delete_before"
  | Delete_after -> "Delete_after"
  | Left -> "Left"
  | Right -> "Right"
  | Start -> "Start"
  | End -> "End"
  | Kill_to_start -> "Kill_to_start"
  | Kill_to_end -> "Kill_to_end"
;;

let alphabet = [ "a"; "b"; " "; "\u{6F22}"; "\u{E9}" ]
let max_inserted_length = 4

let some_text =
  QCheck2.Gen.(
    let+ pieces =
      list_size (int_range Int.zero max_inserted_length) (oneof_list alphabet)
    in
    String.concat "" pieces)
;;

let operation =
  QCheck2.Gen.(
    oneof
      [ map (fun text -> Insert text) some_text
      ; return Delete_before
      ; return Delete_after
      ; return Left
      ; return Right
      ; return Start
      ; return End
      ; return Kill_to_start
      ; return Kill_to_end
      ; map (fun text -> Set_text text) some_text
      ])
;;

let apply edit = function
  | Insert text -> Line_edit.insert edit text
  | Delete_before -> Line_edit.delete_before edit
  | Delete_after -> Line_edit.delete_after edit
  | Left -> Line_edit.move_left edit
  | Right -> Line_edit.move_right edit
  | Start -> Line_edit.move_to_start edit
  | End -> Line_edit.move_to_end edit
  | Kill_to_start -> Line_edit.delete_to_start edit
  | Kill_to_end -> Line_edit.delete_to_end edit
  | Set_text text -> Line_edit.set_text edit text
;;

let operations =
  QCheck2.Gen.(
    pair
      (list_size (int_range Int.zero max_operation_count) operation)
      (int_range Int.one max_field_width))
;;

let print_operations = QCheck2.Print.(pair (list print_operation) int)

let edited_with operations =
  let edit = Line_edit.create () in
  List.iter (apply edit) operations;
  edit
;;

let property_the_cursor_stays_inside_the_text =
  QCheck2.Test.make
    ~name:"the cursor stays inside the text"
    ~print:print_operations
    operations
    (fun (operations, _) ->
       let edit = edited_with operations in
       let length = List.length (Grapheme.clusters (Line_edit.text edit)) in
       Line_edit.cursor edit >= Int.zero && Line_edit.cursor edit <= length)
;;

let property_the_cursor_fits_in_the_field_and_so_does_the_view =
  QCheck2.Test.make
    ~name:"after a reveal the cursor and the view fit the field"
    ~print:print_operations
    operations
    (fun (operations, width) ->
       let edit = edited_with operations in
       Line_edit.reveal edit ~width;
       Line_edit.cursor_column edit < width
       && Grapheme.width (Line_edit.visible edit ~width) <= width)
;;

let property_typing_then_deleting_changes_nothing =
  QCheck2.Test.make
    ~name:"typing a character and deleting it changes nothing"
    ~print:QCheck2.Print.(pair (list print_operation) string)
    QCheck2.Gen.(
      pair
        (list_size (int_range Int.zero max_operation_count) operation)
        (oneof_list alphabet))
    (fun (operations, typed_character) ->
       let edit = edited_with operations in
       let before = Line_edit.text edit in
       Line_edit.insert edit typed_character;
       Line_edit.delete_before edit;
       Line_edit.text edit = before)
;;

let () =
  Alcotest.run
    "Line_edit"
    [ ( "editing"
      , [ Alcotest.test_case
            "new line"
            `Quick
            test_a_new_line_has_its_cursor_at_the_end
        ; Alcotest.test_case "insert" `Quick test_insert_writes_at_the_cursor
        ; Alcotest.test_case
            "no-column characters"
            `Quick
            test_characters_that_take_no_column_are_dropped
        ; Alcotest.test_case
            "characters"
            `Quick
            test_what_the_user_sees_as_one_character_is_one_position
        ; Alcotest.test_case
            "deleting at the edges"
            `Quick
            test_deleting_at_the_edges_does_nothing
        ; Alcotest.test_case
            "deleting"
            `Quick
            test_deletes_work_on_either_side_of_the_cursor
        ; Alcotest.test_case "moving" `Quick test_the_cursor_stops_at_both_ends
        ; Alcotest.test_case "set_text and clear" `Quick test_set_text_and_clear
        ] )
    ; ( "events"
      , [ Alcotest.test_case
            "editing events"
            `Quick
            test_events_that_edit_are_applied
        ; Alcotest.test_case "readline shortcuts" `Quick test_readline_shortcuts
        ; Alcotest.test_case
            "other events"
            `Quick
            test_other_events_are_left_to_the_application
        ] )
    ; ( "view"
      , [ Alcotest.test_case
            "follows the cursor"
            `Quick
            test_the_view_follows_the_cursor
        ; Alcotest.test_case
            "wide characters"
            `Quick
            test_wide_characters_are_never_cut_by_the_view
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_the_cursor_stays_inside_the_text
          ; property_the_cursor_fits_in_the_field_and_so_does_the_view
          ; property_typing_then_deleting_changes_nothing
          ] )
    ]
;;
