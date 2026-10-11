module Buffer = Camlembert.Buffer
module Constraint = Camlembert.Constraint
module Item_list = Camlembert.Item_list
module Layout = Camlembert.Layout
module Line = Camlembert.Line
module Rect = Camlembert.Rect
module Selection = Camlembert.Selection
module Stateful_widget = Camlembert.Stateful_widget
module Style = Camlembert.Style
module Widget = Camlembert.Widget

let origin = 0
let screen_width = 8
let screen_height = 3
let left_share = 3
let top_share = 1
let rect_testable = Alcotest.testable Rect.pp Rect.equal

let screen =
  Rect.make ~column:origin ~row:origin ~width:screen_width ~height:screen_height
;;

(* A widget that fills its region with a letter. *)
let filling letter : Widget.t =
  fun area buffer ->
  Rect.iter_positions area (fun ~column ~row ->
    Buffer.set_string
      buffer
      ~column
      ~row
      ~max_width:Int.one
      ~style:Style.default
      letter)
;;

let draw widget =
  let buffer = Buffer.create screen in
  widget screen buffer;
  Buffer.to_lines buffer
;;

let test_parts_sit_side_by_side () =
  let widget =
    Widget.split
      ~direction:Layout.Horizontal
      [ Constraint.Length left_share, filling "A"
      ; Constraint.Fill Int.one, filling "B"
      ]
  in
  Alcotest.(check (list string))
    "columns"
    (List.init screen_height (fun _ -> "AAABBBBB"))
    (draw widget)
;;

let test_spacing_leaves_a_gap_between_the_parts () =
  let widget =
    Widget.split
      ~spacing:Int.one
      ~direction:Layout.Horizontal
      [ Constraint.Length left_share, filling "A"
      ; Constraint.Fill Int.one, filling "B"
      ]
  in
  Alcotest.(check (list string))
    "gap"
    (List.init screen_height (fun _ -> "AAA BBBB"))
    (draw widget)
;;

let test_parts_are_stacked () =
  let widget =
    Widget.split
      ~direction:Layout.Vertical
      [ Constraint.Length top_share, filling "A"
      ; Constraint.Fill Int.one, filling "B"
      ]
  in
  Alcotest.(check (list string))
    "rows"
    [ "AAAAAAAA"; "BBBBBBBB"; "BBBBBBBB" ]
    (draw widget)
;;

let test_every_part_receives_its_region () =
  let seen = ref [] in
  let recording : Widget.t = fun area _ -> seen := area :: !seen in
  let constraints = [ Constraint.Length left_share; Constraint.Fill Int.one ] in
  Widget.split
    ~direction:Layout.Horizontal
    (List.map (fun constraint_ -> constraint_, recording) constraints)
    screen
    (Buffer.create screen);
  Alcotest.(check (list rect_testable))
    "regions"
    (Layout.split ~direction:Layout.Horizontal ~constraints screen)
    (List.rev !seen)
;;

let test_no_parts_draw_nothing () =
  Alcotest.(check (list string))
    "blank"
    (List.init screen_height (fun _ -> String.make screen_width ' '))
    (draw (Widget.split ~direction:Layout.Vertical []))
;;

let test_a_state_can_be_given_to_a_stateful_widget () =
  let drawn = ref Int.zero in
  let counting : int ref Stateful_widget.t =
    fun _ _ counter ->
    incr counter;
    incr drawn
  in
  let counter = ref Int.zero in
  let widget = Stateful_widget.with_state counting counter in
  ignore (draw widget);
  ignore (draw widget);
  Alcotest.(check int) "state updated each time" 2 !counter;
  Alcotest.(check int) "drawn twice" 2 !drawn
;;

let test_a_list_with_its_state_can_be_one_part_of_a_split () =
  let selection = Selection.create () in
  Selection.select selection (Some Int.zero);
  let list =
    Item_list.make
      ~highlight_symbol:"> "
      (List.map Line.of_string [ "one"; "two" ])
  in
  let widget =
    Widget.split
      ~direction:Layout.Horizontal
      [ Constraint.Length left_share, filling "#"
      ; ( Constraint.Fill Int.one
        , Stateful_widget.with_state (Item_list.widget list) selection )
      ]
  in
  Alcotest.(check string) "first row" "###> one" (List.hd (draw widget))
;;

let () =
  Alcotest.run
    "Widget"
    [ ( "split"
      , [ Alcotest.test_case "side by side" `Quick test_parts_sit_side_by_side
        ; Alcotest.test_case "stacked" `Quick test_parts_are_stacked
        ; Alcotest.test_case
            "spacing"
            `Quick
            test_spacing_leaves_a_gap_between_the_parts
        ; Alcotest.test_case
            "regions"
            `Quick
            test_every_part_receives_its_region
        ; Alcotest.test_case "no parts" `Quick test_no_parts_draw_nothing
        ] )
    ; ( "state"
      , [ Alcotest.test_case
            "with_state"
            `Quick
            test_a_state_can_be_given_to_a_stateful_widget
        ; Alcotest.test_case
            "list in a split"
            `Quick
            test_a_list_with_its_state_can_be_one_part_of_a_split
        ] )
    ]
;;
