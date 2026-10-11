module Event = Camlembert.Event
module Input_parser = Camlembert.Input_parser
module Position = Camlembert.Position

let escape = "\027"
let event_testable = Alcotest.testable Event.pp Event.equal
let held_control = { Event.no_modifiers with control = true }
let held_alt = { Event.no_modifiers with alt = true }
let held_shift = { Event.no_modifiers with shift = true }
let key ?modifiers key_value = Event.key_press ?modifiers key_value
let char ?modifiers text = key ?modifiers (Event.Char text)
let paste text = escape ^ "[200~" ^ text ^ escape ^ "[201~"

(* A mouse report in the extended format; terminals count from one. *)
let report ?(released = false) ~code ~column ~row () =
  Printf.sprintf
    "%s[<%d;%d;%d%c"
    escape
    code
    column
    row
    (if released then 'm' else 'M')
;;

let mouse ?(modifiers = Event.no_modifiers) action ~column ~row =
  Event.Mouse { action; position = { Position.column; row }; modifiers }
;;

(* Every encoding below is complete on its own and cannot be mistaken for the
   start of another one. Each is paired with the event it stands for. *)
let encodings =
  [ "a", char "a"
  ; "Z", char "Z"
  ; "7", char "7"
  ; "?", char "?"
  ; " ", char " "
  ; "\u{E9}", char "\u{E9}"
  ; "\u{6F22}", char "\u{6F22}"
  ; "\u{1F600}", char "\u{1F600}"
  ; "\r", key Event.Enter
  ; "\n", key Event.Enter
  ; "\t", key Event.Tab
  ; "\127", key Event.Backspace
  ; "\b", key Event.Backspace
  ; "\001", char ~modifiers:held_control "a"
  ; "\003", char ~modifiers:held_control "c"
  ; "\026", char ~modifiers:held_control "z"
  ; "\000", char ~modifiers:held_control " "
  ; "\028", char ~modifiers:held_control "\\"
  ; escape ^ "a", char ~modifiers:held_alt "a"
  ; escape ^ "\r", key ~modifiers:held_alt Event.Enter
  ; escape ^ "\u{E9}", char ~modifiers:held_alt "\u{E9}"
  ; escape ^ "[A", key Event.Up
  ; escape ^ "[B", key Event.Down
  ; escape ^ "[C", key Event.Right
  ; escape ^ "[D", key Event.Left
  ; escape ^ "[H", key Event.Home
  ; escape ^ "[1~", key Event.Home
  ; escape ^ "[7~", key Event.Home
  ; escape ^ "OH", key Event.Home
  ; escape ^ "[F", key Event.End
  ; escape ^ "[4~", key Event.End
  ; escape ^ "[8~", key Event.End
  ; escape ^ "OF", key Event.End
  ; escape ^ "[2~", key Event.Insert
  ; escape ^ "[3~", key Event.Delete
  ; escape ^ "[5~", key Event.Page_up
  ; escape ^ "[6~", key Event.Page_down
  ; escape ^ "OA", key Event.Up
  ; escape ^ "[Z", key Event.Backtab
  ; escape ^ "[1;5A", key ~modifiers:held_control Event.Up
  ; escape ^ "[1;2C", key ~modifiers:held_shift Event.Right
  ; escape ^ "[1;3D", key ~modifiers:held_alt Event.Left
  ; ( escape ^ "[3;8~"
    , key
        ~modifiers:{ Event.shift = true; control = true; alt = true }
        Event.Delete )
  ; escape ^ "OP", key (Event.Function 1)
  ; escape ^ "OQ", key (Event.Function 2)
  ; escape ^ "OR", key (Event.Function 3)
  ; escape ^ "OS", key (Event.Function 4)
  ; escape ^ "[15~", key (Event.Function 5)
  ; escape ^ "[17~", key (Event.Function 6)
  ; escape ^ "[21~", key (Event.Function 10)
  ; escape ^ "[23~", key (Event.Function 11)
  ; escape ^ "[24~", key (Event.Function 12)
  ; escape ^ "[15;5~", key ~modifiers:held_control (Event.Function 5)
  ; ( report ~code:0 ~column:10 ~row:5 ()
    , mouse (Event.Press Event.Left_button) ~column:9 ~row:4 )
  ; ( report ~released:true ~code:0 ~column:10 ~row:5 ()
    , mouse (Event.Release Event.Left_button) ~column:9 ~row:4 )
  ; ( report ~code:1 ~column:3 ~row:4 ()
    , mouse (Event.Press Event.Middle_button) ~column:2 ~row:3 )
  ; ( report ~code:2 ~column:3 ~row:4 ()
    , mouse (Event.Press Event.Right_button) ~column:2 ~row:3 )
  ; ( report ~code:32 ~column:11 ~row:5 ()
    , mouse (Event.Drag Event.Left_button) ~column:10 ~row:4 )
  ; ( report ~code:34 ~column:11 ~row:5 ()
    , mouse (Event.Drag Event.Right_button) ~column:10 ~row:4 )
  ; report ~code:64 ~column:1 ~row:1 (), mouse Event.Scroll_up ~column:0 ~row:0
  ; ( report ~code:65 ~column:1 ~row:1 ()
    , mouse Event.Scroll_down ~column:0 ~row:0 )
  ; ( report ~code:66 ~column:1 ~row:1 ()
    , mouse Event.Scroll_left ~column:0 ~row:0 )
  ; ( report ~code:67 ~column:1 ~row:1 ()
    , mouse Event.Scroll_right ~column:0 ~row:0 )
  ; ( report ~code:4 ~column:1 ~row:1 ()
    , mouse
        (Event.Press Event.Left_button)
        ~modifiers:held_shift
        ~column:0
        ~row:0 )
  ; ( report ~code:8 ~column:1 ~row:1 ()
    , mouse (Event.Press Event.Left_button) ~modifiers:held_alt ~column:0 ~row:0
    )
  ; ( report ~code:16 ~column:1 ~row:1 ()
    , mouse
        (Event.Press Event.Left_button)
        ~modifiers:held_control
        ~column:0
        ~row:0 )
  ; ( report ~code:28 ~column:1 ~row:1 ()
    , mouse
        (Event.Press Event.Left_button)
        ~modifiers:{ Event.shift = true; control = true; alt = true }
        ~column:0
        ~row:0 )
  ; ( report ~code:0 ~column:300 ~row:120 ()
    , mouse (Event.Press Event.Left_button) ~column:299 ~row:119 )
  ; paste "hello", Event.Paste "hello"
  ; paste "", Event.Paste ""
  ; ( paste ("one\ntwo " ^ escape ^ "[A is not a key")
    , Event.Paste ("one\ntwo " ^ escape ^ "[A is not a key") )
  ]
;;

let events_of_chunks chunks =
  let parser, events =
    List.fold_left
      (fun (parser, events) chunk ->
         let parser, new_events = Input_parser.feed parser chunk in
         parser, events @ new_events)
      (Input_parser.empty, [])
      chunks
  in
  let _, flushed = Input_parser.flush parser in
  events @ flushed
;;

let events_of text = events_of_chunks [ text ]

let test_known_encodings () =
  List.iter
    (fun (bytes, expected) ->
       Alcotest.(check (list event_testable))
         (String.escaped bytes)
         [ expected ]
         (events_of bytes))
    encodings
;;

let test_several_events_in_one_read () =
  Alcotest.(check (list event_testable))
    "events in order"
    [ char "a"; key Event.Up; char "b" ]
    (events_of ("a" ^ escape ^ "[A" ^ "b"))
;;

let test_lone_escape_waits_to_be_settled () =
  let parser, events = Input_parser.feed Input_parser.empty escape in
  Alcotest.(check (list event_testable)) "nothing yet" [] events;
  Alcotest.(check bool) "pending" true (Input_parser.has_pending parser);
  let settled, flushed = Input_parser.flush parser in
  Alcotest.(check (list event_testable))
    "escape key"
    [ key Event.Escape ]
    flushed;
  Alcotest.(check bool)
    "nothing pending"
    false
    (Input_parser.has_pending settled)
;;

let test_double_escape_gives_one_escape_key_at_once () =
  let parser, events = Input_parser.feed Input_parser.empty (escape ^ escape) in
  Alcotest.(check (list event_testable))
    "first escape"
    [ key Event.Escape ]
    events;
  Alcotest.(check bool)
    "second escape pending"
    true
    (Input_parser.has_pending parser)
;;

let test_sequence_split_across_reads () =
  let parser, first_events =
    Input_parser.feed Input_parser.empty (escape ^ "[")
  in
  Alcotest.(check (list event_testable)) "nothing yet" [] first_events;
  let parser, second_events = Input_parser.feed parser "A" in
  Alcotest.(check (list event_testable)) "up" [ key Event.Up ] second_events;
  Alcotest.(check bool)
    "nothing pending"
    false
    (Input_parser.has_pending parser)
;;

let test_character_split_across_reads () =
  let first_half = "\xC3" in
  let second_half = "\xA9" in
  let parser, first_events = Input_parser.feed Input_parser.empty first_half in
  Alcotest.(check (list event_testable)) "nothing yet" [] first_events;
  let _, second_events = Input_parser.feed parser second_half in
  Alcotest.(check (list event_testable))
    "e acute"
    [ char "\u{E9}" ]
    second_events
;;

let test_a_mouse_report_arriving_in_pieces_is_put_together () =
  let whole = report ~code:0 ~column:10 ~row:5 () in
  let cut = 4 in
  let parser, first_events =
    Input_parser.feed Input_parser.empty (String.sub whole Int.zero cut)
  in
  Alcotest.(check (list event_testable)) "nothing yet" [] first_events;
  let _, second_events =
    Input_parser.feed parser (String.sub whole cut (String.length whole - cut))
  in
  Alcotest.(check (list event_testable))
    "press"
    [ mouse (Event.Press Event.Left_button) ~column:9 ~row:4 ]
    second_events
;;

let test_mouse_reports_among_keys_keep_their_order () =
  Alcotest.(check (list event_testable))
    "key, mouse, key"
    [ char "a"; mouse Event.Scroll_down ~column:0 ~row:0; char "b" ]
    (events_of ("a" ^ report ~code:65 ~column:1 ~row:1 () ^ "b"))
;;

let test_moves_without_a_button_and_broken_reports_are_ignored () =
  let motion_without_button = report ~code:35 ~column:5 ~row:5 () in
  let missing_row = escape ^ "[<0;10M" in
  Alcotest.(check (list event_testable))
    "ignored"
    [ char "a"; char "b" ]
    (events_of ("a" ^ motion_without_button ^ missing_row ^ "b"))
;;

let test_a_paste_arriving_in_pieces_is_delivered_once_it_is_complete () =
  let parser, first_events =
    Input_parser.feed Input_parser.empty (escape ^ "[200~par")
  in
  Alcotest.(check (list event_testable)) "nothing yet" [] first_events;
  let parser, second_events = Input_parser.feed parser "tial" in
  Alcotest.(check (list event_testable)) "still nothing" [] second_events;
  let parser, third_events =
    Input_parser.feed parser (" text" ^ escape ^ "[201~")
  in
  Alcotest.(check (list event_testable))
    "the whole paste"
    [ Event.Paste "partial text" ]
    third_events;
  Alcotest.(check bool)
    "nothing pending"
    false
    (Input_parser.has_pending parser)
;;

let test_a_pause_in_the_middle_of_a_paste_is_not_a_lone_escape () =
  let parser, _ = Input_parser.feed Input_parser.empty (escape ^ "[200~par") in
  Alcotest.(check bool)
    "no timeout needed"
    false
    (Input_parser.has_pending parser);
  let _, finished = Input_parser.feed parser (escape ^ "[201~") in
  Alcotest.(check (list event_testable)) "paste" [ Event.Paste "par" ] finished
;;

let test_keys_around_a_paste_are_not_mixed_with_it () =
  Alcotest.(check (list event_testable))
    "key, paste, key"
    [ char "a"; Event.Paste "b c"; char "d" ]
    (events_of ("a" ^ paste "b c" ^ "d"))
;;

let test_an_unfinished_start_marker_is_a_normal_pending_sequence () =
  let parser, events = Input_parser.feed Input_parser.empty (escape ^ "[20") in
  Alcotest.(check (list event_testable)) "nothing yet" [] events;
  Alcotest.(check bool) "pending" true (Input_parser.has_pending parser)
;;

let test_unknown_sequences_are_ignored () =
  let focus_report = escape ^ "[I" in
  let unknown_final = escape ^ "[99X" in
  Alcotest.(check (list event_testable))
    "ignored"
    [ char "a"; char "b" ]
    (events_of ("a" ^ focus_report ^ unknown_final ^ "b"))
;;

let test_invalid_bytes_are_dropped () =
  Alcotest.(check (list event_testable))
    "dropped"
    [ char "a" ]
    (events_of ("\xFF" ^ "a"))
;;

let test_flush_gives_up_on_unfinished_sequences () =
  let parser, events = Input_parser.feed Input_parser.empty (escape ^ "[1;") in
  Alcotest.(check (list event_testable)) "nothing yet" [] events;
  let settled, flushed = Input_parser.flush parser in
  Alcotest.(check bool)
    "starts with escape"
    true
    (List.hd flushed = key Event.Escape);
  Alcotest.(check bool)
    "nothing pending"
    false
    (Input_parser.has_pending settled)
;;

(* Cuts a text into pieces of the given sizes, repeating the sizes if needed. *)
let cut_in_chunks text sizes =
  let rec cut position sizes_left chunks =
    if position >= String.length text
    then List.rev chunks
    else (
      let size, next_sizes =
        match sizes_left with
        | size :: rest -> size, rest
        | [] -> String.length text, []
      in
      let length = Int.min size (String.length text - position) in
      cut
        (position + length)
        next_sizes
        (String.sub text position length :: chunks))
  in
  cut Int.zero sizes []
;;

let max_encoding_count = 8
let max_chunk_size = 4
let max_random_text_length = 24

let chunk_sizes =
  QCheck2.Gen.(
    list_size
      (int_range Int.one max_random_text_length)
      (int_range Int.one max_chunk_size))
;;

let property_chunking_does_not_change_the_events =
  QCheck2.Test.make
    ~name:"keys are the same however the bytes are cut"
    ~print:QCheck2.Print.(pair (list string) (list int))
    QCheck2.Gen.(
      pair
        (list_size
           (int_range Int.one max_encoding_count)
           (oneof_list (List.map fst encodings)))
        chunk_sizes)
    (fun (typed, sizes) ->
       let text = String.concat "" typed in
       let expected =
         List.map (fun bytes -> List.assoc bytes encodings) typed
       in
       events_of_chunks (cut_in_chunks text sizes) = expected)
;;

let property_any_bytes_are_handled_the_same_in_any_chunks =
  QCheck2.Test.make
    ~name:"arbitrary bytes give the same events however they are cut"
    ~print:QCheck2.Print.(pair string (list int))
    QCheck2.Gen.(
      pair
        (string_size ~gen:char (int_range Int.zero max_random_text_length))
        chunk_sizes)
    (fun (text, sizes) ->
       events_of_chunks (cut_in_chunks text sizes) = events_of text)
;;

let property_nothing_stays_pending_after_flush =
  QCheck2.Test.make
    ~name:"nothing stays pending once the parser is flushed"
    ~print:QCheck2.Print.string
    QCheck2.Gen.(
      string_size ~gen:char (int_range Int.zero max_random_text_length))
    (fun text ->
       let parser, _ = Input_parser.feed Input_parser.empty text in
       let settled, _ = Input_parser.flush parser in
       not (Input_parser.has_pending settled))
;;

let () =
  Alcotest.run
    "Input_parser"
    [ ( "keys"
      , [ Alcotest.test_case "known encodings" `Quick test_known_encodings
        ; Alcotest.test_case
            "several events"
            `Quick
            test_several_events_in_one_read
        ] )
    ; ( "pieces"
      , [ Alcotest.test_case
            "lone escape"
            `Quick
            test_lone_escape_waits_to_be_settled
        ; Alcotest.test_case
            "double escape"
            `Quick
            test_double_escape_gives_one_escape_key_at_once
        ; Alcotest.test_case
            "split sequence"
            `Quick
            test_sequence_split_across_reads
        ; Alcotest.test_case
            "split character"
            `Quick
            test_character_split_across_reads
        ; Alcotest.test_case
            "mouse report in pieces"
            `Quick
            test_a_mouse_report_arriving_in_pieces_is_put_together
        ; Alcotest.test_case
            "mouse reports among keys"
            `Quick
            test_mouse_reports_among_keys_keep_their_order
        ; Alcotest.test_case
            "paste in pieces"
            `Quick
            test_a_paste_arriving_in_pieces_is_delivered_once_it_is_complete
        ; Alcotest.test_case
            "pause inside a paste"
            `Quick
            test_a_pause_in_the_middle_of_a_paste_is_not_a_lone_escape
        ; Alcotest.test_case
            "keys around a paste"
            `Quick
            test_keys_around_a_paste_are_not_mixed_with_it
        ; Alcotest.test_case
            "unfinished start marker"
            `Quick
            test_an_unfinished_start_marker_is_a_normal_pending_sequence
        ] )
    ; ( "noise"
      , [ Alcotest.test_case
            "unknown sequences"
            `Quick
            test_unknown_sequences_are_ignored
        ; Alcotest.test_case
            "moves without a button"
            `Quick
            test_moves_without_a_button_and_broken_reports_are_ignored
        ; Alcotest.test_case
            "invalid bytes"
            `Quick
            test_invalid_bytes_are_dropped
        ; Alcotest.test_case
            "unfinished sequences"
            `Quick
            test_flush_gives_up_on_unfinished_sequences
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_chunking_does_not_change_the_events
          ; property_any_bytes_are_handled_the_same_in_any_chunks
          ; property_nothing_stays_pending_after_flush
          ] )
    ]
;;
