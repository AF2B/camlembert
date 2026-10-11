module Event = Camlembert.Event
module Input = Camlembert_unix.Input

let escape = "\027"
let patient_timeout = 2.0
let brief_timeout = 0.02
let only_look = 0.0
let terminal_columns = 80
let terminal_rows = 24
let bigger_columns = 120
let bigger_rows = 40
let event_testable = Alcotest.testable Event.pp Event.equal
let key key_value = Event.key_press key_value
let unchanged_size () = terminal_columns, terminal_rows

let close_quietly descriptor =
  try Unix.close descriptor with
  | Unix.Unix_error (Unix.EBADF, _, _) -> ()
;;

(* Runs a test on a pipe, which stands for the keyboard of a terminal. *)
let with_pipe run =
  let read_end, write_end = Unix.pipe () in
  Fun.protect
    ~finally:(fun () ->
      close_quietly read_end;
      close_quietly write_end)
    (fun () -> run ~read_end ~write_end)
;;

let send write_end text =
  ignore (Unix.write_substring write_end text Int.zero (String.length text))
;;

let reader ?(size = unchanged_size) read_end =
  Input.create ~descriptor:read_end ~size ()
;;

let test_a_pressed_key_is_delivered () =
  with_pipe (fun ~read_end ~write_end ->
    let source = reader read_end in
    send write_end "q";
    Alcotest.(check (option event_testable))
      "q"
      (Some (key (Event.Char "q")))
      (Input.poll source ~timeout:patient_timeout))
;;

let test_nothing_pressed_gives_nothing () =
  with_pipe (fun ~read_end ~write_end:_ ->
    let source = reader read_end in
    Alcotest.(check (option event_testable))
      "nothing"
      None
      (Input.poll source ~timeout:brief_timeout);
    Alcotest.(check (option event_testable))
      "nothing, without waiting"
      None
      (Input.poll source ~timeout:only_look))
;;

let test_events_read_together_come_out_in_order () =
  with_pipe (fun ~read_end ~write_end ->
    let source = reader read_end in
    send write_end ("ab" ^ escape ^ "[A");
    let next () = Input.poll source ~timeout:patient_timeout in
    Alcotest.(check (option event_testable))
      "first"
      (Some (key (Event.Char "a")))
      (next ());
    Alcotest.(check (option event_testable))
      "second"
      (Some (key (Event.Char "b")))
      (next ());
    Alcotest.(check (option event_testable))
      "third"
      (Some (key Event.Up))
      (next ());
    Alcotest.(check (option event_testable))
      "no more"
      None
      (Input.poll source ~timeout:only_look))
;;

let test_wait_returns_an_event_that_is_there () =
  with_pipe (fun ~read_end ~write_end ->
    let source = reader read_end in
    send write_end "x";
    Alcotest.check event_testable "x" (key (Event.Char "x")) (Input.wait source))
;;

let test_a_lone_escape_becomes_the_escape_key () =
  with_pipe (fun ~read_end ~write_end ->
    let source = reader read_end in
    send write_end escape;
    Alcotest.(check (option event_testable))
      "escape"
      (Some (key Event.Escape))
      (Input.poll source ~timeout:patient_timeout))
;;

let test_a_sequence_arriving_in_pieces_is_put_together () =
  with_pipe (fun ~read_end ~write_end ->
    let source = reader read_end in
    send write_end (escape ^ "[");
    Alcotest.(check (option event_testable))
      "still waiting"
      None
      (Input.poll source ~timeout:brief_timeout);
    send write_end "A";
    Alcotest.(check (option event_testable))
      "up"
      (Some (key Event.Up))
      (Input.poll source ~timeout:patient_timeout))
;;

let test_a_new_size_is_reported_once () =
  with_pipe (fun ~read_end ~write_end:_ ->
    let current_size = ref (terminal_columns, terminal_rows) in
    let source = reader ~size:(fun () -> !current_size) read_end in
    Alcotest.(check (option event_testable))
      "same size"
      None
      (Input.poll source ~timeout:brief_timeout);
    current_size := bigger_columns, bigger_rows;
    Alcotest.(check (option event_testable))
      "resized"
      (Some (Event.Resize { width = bigger_columns; height = bigger_rows }))
      (Input.poll source ~timeout:patient_timeout);
    Alcotest.(check (option event_testable))
      "reported once"
      None
      (Input.poll source ~timeout:brief_timeout))
;;

let test_a_closed_input_is_reported () =
  with_pipe (fun ~read_end ~write_end ->
    let source = reader read_end in
    Unix.close write_end;
    Alcotest.check_raises "end of input" End_of_file (fun () ->
      ignore (Input.wait source)))
;;

let () =
  Alcotest.run
    "Input"
    [ ( "keys"
      , [ Alcotest.test_case "key press" `Quick test_a_pressed_key_is_delivered
        ; Alcotest.test_case
            "nothing pressed"
            `Quick
            test_nothing_pressed_gives_nothing
        ; Alcotest.test_case
            "order"
            `Quick
            test_events_read_together_come_out_in_order
        ; Alcotest.test_case
            "wait"
            `Quick
            test_wait_returns_an_event_that_is_there
        ; Alcotest.test_case
            "lone escape"
            `Quick
            test_a_lone_escape_becomes_the_escape_key
        ; Alcotest.test_case
            "sequence in pieces"
            `Quick
            test_a_sequence_arriving_in_pieces_is_put_together
        ] )
    ; ( "terminal"
      , [ Alcotest.test_case "resize" `Quick test_a_new_size_is_reported_once
        ; Alcotest.test_case
            "closed input"
            `Quick
            test_a_closed_input_is_reported
        ] )
    ]
;;
