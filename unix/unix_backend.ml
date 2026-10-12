module Backend = Camlembert.Backend
module Position = Camlembert.Position
module Rect = Camlembert.Rect
module Terminal = Camlembert.Terminal

let origin = 0

let screen_area () =
  let columns, rows = Tty.size () in
  Rect.make ~column:origin ~row:origin ~width:columns ~height:rows
;;

let send text = Out_channel.output_string Out_channel.stdout text
let flush () = Out_channel.flush Out_channel.stdout

(* Seconds the terminal is given to tell where its cursor is. *)
let cursor_report_timeout = 1.0
let no_time = 0.0

(* Asks the terminal and reads the answer from the input, ignoring anything
   else that arrives meanwhile. *)
let cursor_position () =
  send Ansi.request_cursor_position;
  flush ();
  let deadline = Unix.gettimeofday () +. cursor_report_timeout in
  let rec wait_for_report received =
    match Ansi.find_cursor_report received with
    | Some position -> position
    | None ->
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= no_time
      then
        failwith
          "Unix_backend.cursor_position: the terminal did not report the \
           position of the cursor"
      else
        wait_for_report
          (received
           ^ Option.value
               (Tty.read_within ~descriptor:Unix.stdin ~seconds:remaining)
               ~default:"")
  in
  wait_for_report ""
;;

let move_cursor { Position.column; row } = send (Ansi.move_cursor ~column ~row)
let clear_from { Position.column; row } = send (Ansi.clear_from ~column ~row)

let scroll_up rows =
  send (Ansi.scroll_up ~rows ~last_row:(pred (screen_area ()).Rect.height))
;;

let backend () =
  { Backend.size = screen_area
  ; cursor_position
  ; draw = (fun changes -> send (Ansi.encode_changes changes))
  ; clear_from
  ; scroll_up
  ; move_cursor
  ; set_cursor = (fun position -> send (Ansi.encode_cursor position))
  ; flush
  }
;;

let require_terminal () =
  if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout)
  then
    failwith
      "Unix_backend.with_terminal: standard input and output must be a terminal"
;;

type feature =
  | Mouse
  | Bracketed_paste

let enable_sequence = function
  | Mouse -> Ansi.enable_mouse
  | Bracketed_paste -> Ansi.enable_bracketed_paste
;;

let disable_sequence = function
  | Mouse -> Ansi.disable_mouse
  | Bracketed_paste -> Ansi.disable_bracketed_paste
;;

let with_terminal ?(features = []) run =
  require_terminal ();
  let saved_mode = Tty.enter_raw_mode () in
  let enable_features = String.concat "" (List.map enable_sequence features) in
  let disable_features =
    String.concat "" (List.map disable_sequence features)
  in
  let leave_terminal () =
    send
      (disable_features
       ^ Ansi.reset_style
       ^ Ansi.show_cursor
       ^ Ansi.leave_alternate_screen);
    flush ();
    Tty.restore_mode saved_mode;
    Sys.catch_break false
  in
  Sys.catch_break true;
  Fun.protect ~finally:leave_terminal (fun () ->
    send
      (Ansi.enter_alternate_screen
       ^ enable_features
       ^ Ansi.hide_cursor
       ^ Ansi.clear_screen);
    run (Terminal.create (backend ())))
;;
