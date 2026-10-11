module Backend = Camlembert.Backend
module Rect = Camlembert.Rect
module Terminal = Camlembert.Terminal

let origin = 0

let screen_area () =
  let columns, rows = Tty.size () in
  Rect.make ~column:origin ~row:origin ~width:columns ~height:rows
;;

let send text = Out_channel.output_string Out_channel.stdout text
let flush () = Out_channel.flush Out_channel.stdout

let backend () =
  { Backend.size = screen_area
  ; draw = (fun changes -> send (Ansi.encode_changes changes))
  ; clear = (fun () -> send Ansi.clear_screen)
  ; flush
  }
;;

let require_terminal () =
  if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout)
  then
    failwith
      "Unix_backend.with_terminal: standard input and output must be a terminal"
;;

let with_terminal run =
  require_terminal ();
  let saved_mode = Tty.enter_raw_mode () in
  let leave_terminal () =
    send (Ansi.reset_style ^ Ansi.show_cursor ^ Ansi.leave_alternate_screen);
    flush ();
    Tty.restore_mode saved_mode;
    Sys.catch_break false
  in
  Sys.catch_break true;
  Fun.protect ~finally:leave_terminal (fun () ->
    send (Ansi.enter_alternate_screen ^ Ansi.hide_cursor ^ Ansi.clear_screen);
    run (Terminal.create (backend ())))
;;
