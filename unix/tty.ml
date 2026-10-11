type saved_mode = Unix.terminal_io

external size : unit -> int * int = "camlembert_terminal_size"

let bits_per_character = 8
let minimum_bytes_per_read = 1
let no_read_timeout = 0

(* Same switches as cfmakeraw, except that interrupt signals stay enabled. *)
let raw_attributes (saved : Unix.terminal_io) =
  { saved with
    Unix.c_ignbrk = false
  ; c_brkint = false
  ; c_parmrk = false
  ; c_istrip = false
  ; c_inlcr = false
  ; c_igncr = false
  ; c_icrnl = false
  ; c_ixon = false
  ; c_opost = false
  ; c_echo = false
  ; c_echonl = false
  ; c_icanon = false
  ; c_parenb = false
  ; c_csize = bits_per_character
  ; c_vmin = minimum_bytes_per_read
  ; c_vtime = no_read_timeout
  }
;;

let enter_raw_mode () =
  let saved = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH (raw_attributes saved);
  saved
;;

let restore_mode saved = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH saved
