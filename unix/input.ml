module Event = Camlembert.Event
module Input_parser = Camlembert.Input_parser

type t =
  { descriptor : Unix.file_descr
  ; size : unit -> int * int
  ; mutable parser : Input_parser.t
  ; mutable queue : Event.t list
  ; mutable last_size : int * int
  }

(* Seconds a lone escape byte waits for the rest of its sequence. *)
let escape_timeout = 0.05

(* Seconds between two looks at the size of the terminal. *)
let resize_poll_interval = 0.1
let no_time = 0.0
let read_chunk_size = 1024
let end_of_input = 0

let create ?(descriptor = Unix.stdin) ?(size = Tty.size) () =
  { descriptor
  ; size
  ; parser = Input_parser.empty
  ; queue = []
  ; last_size = size ()
  }
;;

let enqueue source events = source.queue <- source.queue @ events

let pop source =
  match source.queue with
  | [] -> None
  | event :: rest ->
    source.queue <- rest;
    Some event
;;

let feed source bytes =
  let parser, events = Input_parser.feed source.parser bytes in
  source.parser <- parser;
  enqueue source events
;;

let settle_pending source =
  let parser, events = Input_parser.flush source.parser in
  source.parser <- parser;
  enqueue source events
;;

let notice_resize source =
  let current_size = source.size () in
  if current_size <> source.last_size
  then (
    source.last_size <- current_size;
    let width, height = current_size in
    enqueue source [ Event.Resize { width; height } ])
;;

let is_readable source ~seconds =
  match Unix.select [ source.descriptor ] [] [] seconds with
  | [], _, _ -> false
  | _ :: _, _, _ -> true
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> false
;;

let read_available source =
  let bytes = Bytes.create read_chunk_size in
  match Unix.read source.descriptor bytes 0 read_chunk_size with
  | count when count = end_of_input -> raise End_of_file
  | count -> feed source (Bytes.sub_string bytes 0 count)
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
;;

(* Waits at most [remaining] seconds for input, then takes care of whatever
   happened: new bytes, a lone escape byte that waited long enough, or a
   resize. *)
let step source ~remaining =
  let waits_for_escape = Input_parser.has_pending source.parser in
  let slice =
    if waits_for_escape then escape_timeout else resize_poll_interval
  in
  let seconds = Float.min remaining slice in
  if is_readable source ~seconds
  then read_available source
  else if waits_for_escape && seconds >= escape_timeout
  then settle_pending source;
  notice_resize source
;;

let rec poll_until source ~deadline =
  let remaining = Float.max no_time (deadline -. Unix.gettimeofday ()) in
  step source ~remaining;
  match pop source with
  | Some _ as event -> event
  | None -> if remaining <= no_time then None else poll_until source ~deadline
;;

let poll source ~timeout =
  match pop source with
  | Some _ as event -> event
  | None -> poll_until source ~deadline:(Unix.gettimeofday () +. timeout)
;;

let rec wait source =
  match poll source ~timeout:resize_poll_interval with
  | Some event -> event
  | None -> wait source
;;
