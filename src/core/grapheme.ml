(* This library defines its own [Buffer] module, so the standard one is always
   written as [Stdlib.Buffer] here. *)

let widest_cluster = 2
let first_printable_ascii = ' '
let delete_character = '\127'

let is_printable_ascii character =
  character >= first_printable_ascii && character < delete_character
;;

let sum = List.fold_left ( + ) Int.zero

let scalars text =
  let rec collect offset collected =
    if offset >= String.length text
    then List.rev collected
    else (
      let decoded = String.get_utf_8_uchar text offset in
      collect
        (offset + Uchar.utf_decode_length decoded)
        (Uchar.utf_decode_uchar decoded :: collected))
  in
  collect Int.zero []
;;

let string_of_scalars scalars =
  let buffer = Stdlib.Buffer.create (List.length scalars) in
  List.iter (Stdlib.Buffer.add_utf_8_uchar buffer) scalars;
  Stdlib.Buffer.contents buffer
;;

(* Both lists are kept newest first while scanning. *)
type scan =
  { finished : string list
  ; current : Uchar.t list
  }

let empty_scan = { finished = []; current = [] }

let close_cluster scan =
  match scan.current with
  | [] -> scan
  | newest_first ->
    { finished = string_of_scalars (List.rev newest_first) :: scan.finished
    ; current = []
    }
;;

let clusters text =
  let segmenter = Uuseg.create `Grapheme_cluster in
  let rec feed scan event =
    match Uuseg.add segmenter event with
    | `Uchar scalar ->
      feed { scan with current = scalar :: scan.current } `Await
    | `Boundary -> feed (close_cluster scan) `Await
    | `Await | `End -> scan
  in
  let feed_scalar scan scalar = feed scan (`Uchar scalar) in
  let scanned_text = List.fold_left feed_scalar empty_scan (scalars text) in
  let complete = close_cluster (feed scanned_text `End) in
  List.rev complete.finished
;;

let scalar_width scalar = Int.max Int.zero (Uucp.Break.tty_width_hint scalar)

let cluster_width cluster =
  scalars cluster |> List.map scalar_width |> sum |> Int.min widest_cluster
;;

(* Printable ASCII is one column per character, so it skips the segmenter. *)
let width text =
  if String.for_all is_printable_ascii text
  then String.length text
  else clusters text |> List.map cluster_width |> sum
;;
