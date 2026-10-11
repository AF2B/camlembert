type mode =
  | Word
  | Character

(* A grapheme cluster with the style of the span it comes from. *)
type piece =
  { cluster : string
  ; style : Style.t
  ; width : int
  }

let space_cluster = " "
let narrowest_width = Int.one
let is_space piece = piece.cluster = space_cluster

let width_of_pieces =
  List.fold_left (fun total piece -> total + piece.width) Int.zero
;;

let pieces_of_line (line : Line.t) =
  line.spans
  |> List.concat_map (fun (span : Span.t) ->
    Grapheme.clusters span.content
    |> List.map (fun cluster ->
      { cluster; style = span.style; width = Grapheme.width cluster }))
  |> List.filter (fun piece -> piece.width > Int.zero)
;;

type token =
  | Spaces of piece list
  | Word_token of piece list

(* Groups consecutive pieces into runs of spaces and runs of other pieces. *)
let words_and_spaces pieces =
  let add_to_tokens piece tokens =
    match tokens, is_space piece with
    | Spaces run :: rest, true -> Spaces (piece :: run) :: rest
    | Word_token run :: rest, false -> Word_token (piece :: run) :: rest
    | _, true -> Spaces [ piece ] :: tokens
    | _, false -> Word_token [ piece ] :: tokens
  in
  List.fold_right add_to_tokens pieces []
;;

(* Rows are laid out piece by piece. The rows already finished and the piece of
   the row being built are kept newest first. *)
type layout =
  { finished_rows : piece list list
  ; current_row : piece list
  ; used_width : int
  }

let empty_layout =
  { finished_rows = []; current_row = []; used_width = Int.zero }
;;

let add_piece layout piece =
  { layout with
    current_row = piece :: layout.current_row
  ; used_width = layout.used_width + piece.width
  }
;;

let add_pieces layout pieces = List.fold_left add_piece layout pieces

let end_row layout =
  match layout.current_row with
  | [] -> layout
  | newest_first ->
    { finished_rows = List.rev newest_first :: layout.finished_rows
    ; current_row = []
    ; used_width = Int.zero
    }
;;

let rec without_trailing_spaces = function
  | piece :: rest when is_space piece -> without_trailing_spaces rest
  | row -> row
;;

(* Ends a row between words, where the spaces that end it are not shown. *)
let break_row layout =
  end_row
    { layout with current_row = without_trailing_spaces layout.current_row }
;;

(* A piece always fits on an empty row, even a piece wider than the width. *)
let place_piece ~width layout piece =
  if
    List.is_empty layout.current_row || layout.used_width + piece.width <= width
  then add_piece layout piece
  else add_piece (end_row layout) piece
;;

let place_token ~width layout token =
  match token with
  | Spaces pieces ->
    if layout.used_width + width_of_pieces pieces <= width
    then add_pieces layout pieces
    else break_row layout
  | Word_token pieces ->
    let token_width = width_of_pieces pieces in
    if layout.used_width + token_width <= width
    then add_pieces layout pieces
    else if token_width <= width
    then add_pieces (break_row layout) pieces
    else List.fold_left (place_piece ~width) (break_row layout) pieces
;;

let rows_of_pieces mode ~width pieces =
  let layout =
    match mode with
    | Character -> List.fold_left (place_piece ~width) empty_layout pieces
    | Word ->
      List.fold_left (place_token ~width) empty_layout (words_and_spaces pieces)
  in
  match (end_row layout).finished_rows with
  | [] -> [ [] ]
  | newest_first -> List.rev newest_first
;;

(* Pieces that follow each other with the same style become one span. *)
let spans_of_row row =
  let add_to_spans piece spans =
    match spans with
    | (span : Span.t) :: rest when Style.equal span.style piece.style ->
      { span with Span.content = piece.cluster ^ span.content } :: rest
    | _ -> Span.make ~style:piece.style piece.cluster :: spans
  in
  List.fold_right add_to_spans row []
;;

let line mode ~width (line : Line.t) =
  let width = Int.max narrowest_width width in
  pieces_of_line line
  |> rows_of_pieces mode ~width
  |> List.map (fun row ->
    Line.make ?alignment:line.alignment (spans_of_row row))
;;

let text mode ~width text = List.concat_map (line mode ~width) text
