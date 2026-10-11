type t =
  { track_style : Style.t
  ; thumb_style : Style.t
  ; track_symbol : string
  ; thumb_symbol : string
  ; side : Block.side
  ; content_length : int
  ; viewport_length : int
  ; position : int
  }

let thin_vertical_line = "\u{2502}"
let full_block = "\u{2588}"
let shortest_thumb = Int.one

let make
      ?(track_style = Style.default)
      ?(thumb_style = Style.default)
      ?(track_symbol = thin_vertical_line)
      ?(thumb_symbol = full_block)
      ~side
      ~content_length
      ~viewport_length
      ~position
      ()
  =
  if
    content_length < Int.zero
    || viewport_length < Int.zero
    || position < Int.zero
  then invalid_arg "Scrollbar.make: negative length or position";
  { track_style
  ; thumb_style
  ; track_symbol
  ; thumb_symbol
  ; side
  ; content_length
  ; viewport_length
  ; position
  }
;;

let rounded_ratio ~of_ numerator denominator =
  Float.to_int
    (Float.round
       (Float.of_int of_ *. Float.of_int numerator /. Float.of_int denominator))
;;

(* The thumb is as long, on the track, as the visible part is in the content,
   but never shorter than one cell. *)
let thumb_length bar ~track =
  if bar.content_length <= bar.viewport_length
  then track
  else
    Int.min
      track
      (Int.max
         shortest_thumb
         (rounded_ratio ~of_:track bar.viewport_length bar.content_length))
;;

let thumb_start bar ~track ~thumb =
  let furthest_position = bar.content_length - bar.viewport_length in
  if furthest_position <= Int.zero
  then Int.zero
  else (
    let position = Int.min furthest_position bar.position in
    rounded_ratio ~of_:(track - thumb) position furthest_position)
;;

(* The cell at [index] along the track, counted from the top or the left. *)
let cell_position bar area index =
  let { Rect.column; row; _ } = area in
  match bar.side with
  | Block.Right -> Rect.right area - Int.one, row + index
  | Block.Left -> column, row + index
  | Block.Bottom -> column + index, Rect.bottom area - Int.one
  | Block.Top -> column + index, row
;;

let track_size bar area =
  match bar.side with
  | Block.Left | Block.Right -> area.Rect.height
  | Block.Top | Block.Bottom -> area.Rect.width
;;

let widget bar area buffer =
  let track = track_size bar area in
  let thumb = thumb_length bar ~track in
  let start = thumb_start bar ~track ~thumb in
  let draw_cell index =
    let column, row = cell_position bar area index in
    let on_thumb = index >= start && index < start + thumb in
    Buffer.set_string
      buffer
      ~column
      ~row
      ~max_width:Int.one
      ~style:(if on_thumb then bar.thumb_style else bar.track_style)
      (if on_thumb then bar.thumb_symbol else bar.track_symbol)
  in
  if not (Rect.is_empty area) then List.init track Fun.id |> List.iter draw_cell
;;
