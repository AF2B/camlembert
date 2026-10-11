type t =
  { block : Block.t option
  ; style : Style.t
  ; maximum : int option
  ; values : int list
  }

let eighths_per_cell = 8

(* From an empty cell to a full one, in steps of an eighth. *)
let bar_symbols =
  [| " "
   ; "\u{2581}"
   ; "\u{2582}"
   ; "\u{2583}"
   ; "\u{2584}"
   ; "\u{2585}"
   ; "\u{2586}"
   ; "\u{2587}"
   ; "\u{2588}"
  |]
;;

let make ?block ?(style = Style.default) ?maximum values =
  let is_negative value = value < Int.zero in
  if
    List.exists is_negative values
    || Option.fold ~none:false ~some:is_negative maximum
  then invalid_arg "Sparkline.make: negative value";
  { block; style; maximum; values }
;;

let largest_value sparkline =
  match sparkline.maximum with
  | Some maximum -> maximum
  | None -> List.fold_left Int.max Int.zero sparkline.values
;;

(* The last [count] values, or all of them if there are fewer. *)
let last_values values ~count =
  let skipped = Int.max Int.zero (List.length values - count) in
  List.filteri (fun index _ -> index >= skipped) values
;;

(* How many eighths of the full height a value fills. *)
let filled_eighths ~value ~maximum ~height =
  let total_eighths = height * eighths_per_cell in
  if maximum <= Int.zero
  then Int.zero
  else
    Int.min
      total_eighths
      (Float.to_int
         (Float.round
            (Float.of_int value
             *. Float.of_int total_eighths
             /. Float.of_int maximum)))
;;

(* The symbol of the cell [level] cells above the bottom of a bar. *)
let bar_symbol ~eighths ~level =
  let in_this_cell =
    Int.max
      Int.zero
      (Int.min eighths_per_cell (eighths - (level * eighths_per_cell)))
  in
  bar_symbols.(in_this_cell)
;;

let draw_bars sparkline buffer inner =
  let { Rect.column; row; width; height } = inner in
  let maximum = largest_value sparkline in
  let draw_bar offset value =
    let eighths = filled_eighths ~value ~maximum ~height in
    List.init height Fun.id
    |> List.iter (fun level ->
      Buffer.set_string
        buffer
        ~column:(column + offset)
        ~row:(row + height - Int.one - level)
        ~max_width:Int.one
        ~style:Style.default
        (bar_symbol ~eighths ~level))
  in
  List.iteri draw_bar (last_values sparkline.values ~count:width)
;;

let widget sparkline area buffer =
  let inner =
    match sparkline.block with
    | None -> area
    | Some block ->
      Block.widget block area buffer;
      Block.inner block area
  in
  Buffer.set_style buffer inner sparkline.style;
  if not (Rect.is_empty inner) then draw_bars sparkline buffer inner
;;
