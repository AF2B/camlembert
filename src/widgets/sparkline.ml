type t =
  { block : Block.t option
  ; style : Style.t
  ; maximum : int option
  ; values : int list
  }

let no_fraction = 0.0

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
  let fraction =
    if maximum <= Int.zero
    then no_fraction
    else Float.of_int value /. Float.of_int maximum
  in
  Eighths.filled ~fraction ~cells:height
;;

(* The symbol of the cell [level] cells above the bottom of a bar. *)
let bar_symbol ~eighths ~level =
  Eighths.lower_block (eighths - (level * Eighths.per_cell))
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
