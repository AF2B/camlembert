type bar =
  { label : string
  ; value : int
  }

type t =
  { block : Block.t option
  ; style : Style.t
  ; bar_style : Style.t
  ; bar_width : int
  ; bar_gap : int
  ; maximum : int option
  ; bars : bar list
  }

let no_fraction = 0.0
let default_bar_width = 3
let default_bar_gap = 1
let label_rows = 1
let reversed = Style.make ~modifiers:[ Style.Reversed ] ()
let blank = " "

let make
      ?block
      ?(style = Style.default)
      ?(bar_style = Style.default)
      ?(bar_width = default_bar_width)
      ?(bar_gap = default_bar_gap)
      ?maximum
      bars
  =
  let is_negative value = value < Int.zero in
  if
    List.exists (fun bar -> is_negative bar.value) bars
    || Option.fold ~none:false ~some:is_negative maximum
  then invalid_arg "Bar_chart.make: negative value";
  if bar_width < Int.one then invalid_arg "Bar_chart.make: bar width below one";
  if is_negative bar_gap then invalid_arg "Bar_chart.make: negative gap";
  { block; style; bar_style; bar_width; bar_gap; maximum; bars }
;;

let largest_value chart =
  match chart.maximum with
  | Some maximum -> maximum
  | None ->
    List.fold_left
      (fun largest bar -> Int.max largest bar.value)
      Int.zero
      chart.bars
;;

let fraction_of ~value ~maximum =
  if maximum <= Int.zero
  then no_fraction
  else Float.of_int value /. Float.of_int maximum
;;

(* One row of a bar: a full cell is a blank with the colors swapped, so the
   value written over it stays readable; the top cell may be partly filled. *)
let draw_bar_row chart buffer ~column ~row ~eighths_in_row =
  if eighths_in_row >= Eighths.per_cell
  then
    Buffer.set_string
      buffer
      ~column
      ~row
      ~max_width:chart.bar_width
      ~style:(Style.patch chart.bar_style reversed)
      (String.concat "" (List.init chart.bar_width (Fun.const blank)))
  else if eighths_in_row > Int.zero
  then
    Buffer.set_string
      buffer
      ~column
      ~row
      ~max_width:chart.bar_width
      ~style:chart.bar_style
      (String.concat
         ""
         (List.init
            chart.bar_width
            (Fun.const (Eighths.lower_block eighths_in_row))))
;;

let draw_bar chart buffer inner ~bars_height ~maximum ~column bar =
  let { Rect.row = first_row; _ } = inner in
  let eighths =
    Eighths.filled
      ~fraction:(fraction_of ~value:bar.value ~maximum)
      ~cells:bars_height
  in
  List.init bars_height Fun.id
  |> List.iter (fun level ->
    draw_bar_row
      chart
      buffer
      ~column
      ~row:(first_row + bars_height - Int.one - level)
      ~eighths_in_row:(eighths - (level * Eighths.per_cell)));
  let value_text = Line.of_string (string_of_int bar.value) in
  if eighths >= Eighths.per_cell && Line.width value_text <= chart.bar_width
  then
    Line.render_aligned
      value_text
      buffer
      ~alignment:Alignment.Center
      ~column
      ~row:(first_row + bars_height - Int.one)
      ~width:chart.bar_width;
  if inner.Rect.height > bars_height
  then
    Line.render_aligned
      (Line.of_string bar.label)
      buffer
      ~alignment:Alignment.Center
      ~column
      ~row:(first_row + bars_height)
      ~width:chart.bar_width
;;

let draw_bars chart buffer inner =
  let { Rect.column = first_column; height; _ } = inner in
  let bars_height = Int.max Int.zero (height - label_rows) in
  let maximum = largest_value chart in
  let fits column = column + chart.bar_width <= Rect.right inner in
  List.iteri
    (fun index bar ->
       let column =
         first_column + (index * (chart.bar_width + chart.bar_gap))
       in
       if fits column
       then draw_bar chart buffer inner ~bars_height ~maximum ~column bar)
    chart.bars
;;

let widget chart area buffer =
  let inner =
    match chart.block with
    | None -> area
    | Some block ->
      Block.widget block area buffer;
      Block.inner block area
  in
  Buffer.set_style buffer inner chart.style;
  if not (Rect.is_empty inner) then draw_bars chart buffer inner
;;
