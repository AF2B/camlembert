type t =
  { block : Block.t option
  ; style : Style.t
  ; gauge_style : Style.t
  ; label : Line.t option
  ; label_style : Style.t
  ; ratio : float
  }

let lowest_ratio = 0.0
let highest_ratio = 1.0
let percent_scale = 100.0
let eighths_per_cell = 8
let halving_divisor = 2
let half_of value = value / halving_divisor

(* The block that fills a cell from the left, from an eighth to seven eighths. *)
let partial_blocks =
  [ "\u{258F}"
  ; "\u{258E}"
  ; "\u{258D}"
  ; "\u{258C}"
  ; "\u{258B}"
  ; "\u{258A}"
  ; "\u{2589}"
  ]
;;

let filled_style = Style.make ~modifiers:[ Style.Reversed ] ()
let filled_symbol = " "

let make
      ?block
      ?(style = Style.default)
      ?(gauge_style = Style.default)
      ?label
      ?(label_style = Style.default)
      ~ratio
      ()
  =
  if Float.is_nan ratio || ratio < lowest_ratio || ratio > highest_ratio
  then invalid_arg "Gauge.make: ratio outside of 0 to 1";
  { block; style; gauge_style; label; label_style; ratio }
;;

let percentage_label gauge =
  let percent = Float.to_int (Float.round (gauge.ratio *. percent_scale)) in
  Line.of_string (Printf.sprintf "%d%%" percent)
;;

(* The label with its own styles layered over the label style. *)
let styled_label gauge =
  let label = Option.value gauge.label ~default:(percentage_label gauge) in
  let restyle (span : Span.t) =
    { span with Span.style = Style.patch gauge.label_style span.style }
  in
  { label with Line.spans = List.map restyle label.Line.spans }
;;

let draw_bar gauge buffer inner =
  let { Rect.column; row; width; height } = inner in
  let filled_eighths =
    Float.to_int
      (Float.round (gauge.ratio *. Float.of_int (width * eighths_per_cell)))
  in
  let full_cells = filled_eighths / eighths_per_cell in
  let partial_eighths = filled_eighths mod eighths_per_cell in
  let draw_row current_row =
    Buffer.set_string
      buffer
      ~column
      ~row:current_row
      ~max_width:full_cells
      ~style:(Style.patch gauge.gauge_style filled_style)
      (String.concat "" (List.init full_cells (Fun.const filled_symbol)));
    if partial_eighths > Int.zero
    then
      Buffer.set_string
        buffer
        ~column:(column + full_cells)
        ~row:current_row
        ~max_width:Int.one
        ~style:gauge.gauge_style
        (List.nth partial_blocks (pred partial_eighths))
  in
  List.init height (fun offset -> row + offset) |> List.iter draw_row
;;

let draw_label gauge buffer inner =
  let { Rect.column; row; width; height } = inner in
  Line.render_aligned
    (styled_label gauge)
    buffer
    ~alignment:Alignment.Center
    ~column
    ~row:(row + half_of height)
    ~width
;;

let widget gauge area buffer =
  let inner =
    match gauge.block with
    | None -> area
    | Some block ->
      Block.widget block area buffer;
      Block.inner block area
  in
  Buffer.set_style buffer inner gauge.style;
  if not (Rect.is_empty inner)
  then (
    draw_bar gauge buffer inner;
    draw_label gauge buffer inner)
;;
