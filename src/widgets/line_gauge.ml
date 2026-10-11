type t =
  { block : Block.t option
  ; style : Style.t
  ; filled_style : Style.t
  ; unfilled_style : Style.t
  ; label : Line.t option
  ; label_style : Style.t
  ; ratio : float
  }

let lowest_ratio = 0.0
let highest_ratio = 1.0
let percent_scale = 100.0
let filled_symbol = "\u{2501}"
let unfilled_symbol = "\u{2500}"
let label_gap = Int.one

let make
      ?block
      ?(style = Style.default)
      ?(filled_style = Style.default)
      ?(unfilled_style = Style.default)
      ?label
      ?(label_style = Style.default)
      ~ratio
      ()
  =
  if Float.is_nan ratio || ratio < lowest_ratio || ratio > highest_ratio
  then invalid_arg "Line_gauge.make: ratio outside of 0 to 1";
  { block; style; filled_style; unfilled_style; label; label_style; ratio }
;;

let percentage_label gauge =
  let percent = Float.to_int (Float.round (gauge.ratio *. percent_scale)) in
  Line.of_string (Printf.sprintf "%d%%" percent)
;;

let styled_label gauge =
  Line.patch_style
    (Option.value gauge.label ~default:(percentage_label gauge))
    gauge.label_style
;;

let run_of symbol count = String.concat "" (List.init count (Fun.const symbol))

let draw_bar gauge buffer ~column ~row ~width =
  let filled_cells =
    Eighths.filled ~fraction:gauge.ratio ~cells:width / Eighths.per_cell
  in
  Buffer.set_string
    buffer
    ~column
    ~row
    ~max_width:filled_cells
    ~style:gauge.filled_style
    (run_of filled_symbol filled_cells);
  Buffer.set_string
    buffer
    ~column:(column + filled_cells)
    ~row
    ~max_width:(width - filled_cells)
    ~style:gauge.unfilled_style
    (run_of unfilled_symbol (width - filled_cells))
;;

let draw_line gauge buffer inner =
  let { Rect.column; row; width; _ } = inner in
  let label = styled_label gauge in
  let label_width = Line.width label in
  let gap = if label_width > Int.zero then label_gap else Int.zero in
  let bar_column = column + label_width + gap in
  let bar_width = Rect.right inner - bar_column in
  Line.render label buffer ~column ~row ~max_width:width;
  if bar_width > Int.zero
  then draw_bar gauge buffer ~column:bar_column ~row ~width:bar_width
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
  if not (Rect.is_empty inner) then draw_line gauge buffer inner
;;
