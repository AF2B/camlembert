type t =
  { block : Block.t option
  ; style : Style.t
  ; header : Line.t list option
  ; header_style : Style.t
  ; highlight_style : Style.t
  ; highlight_symbol : string
  ; column_spacing : int
  ; widths : Constraint.t list
  ; rows : Line.t list list
  }

let default_column_spacing = 1
let header_rows = 1

let make
      ?block
      ?(style = Style.default)
      ?header
      ?(header_style = Style.default)
      ?(highlight_style = Style.default)
      ?(highlight_symbol = "")
      ?(column_spacing = default_column_spacing)
      ~widths
      rows
  =
  { block
  ; style
  ; header
  ; header_style
  ; highlight_style
  ; highlight_symbol
  ; column_spacing
  ; widths
  ; rows
  }
;;

(* A column or the gap that separates it from the next one. *)
type slot =
  | Column of Constraint.t
  | Gap

let rec with_gaps = function
  | [] -> []
  | [ last ] -> [ last ]
  | slot :: rest -> slot :: Gap :: with_gaps rest
;;

(* The region of each column on a row that starts at [column]. *)
let column_regions table ~column ~row ~width =
  let slots = with_gaps (List.map (fun width -> Column width) table.widths) in
  let constraint_of = function
    | Column constraint_ -> constraint_
    | Gap -> Constraint.Length table.column_spacing
  in
  let row_width = Int.max Int.zero width in
  let row_region = Rect.make ~column ~row ~width:row_width ~height:Int.one in
  Layout.horizontal (List.map constraint_of slots) row_region
  |> List.combine slots
  |> List.filter_map (function
    | Column _, region -> Some region
    | Gap, _ -> None)
;;

let rec zip_shortest first second =
  match first, second with
  | head :: rest, other :: other_rest ->
    (head, other) :: zip_shortest rest other_rest
  | [], _ | _, [] -> []
;;

let draw_cells buffer regions ~row cells =
  zip_shortest cells regions
  |> List.iter (fun (cell, region) ->
    Line.render_aligned
      cell
      buffer
      ~alignment:Alignment.Left
      ~column:region.Rect.column
      ~row
      ~width:region.Rect.width)
;;

let draw_header table buffer ~regions ~inner cells =
  let { Rect.column; row; width; _ } = inner in
  Buffer.set_style
    buffer
    (Rect.make ~column ~row ~width ~height:Int.one)
    table.header_style;
  draw_cells buffer regions ~row cells
;;

let highlight_row table buffer ~column ~row ~width =
  Buffer.set_style
    buffer
    (Rect.make ~column ~row ~width ~height:Int.one)
    table.highlight_style;
  Buffer.set_string
    buffer
    ~column
    ~row
    ~max_width:(Grapheme.width table.highlight_symbol)
    ~style:Style.default
    table.highlight_symbol
;;

let draw_body table buffer ~inner ~body_row ~symbol_width selection =
  let { Rect.column; width; height; _ } = inner in
  let offset = Selection.offset selection in
  let body_height = height - (body_row - inner.Rect.row) in
  let regions =
    column_regions
      table
      ~column:(column + symbol_width)
      ~row:body_row
      ~width:(width - symbol_width)
  in
  let draw_row (index, cells) =
    let row = body_row + index - offset in
    if Selection.selected selection = Some index
    then highlight_row table buffer ~column ~row ~width;
    draw_cells buffer regions ~row cells
  in
  table.rows
  |> List.mapi (fun index cells -> index, cells)
  |> List.filter (fun (index, _) ->
    index >= offset && index < offset + body_height)
  |> List.iter draw_row
;;

let widget table area buffer selection =
  let inner =
    match table.block with
    | None -> area
    | Some block ->
      Block.widget block area buffer;
      Block.inner block area
  in
  Buffer.set_style buffer inner table.style;
  if not (Rect.is_empty inner)
  then (
    let { Rect.column; row; width; height } = inner in
    let reserved_rows =
      if Option.is_some table.header then header_rows else Int.zero
    in
    Selection.reveal
      selection
      ~item_count:(List.length table.rows)
      ~visible_rows:(height - reserved_rows);
    let symbol_width =
      if Option.is_some (Selection.selected selection)
      then Grapheme.width table.highlight_symbol
      else Int.zero
    in
    Option.iter
      (draw_header
         table
         buffer
         ~regions:
           (column_regions
              table
              ~column:(column + symbol_width)
              ~row
              ~width:(width - symbol_width))
         ~inner)
      table.header;
    draw_body
      table
      buffer
      ~inner
      ~body_row:(row + reserved_rows)
      ~symbol_width
      selection)
;;
