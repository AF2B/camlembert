type t =
  { block : Block.t option
  ; style : Style.t
  ; highlight_style : Style.t
  ; highlight_symbol : string
  ; items : Line.t list
  }

let make
      ?block
      ?(style = Style.default)
      ?(highlight_style = Style.default)
      ?(highlight_symbol = "")
      items
  =
  { block; style; highlight_style; highlight_symbol; items }
;;

let highlight_row list buffer ~column ~row ~width =
  Buffer.set_style
    buffer
    (Rect.make ~column ~row ~width ~height:Int.one)
    list.highlight_style;
  Buffer.set_string
    buffer
    ~column
    ~row
    ~max_width:(Grapheme.width list.highlight_symbol)
    ~style:Style.default
    list.highlight_symbol
;;

let draw_rows list buffer inner selection =
  let { Rect.column; row = first_row; width; height } = inner in
  let offset = Selection.offset selection in
  let symbol_width =
    if Option.is_some (Selection.selected selection)
    then Grapheme.width list.highlight_symbol
    else Int.zero
  in
  let draw_row (index, item) =
    let row = first_row + index - offset in
    if Selection.selected selection = Some index
    then highlight_row list buffer ~column ~row ~width;
    Line.render_aligned
      item
      buffer
      ~alignment:Alignment.Left
      ~column:(column + symbol_width)
      ~row
      ~width:(width - symbol_width)
  in
  list.items
  |> List.mapi (fun index item -> index, item)
  |> List.filter (fun (index, _) -> index >= offset && index < offset + height)
  |> List.iter draw_row
;;

let inner_area list area =
  match list.block with
  | None -> area
  | Some block -> Block.inner block area
;;

let index_at list selection ~area { Position.column; row } =
  let inner = inner_area list area in
  let item_count = List.length list.items in
  Selection.reveal selection ~item_count ~visible_rows:inner.Rect.height;
  let index = Selection.offset selection + (row - inner.Rect.row) in
  if Rect.contains inner ~column ~row && index < item_count
  then Some index
  else None
;;

let widget list area buffer selection =
  let inner =
    match list.block with
    | None -> area
    | Some block ->
      Block.widget block area buffer;
      Block.inner block area
  in
  Buffer.set_style buffer inner list.style;
  if not (Rect.is_empty inner)
  then (
    Selection.reveal
      selection
      ~item_count:(List.length list.items)
      ~visible_rows:inner.Rect.height;
    draw_rows list buffer inner selection)
;;
