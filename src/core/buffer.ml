type t =
  { area : Rect.t
  ; cells : Cell.t array
  }

type change =
  { column : int
  ; row : int
  ; cell : Cell.t
  }

let create area = { area; cells = Array.make (Rect.area area) Cell.empty }
let area buffer = buffer.area

let index_of buffer ~column ~row =
  let { Rect.column = first_column; row = first_row; width; _ } = buffer.area in
  ((row - first_row) * width) + (column - first_column)
;;

let position_of buffer index =
  let { Rect.column = first_column; row = first_row; width; _ } = buffer.area in
  first_column + (index mod width), first_row + (index / width)
;;

let get buffer ~column ~row =
  if Rect.contains buffer.area ~column ~row
  then buffer.cells.(index_of buffer ~column ~row)
  else invalid_arg "Buffer.get: position outside of the buffer area"
;;

let set_cell buffer ~column ~row cell =
  buffer.cells.(index_of buffer ~column ~row) <- cell
;;

let covered_columns ~column ~width =
  List.init (Int.max Int.zero (pred width)) (fun offset -> column + succ offset)
;;

(* A wide character hides the cells it spans, so they are blanked. *)
let blank_covered_cells buffer ~column ~row ~width =
  covered_columns ~column ~width
  |> List.iter (fun covered_column ->
    if Rect.contains buffer.area ~column:covered_column ~row
    then set_cell buffer ~column:covered_column ~row Cell.empty)
;;

let set_string buffer ~column ~row ~max_width ~style text =
  let limit = Int.min (Rect.right buffer.area) (column + max_width) in
  let write_cluster ~column cluster ~width =
    if Rect.contains buffer.area ~column ~row
    then (
      let written =
        Cell.patch_style
          (Cell.with_symbol (get buffer ~column ~row) cluster)
          style
      in
      set_cell buffer ~column ~row written;
      blank_covered_cells buffer ~column ~row ~width)
  in
  let rec write next_column = function
    | [] -> ()
    | cluster :: remaining ->
      let width = Grapheme.width cluster in
      if next_column + width <= limit
      then (
        if width > Int.zero
        then write_cluster ~column:next_column cluster ~width;
        write (next_column + width) remaining)
  in
  write column (Grapheme.clusters text)
;;

let set_style buffer region style =
  Rect.iter_positions
    (Rect.intersection region buffer.area)
    (fun ~column ~row ->
       let styled = Cell.patch_style (get buffer ~column ~row) style in
       set_cell buffer ~column ~row styled)
;;

let clear buffer region =
  Rect.iter_positions
    (Rect.intersection region buffer.area)
    (fun ~column ~row -> set_cell buffer ~column ~row Cell.empty)
;;

let reset buffer =
  Array.fill buffer.cells Int.zero (Array.length buffer.cells) Cell.empty
;;

let saturating_pred count = Int.max Int.zero (pred count)

type scan =
  { updates : change list
  ; cells_to_skip : int
  ; cells_invalidated : int
  }

let first_scan =
  { updates = []; cells_to_skip = Int.zero; cells_invalidated = Int.zero }
;;

let diff ~previous ~next =
  if not (Rect.equal previous.area next.area)
  then invalid_arg "Buffer.diff: buffers cover different areas";
  let scan_cell scan index =
    let current = next.cells.(index) in
    let before = previous.cells.(index) in
    let needs_update =
      (not (Cell.equal current before)) || scan.cells_invalidated > Int.zero
    in
    let updates =
      if needs_update && scan.cells_to_skip = Int.zero
      then (
        let column, row = position_of next index in
        { column; row; cell = current } :: scan.updates)
      else scan.updates
    in
    let affected_width = Int.max (Cell.width current) (Cell.width before) in
    { updates
    ; cells_to_skip = saturating_pred (Cell.width current)
    ; cells_invalidated =
        saturating_pred (Int.max affected_width scan.cells_invalidated)
    }
  in
  Seq.ints Int.zero
  |> Seq.take (Array.length next.cells)
  |> Seq.fold_left scan_cell first_scan
  |> fun scan -> List.rev scan.updates
;;

let apply buffer changes =
  let apply_change { column; row; cell } =
    if not (Rect.contains buffer.area ~column ~row)
    then invalid_arg "Buffer.apply: change outside of the buffer area";
    set_cell buffer ~column ~row cell;
    blank_covered_cells buffer ~column ~row ~width:(Cell.width cell)
  in
  List.iter apply_change changes
;;

let row_to_string buffer ~row =
  let { Rect.column = first_column; width; _ } = buffer.area in
  let add_symbol (hidden_cells, symbols) column =
    let cell = get buffer ~column ~row in
    if hidden_cells > Int.zero
    then pred hidden_cells, symbols
    else saturating_pred (Cell.width cell), cell.Cell.symbol :: symbols
  in
  List.init width (fun offset -> first_column + offset)
  |> List.fold_left add_symbol (Int.zero, [])
  |> snd
  |> List.rev
  |> String.concat ""
;;

let to_lines buffer =
  let { Rect.row = first_row; height; _ } = buffer.area in
  List.init height (fun offset ->
    row_to_string buffer ~row:(first_row + offset))
;;
