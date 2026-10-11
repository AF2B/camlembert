type t =
  { mutable selected : int option
  ; mutable offset : int
  }

let create () = { selected = None; offset = Int.zero }
let selected selection = selection.selected
let select selection position = selection.selected <- position
let offset selection = selection.offset

let select_next selection ~item_count =
  let last_row = pred item_count in
  selection.selected
  <- (if item_count <= Int.zero
      then None
      else (
        match selection.selected with
        | None -> Some Int.zero
        | Some position -> Some (Int.min last_row (succ position))))
;;

let select_previous selection ~item_count =
  let last_row = pred item_count in
  selection.selected
  <- (if item_count <= Int.zero
      then None
      else (
        match selection.selected with
        | None -> Some last_row
        | Some position -> Some (Int.max Int.zero (pred position))))
;;

let narrowest_view = Int.one

let reveal selection ~item_count ~visible_rows =
  let visible_rows = Int.max narrowest_view visible_rows in
  let last_row = pred item_count in
  let highest_offset = Int.max Int.zero (item_count - visible_rows) in
  selection.selected
  <- (if item_count <= Int.zero
      then None
      else
        Option.map
          (fun position -> Int.min last_row (Int.max Int.zero position))
          selection.selected);
  let scrolled_to_selection =
    match selection.selected with
    | Some position when position < selection.offset -> position
    | Some position when position >= selection.offset + visible_rows ->
      position - visible_rows + Int.one
    | Some _ | None -> selection.offset
  in
  selection.offset
  <- Int.min highest_offset (Int.max Int.zero scrolled_to_selection)
;;
