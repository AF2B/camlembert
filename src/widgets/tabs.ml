type t =
  { block : Block.t option
  ; style : Style.t
  ; highlight_style : Style.t
  ; divider : Line.t
  ; selected : int
  ; titles : Line.t list
  }

let thin_vertical_line = "\u{2502}"
let padding = Line.of_string " "

let make
      ?block
      ?(style = Style.default)
      ?(highlight_style = Style.default)
      ?(divider = Line.of_string thin_vertical_line)
      ?(selected = Int.zero)
      titles
  =
  { block; style; highlight_style; divider; selected; titles }
;;

(* What is drawn for one title: a divider if it is not the first one, then the
   title between two spaces. *)
let pieces_of_tab tabs index title =
  let title =
    if index = tabs.selected
    then Line.patch_style title tabs.highlight_style
    else title
  in
  let divider = if index = Int.zero then [] else [ tabs.divider ] in
  divider @ [ padding; title; padding ]
;;

let draw_pieces tabs buffer inner =
  let { Rect.column; row; _ } = inner in
  let right_edge = Rect.right inner in
  let draw_piece next_column piece =
    Line.render
      piece
      buffer
      ~column:next_column
      ~row
      ~max_width:(right_edge - next_column);
    next_column + Line.width piece
  in
  List.concat (List.mapi (pieces_of_tab tabs) tabs.titles)
  |> List.fold_left draw_piece column
  |> ignore
;;

let widget tabs area buffer =
  let inner =
    match tabs.block with
    | None -> area
    | Some block ->
      Block.widget block area buffer;
      Block.inner block area
  in
  Buffer.set_style buffer inner tabs.style;
  if not (Rect.is_empty inner) then draw_pieces tabs buffer inner
;;
