type t =
  { block : Block.t option
  ; style : Style.t
  ; alignment : Alignment.t
  ; text : Text.t
  }

let make ?block ?(style = Style.default) ?(alignment = Alignment.Left) text =
  { block; style; alignment; text }
;;

(* Draws the frame, if any, and tells where the text goes. *)
let draw_block paragraph area buffer =
  match paragraph.block with
  | None -> area
  | Some block ->
    Block.widget block area buffer;
    Block.inner block area
;;

let draw_line ~paragraph ~buffer ~text_area ~row line =
  let { Rect.column; width; _ } = text_area in
  let alignment =
    Option.value line.Line.alignment ~default:paragraph.alignment
  in
  let offset =
    Alignment.offset alignment ~available:width ~content:(Line.width line)
  in
  Line.render
    line
    buffer
    ~column:(column + offset)
    ~row
    ~max_width:(width - offset)
;;

let widget paragraph area buffer =
  let text_area = draw_block paragraph area buffer in
  let { Rect.row = first_row; height; _ } = text_area in
  Buffer.set_style buffer text_area paragraph.style;
  paragraph.text
  |> List.filteri (fun index _ -> index < height)
  |> List.iteri (fun index line ->
    draw_line ~paragraph ~buffer ~text_area ~row:(first_row + index) line)
;;
