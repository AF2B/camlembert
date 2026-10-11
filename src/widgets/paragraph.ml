type t =
  { block : Block.t option
  ; style : Style.t
  ; alignment : Alignment.t
  ; wrap : Wrap.mode option
  ; scroll : int
  ; text : Text.t
  }

let make
      ?block
      ?(style = Style.default)
      ?(alignment = Alignment.Left)
      ?wrap
      ?(scroll = Int.zero)
      text
  =
  if scroll < Int.zero then invalid_arg "Paragraph.make: negative scroll";
  { block; style; alignment; wrap; scroll; text }
;;

(* The rows of the text, once long lines are broken if the paragraph wraps. *)
let rows paragraph ~width =
  match paragraph.wrap with
  | None -> paragraph.text
  | Some mode -> Wrap.text mode ~width paragraph.text
;;

let line_count paragraph ~width = List.length (rows paragraph ~width)

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
  let { Rect.row = first_row; width; height; _ } = text_area in
  let is_visible index =
    index >= paragraph.scroll && index < paragraph.scroll + height
  in
  Buffer.set_style buffer text_area paragraph.style;
  rows paragraph ~width
  |> List.filteri (fun index _ -> is_visible index)
  |> List.iteri (fun index line ->
    draw_line ~paragraph ~buffer ~text_area ~row:(first_row + index) line)
;;
