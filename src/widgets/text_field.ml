type t =
  { block : Block.t option
  ; style : Style.t
  ; placeholder : string option
  ; placeholder_style : Style.t
  }

let dimmed = Style.make ~modifiers:[ Style.Dim ] ()
let no_text = ""

let make
      ?block
      ?(style = Style.default)
      ?placeholder
      ?(placeholder_style = dimmed)
      ()
  =
  { block; style; placeholder; placeholder_style }
;;

let inner_area field area =
  match field.block with
  | None -> area
  | Some block -> Block.inner block area
;;

let draw_text field buffer inner edit =
  let { Rect.column; row; width; _ } = inner in
  if String.equal (Line_edit.text edit) no_text
  then
    Option.iter
      (fun placeholder ->
         Buffer.set_string
           buffer
           ~column
           ~row
           ~max_width:width
           ~style:field.placeholder_style
           placeholder)
      field.placeholder
  else
    Buffer.set_string
      buffer
      ~column
      ~row
      ~max_width:width
      ~style:Style.default
      (Line_edit.visible edit ~width)
;;

let widget field area buffer edit =
  Option.iter (fun block -> Block.widget block area buffer) field.block;
  let inner = inner_area field area in
  Buffer.set_style buffer inner field.style;
  if not (Rect.is_empty inner)
  then (
    Line_edit.reveal edit ~width:inner.Rect.width;
    draw_text field buffer inner edit)
;;

let cursor_position field edit ~area =
  let inner = inner_area field area in
  Line_edit.reveal edit ~width:inner.Rect.width;
  { Position.column = inner.Rect.column + Line_edit.cursor_column edit
  ; row = inner.Rect.row
  }
;;
