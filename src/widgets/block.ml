type side =
  | Top
  | Bottom
  | Left
  | Right

type border_type =
  | Plain
  | Rounded
  | Double
  | Thick

type t =
  { borders : side list
  ; border_type : border_type
  ; border_style : Style.t
  ; style : Style.t
  ; title : Line.t option
  ; title_alignment : Alignment.t
  }

let all_sides = [ Top; Bottom; Left; Right ]

let make
      ?(borders = [])
      ?(border_type = Plain)
      ?(border_style = Style.default)
      ?(style = Style.default)
      ?title
      ?(title_alignment = Alignment.Left)
      ()
  =
  { borders; border_type; border_style; style; title; title_alignment }
;;

type border_set =
  { horizontal : string
  ; vertical : string
  ; top_left : string
  ; top_right : string
  ; bottom_left : string
  ; bottom_right : string
  }

let plain_set =
  { horizontal = "─"
  ; vertical = "│"
  ; top_left = "┌"
  ; top_right = "┐"
  ; bottom_left = "└"
  ; bottom_right = "┘"
  }
;;

let rounded_set =
  { plain_set with
    top_left = "╭"
  ; top_right = "╮"
  ; bottom_left = "╰"
  ; bottom_right = "╯"
  }
;;

let double_set =
  { horizontal = "═"
  ; vertical = "║"
  ; top_left = "╔"
  ; top_right = "╗"
  ; bottom_left = "╚"
  ; bottom_right = "╝"
  }
;;

let thick_set =
  { horizontal = "━"
  ; vertical = "┃"
  ; top_left = "┏"
  ; top_right = "┓"
  ; bottom_left = "┗"
  ; bottom_right = "┛"
  }
;;

let border_set = function
  | Plain -> plain_set
  | Rounded -> rounded_set
  | Double -> double_set
  | Thick -> thick_set
;;

let border_thickness = Int.one
let has_border block side = List.mem side block.borders
let has_title block = Option.is_some block.title

let thickness block side =
  let takes_room =
    match side with
    | Top -> has_border block Top || has_title block
    | Bottom | Left | Right -> has_border block side
  in
  if takes_room then border_thickness else Int.zero
;;

let inner block area =
  Rect.shrink
    area
    ~top:(thickness block Top)
    ~bottom:(thickness block Bottom)
    ~left:(thickness block Left)
    ~right:(thickness block Right)
;;

let edge_symbol symbols = function
  | Top | Bottom -> symbols.horizontal
  | Left | Right -> symbols.vertical
;;

(* One-cell-thick strips along each edge of the area. *)
let edge_strips area =
  let { Rect.column; row; width; height } = area in
  let last_column = pred (Rect.right area) in
  let last_row = pred (Rect.bottom area) in
  [ Top, Rect.make ~column ~row ~width ~height:border_thickness
  ; Bottom, Rect.make ~column ~row:last_row ~width ~height:border_thickness
  ; Left, Rect.make ~column ~row ~width:border_thickness ~height
  ; Right, Rect.make ~column:last_column ~row ~width:border_thickness ~height
  ]
;;

(* A corner is only drawn when both of the sides that meet there are. *)
let corners area symbols =
  let first_column = area.Rect.column in
  let first_row = area.Rect.row in
  let last_column = pred (Rect.right area) in
  let last_row = pred (Rect.bottom area) in
  [ (Top, Left), (first_column, first_row), symbols.top_left
  ; (Top, Right), (last_column, first_row), symbols.top_right
  ; (Bottom, Left), (first_column, last_row), symbols.bottom_left
  ; (Bottom, Right), (last_column, last_row), symbols.bottom_right
  ]
;;

let put_symbol block buffer ~column ~row symbol =
  Buffer.set_string
    buffer
    ~column
    ~row
    ~max_width:border_thickness
    ~style:block.border_style
    symbol
;;

let draw_borders block buffer area =
  let symbols = border_set block.border_type in
  let draw_edge (side, strip) =
    if has_border block side
    then
      Rect.iter_positions strip (fun ~column ~row ->
        put_symbol block buffer ~column ~row (edge_symbol symbols side))
  in
  let draw_corner ((first_side, second_side), (column, row), symbol) =
    if has_border block first_side && has_border block second_side
    then put_symbol block buffer ~column ~row symbol
  in
  List.iter draw_edge (edge_strips area);
  List.iter draw_corner (corners area symbols)
;;

let draw_title block buffer area =
  Option.iter
    (fun title ->
       let left = thickness block Left in
       let available =
         Int.max Int.zero (area.Rect.width - left - thickness block Right)
       in
       let offset =
         Alignment.offset
           block.title_alignment
           ~available
           ~content:(Line.width title)
       in
       Line.render
         title
         buffer
         ~column:(area.Rect.column + left + offset)
         ~row:area.Rect.row
         ~max_width:(available - offset))
    block.title
;;

let widget block area buffer =
  if not (Rect.is_empty area)
  then (
    Buffer.set_style buffer area block.style;
    draw_borders block buffer area;
    draw_title block buffer area)
;;
