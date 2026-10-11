type t =
  { column : int
  ; row : int
  ; width : int
  ; height : int
  }

let is_negative value = value < Int.zero

let make ~column ~row ~width ~height =
  if List.exists is_negative [ column; row; width; height ]
  then invalid_arg "Rect.make: negative argument"
  else { column; row; width; height }
;;

let area rect = rect.width * rect.height
let right rect = rect.column + rect.width
let bottom rect = rect.row + rect.height
let is_empty rect = rect.width = Int.zero || rect.height = Int.zero

let contains rect ~column ~row =
  column >= rect.column
  && column < right rect
  && row >= rect.row
  && row < bottom rect
;;

let intersection first second =
  let column = Int.max first.column second.column in
  let row = Int.max first.row second.row in
  let right_edge = Int.min (right first) (right second) in
  let bottom_edge = Int.min (bottom first) (bottom second) in
  { column
  ; row
  ; width = Int.max Int.zero (right_edge - column)
  ; height = Int.max Int.zero (bottom_edge - row)
  }
;;

let shrink rect ~top ~bottom ~left ~right =
  if List.exists is_negative [ top; bottom; left; right ]
  then invalid_arg "Rect.shrink: negative margin"
  else
    { column = rect.column + left
    ; row = rect.row + top
    ; width = Int.max Int.zero (rect.width - left - right)
    ; height = Int.max Int.zero (rect.height - top - bottom)
    }
;;

let halving_divisor = 2

let centered rect ~width ~height =
  if is_negative width || is_negative height
  then invalid_arg "Rect.centered: negative size"
  else (
    let fitting_width = Int.min width rect.width in
    let fitting_height = Int.min height rect.height in
    { column = rect.column + ((rect.width - fitting_width) / halving_divisor)
    ; row = rect.row + ((rect.height - fitting_height) / halving_divisor)
    ; width = fitting_width
    ; height = fitting_height
    })
;;

let iter_positions rect visit =
  for row = rect.row to pred (bottom rect) do
    for column = rect.column to pred (right rect) do
      visit ~column ~row
    done
  done
;;

let equal (first : t) (second : t) = first = second

let pp formatter rect =
  Format.fprintf
    formatter
    "{ column = %d; row = %d; width = %d; height = %d }"
    rect.column
    rect.row
    rect.width
    rect.height
;;
