type t =
  | Left
  | Center
  | Right

let centering_divisor = 2

let offset alignment ~available ~content =
  let free_columns = Int.max Int.zero (available - content) in
  match alignment with
  | Left -> Int.zero
  | Center -> free_columns / centering_divisor
  | Right -> free_columns
;;
