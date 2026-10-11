let per_cell = 8

(* From an empty cell to a full one. *)
let lower_blocks =
  [| " "
   ; "\u{2581}"
   ; "\u{2582}"
   ; "\u{2583}"
   ; "\u{2584}"
   ; "\u{2585}"
   ; "\u{2586}"
   ; "\u{2587}"
   ; "\u{2588}"
  |]
;;

let left_blocks =
  [| " "
   ; "\u{258F}"
   ; "\u{258E}"
   ; "\u{258D}"
   ; "\u{258C}"
   ; "\u{258B}"
   ; "\u{258A}"
   ; "\u{2589}"
   ; "\u{2588}"
  |]
;;

let block_at blocks eighths =
  blocks.(Int.max Int.zero (Int.min per_cell eighths))
;;

let lower_block eighths = block_at lower_blocks eighths
let left_block eighths = block_at left_blocks eighths
let lowest_fraction = 0.0
let highest_fraction = 1.0

let filled ~fraction ~cells =
  let total = cells * per_cell in
  let fraction =
    Float.max lowest_fraction (Float.min highest_fraction fraction)
  in
  Float.to_int (Float.round (fraction *. Float.of_int total))
;;
