type direction =
  | Horizontal
  | Vertical

let full_percentage = 100
let equal_share = Int.one
let sum = List.fold_left ( + ) Int.zero
let non_negative = Int.max Int.zero

let is_fill = function
  | Constraint.Fill _ -> true
  | Constraint.Length _
  | Constraint.Percentage _
  | Constraint.Ratio _
  | Constraint.Min _
  | Constraint.Max _ -> false
;;

(* The size an item claims before the leftover space is shared. *)
let requested_size ~available = function
  | Constraint.Length size | Constraint.Min size | Constraint.Max size ->
    non_negative size
  | Constraint.Percentage percentage ->
    available * non_negative percentage / full_percentage
  | Constraint.Ratio { numerator; denominator } ->
    if denominator <= Int.zero
    then Int.zero
    else available * non_negative numerator / denominator
  | Constraint.Fill _ -> Int.zero
;;

(* How big a share of the leftover space an item receives. *)
let leftover_weight ~has_fill = function
  | Constraint.Fill weight -> non_negative weight
  | Constraint.Min _ -> if has_fill then Int.zero else equal_share
  | Constraint.Length _
  | Constraint.Percentage _
  | Constraint.Ratio _
  | Constraint.Max _ -> Int.zero
;;

let give_remainder_to_last_receiver ~weights ~remainder shares =
  let add_remainder (weight, share) (pending, accumulated) =
    if weight > Int.zero && pending > Int.zero
    then Int.zero, (share + pending) :: accumulated
    else pending, share :: accumulated
  in
  List.fold_right add_remainder (List.combine weights shares) (remainder, [])
  |> snd
;;

let distribute ~leftover ~weights =
  let total_weight = sum weights in
  if total_weight = Int.zero
  then List.map (Fun.const Int.zero) weights
  else (
    let shares =
      List.map (fun weight -> leftover * weight / total_weight) weights
    in
    give_remainder_to_last_receiver
      ~weights
      ~remainder:(leftover - sum shares)
      shares)
;;

let shrink_from_end sizes ~overflow =
  let take_back size (remaining_overflow, accumulated) =
    let removed = Int.min size remaining_overflow in
    remaining_overflow - removed, (size - removed) :: accumulated
  in
  List.fold_right take_back sizes (overflow, []) |> snd
;;

let sizes ~available constraints =
  let requested = List.map (requested_size ~available) constraints in
  let requested_total = sum requested in
  if requested_total > available
  then shrink_from_end requested ~overflow:(requested_total - available)
  else (
    let has_fill = List.exists is_fill constraints in
    let weights = List.map (leftover_weight ~has_fill) constraints in
    let extra = distribute ~leftover:(available - requested_total) ~weights in
    List.map2 ( + ) requested extra)
;;

let place ~direction area sizes =
  let { Rect.column; row; width; height } = area in
  let rect_at ~offset ~size =
    match direction with
    | Horizontal -> Rect.make ~column:(column + offset) ~row ~width:size ~height
    | Vertical -> Rect.make ~column ~row:(row + offset) ~width ~height:size
  in
  List.fold_left_map
    (fun offset size -> offset + size, rect_at ~offset ~size)
    Int.zero
    sizes
  |> snd
;;

let split ~direction ~constraints area =
  let { Rect.width; height; _ } = area in
  let available =
    match direction with
    | Horizontal -> width
    | Vertical -> height
  in
  sizes ~available constraints |> place ~direction area
;;

let horizontal constraints area = split ~direction:Horizontal ~constraints area
let vertical constraints area = split ~direction:Vertical ~constraints area
