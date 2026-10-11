type t = Rect.t -> Buffer.t -> unit

let split ?spacing ~direction parts area buffer =
  let regions =
    Layout.split ?spacing ~direction ~constraints:(List.map fst parts) area
  in
  List.iter2 (fun (_, widget) region -> widget region buffer) parts regions
;;
