type t = Rect.t -> Buffer.t -> unit

let split ~direction parts area buffer =
  let regions =
    Layout.split ~direction ~constraints:(List.map fst parts) area
  in
  List.iter2 (fun (_, widget) region -> widget region buffer) parts regions
;;
