type 'state t = Rect.t -> Buffer.t -> 'state -> unit

let with_state widget state area buffer = widget area buffer state
