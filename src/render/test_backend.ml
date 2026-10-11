type t =
  { mutable screen : Buffer.t
  ; mutable drawn_cells : int
  }

let origin = 0

let blank_screen ~width ~height =
  Buffer.create (Rect.make ~column:origin ~row:origin ~width ~height)
;;

let create ~width ~height =
  { screen = blank_screen ~width ~height; drawn_cells = Int.zero }
;;

let backend test_backend =
  { Backend.size = (fun () -> Buffer.area test_backend.screen)
  ; draw =
      (fun changes ->
        Buffer.apply test_backend.screen changes;
        test_backend.drawn_cells
        <- test_backend.drawn_cells + List.length changes)
  ; clear = (fun () -> Buffer.reset test_backend.screen)
  ; flush = ignore
  }
;;

let screen test_backend = test_backend.screen
let to_lines test_backend = Buffer.to_lines test_backend.screen
let drawn_cells test_backend = test_backend.drawn_cells

let resize test_backend ~width ~height =
  test_backend.screen <- blank_screen ~width ~height
;;
