type t =
  | Reset
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | Gray
  | Dark_gray
  | Light_red
  | Light_green
  | Light_yellow
  | Light_blue
  | Light_magenta
  | Light_cyan
  | White
  | Indexed of int
  | Rgb of
      { red : int
      ; green : int
      ; blue : int
      }

let ansi_palette =
  [ Black
  ; Red
  ; Green
  ; Yellow
  ; Blue
  ; Magenta
  ; Cyan
  ; Gray
  ; Dark_gray
  ; Light_red
  ; Light_green
  ; Light_yellow
  ; Light_blue
  ; Light_magenta
  ; Light_cyan
  ; White
  ]
;;

let equal (first : t) (second : t) = first = second

let palette_index = function
  | Indexed index -> Some index
  | Reset | Rgb _ -> None
  | named_color -> List.find_index (equal named_color) ansi_palette
;;

let to_string = function
  | Reset -> "Reset"
  | Black -> "Black"
  | Red -> "Red"
  | Green -> "Green"
  | Yellow -> "Yellow"
  | Blue -> "Blue"
  | Magenta -> "Magenta"
  | Cyan -> "Cyan"
  | Gray -> "Gray"
  | Dark_gray -> "Dark_gray"
  | Light_red -> "Light_red"
  | Light_green -> "Light_green"
  | Light_yellow -> "Light_yellow"
  | Light_blue -> "Light_blue"
  | Light_magenta -> "Light_magenta"
  | Light_cyan -> "Light_cyan"
  | White -> "White"
  | Indexed index -> Printf.sprintf "Indexed %d" index
  | Rgb { red; green; blue } -> Printf.sprintf "Rgb (%d, %d, %d)" red green blue
;;

let pp formatter color = Format.pp_print_string formatter (to_string color)
