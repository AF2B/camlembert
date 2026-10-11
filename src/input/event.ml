type key =
  | Char of string
  | Enter
  | Tab
  | Backtab
  | Backspace
  | Escape
  | Delete
  | Insert
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | Function of int

type button =
  | Left_button
  | Middle_button
  | Right_button

type mouse_action =
  | Press of button
  | Release of button
  | Drag of button
  | Scroll_up
  | Scroll_down
  | Scroll_left
  | Scroll_right

type modifiers =
  { shift : bool
  ; control : bool
  ; alt : bool
  }

type t =
  | Key of
      { key : key
      ; modifiers : modifiers
      }
  | Resize of
      { width : int
      ; height : int
      }
  | Paste of string
  | Mouse of
      { action : mouse_action
      ; position : Position.t
      ; modifiers : modifiers
      }

let no_modifiers = { shift = false; control = false; alt = false }
let key_press ?(modifiers = no_modifiers) key = Key { key; modifiers }
let equal (first : t) (second : t) = first = second

let key_name = function
  | Char text -> Printf.sprintf "Char %S" text
  | Enter -> "Enter"
  | Tab -> "Tab"
  | Backtab -> "Backtab"
  | Backspace -> "Backspace"
  | Escape -> "Escape"
  | Delete -> "Delete"
  | Insert -> "Insert"
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | Home -> "Home"
  | End -> "End"
  | Page_up -> "Page_up"
  | Page_down -> "Page_down"
  | Function number -> Printf.sprintf "Function %d" number
;;

let button_name = function
  | Left_button -> "Left_button"
  | Middle_button -> "Middle_button"
  | Right_button -> "Right_button"
;;

let action_name = function
  | Press button -> Printf.sprintf "Press %s" (button_name button)
  | Release button -> Printf.sprintf "Release %s" (button_name button)
  | Drag button -> Printf.sprintf "Drag %s" (button_name button)
  | Scroll_up -> "Scroll_up"
  | Scroll_down -> "Scroll_down"
  | Scroll_left -> "Scroll_left"
  | Scroll_right -> "Scroll_right"
;;

let held_modifier_names { shift; control; alt } =
  [ shift, "shift"; control, "control"; alt, "alt" ]
  |> List.filter_map (fun (held, name) -> if held then Some name else None)
;;

let pp formatter = function
  | Key { key; modifiers } ->
    Format.fprintf
      formatter
      "Key { key = %s; modifiers = [%s] }"
      (key_name key)
      (String.concat "; " (held_modifier_names modifiers))
  | Resize { width; height } ->
    Format.fprintf formatter "Resize { width = %d; height = %d }" width height
  | Paste pasted -> Format.fprintf formatter "Paste %S" pasted
  | Mouse { action; position; modifiers } ->
    Format.fprintf
      formatter
      "Mouse { action = %s; column = %d; row = %d; modifiers = [%s] }"
      (action_name action)
      position.Position.column
      position.Position.row
      (String.concat "; " (held_modifier_names modifiers))
;;
