type modifier =
  | Bold
  | Dim
  | Italic
  | Underlined
  | Slow_blink
  | Rapid_blink
  | Reversed
  | Hidden
  | Crossed_out

module Modifier_set = Set.Make (struct
    type t = modifier

    let compare = Stdlib.compare
  end)

type t =
  { foreground : Color.t option
  ; background : Color.t option
  ; modifiers : Modifier_set.t
  }

let default =
  { foreground = None; background = None; modifiers = Modifier_set.empty }
;;

let make ?foreground ?background ?(modifiers = []) () =
  { foreground; background; modifiers = Modifier_set.of_list modifiers }
;;

let prefer_overlay ~overlay ~base =
  match overlay with
  | Some _ -> overlay
  | None -> base
;;

let patch base overlay =
  { foreground =
      prefer_overlay ~overlay:overlay.foreground ~base:base.foreground
  ; background =
      prefer_overlay ~overlay:overlay.background ~base:base.background
  ; modifiers = Modifier_set.union base.modifiers overlay.modifiers
  }
;;

let equal first second =
  Option.equal Color.equal first.foreground second.foreground
  && Option.equal Color.equal first.background second.background
  && Modifier_set.equal first.modifiers second.modifiers
;;

let modifier_name = function
  | Bold -> "Bold"
  | Dim -> "Dim"
  | Italic -> "Italic"
  | Underlined -> "Underlined"
  | Slow_blink -> "Slow_blink"
  | Rapid_blink -> "Rapid_blink"
  | Reversed -> "Reversed"
  | Hidden -> "Hidden"
  | Crossed_out -> "Crossed_out"
;;

let pp_optional_color =
  Format.pp_print_option
    ~none:(fun formatter () -> Format.pp_print_string formatter "None")
    Color.pp
;;

let pp formatter style =
  let modifier_names =
    Modifier_set.elements style.modifiers
    |> List.map modifier_name
    |> String.concat "; "
  in
  Format.fprintf
    formatter
    "{ foreground = %a; background = %a; modifiers = [%s] }"
    pp_optional_color
    style.foreground
    pp_optional_color
    style.background
    modifier_names
;;
