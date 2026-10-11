type t =
  { symbol : string
  ; style : Style.t
  }

let blank_symbol = " "
let empty = { symbol = blank_symbol; style = Style.default }
let make ?(style = Style.default) symbol = { symbol; style }
let with_symbol cell symbol = { cell with symbol }
let patch_style cell style = { cell with style = Style.patch cell.style style }
let width cell = Grapheme.width cell.symbol

let equal first second =
  String.equal first.symbol second.symbol
  && Style.equal first.style second.style
;;

let pp formatter cell =
  Format.fprintf
    formatter
    "{ symbol = %S; style = %a }"
    cell.symbol
    Style.pp
    cell.style
;;
