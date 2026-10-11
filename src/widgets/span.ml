type t =
  { content : string
  ; style : Style.t
  }

let make ?(style = Style.default) content = { content; style }
let width span = Grapheme.width span.content
