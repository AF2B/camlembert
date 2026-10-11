type t = Line.t list

let line_separator = '\n'

let of_string ?style content =
  String.split_on_char line_separator content
  |> List.map (Line.of_string ?style)
;;
