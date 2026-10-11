type t =
  { spans : Span.t list
  ; alignment : Alignment.t option
  }

let make ?alignment spans = { spans; alignment }

let of_string ?style ?alignment content =
  make ?alignment [ Span.make ?style content ]
;;

let width line =
  List.fold_left (fun total span -> total + Span.width span) Int.zero line.spans
;;

let render line buffer ~column ~row ~max_width =
  let draw_span used_width span =
    Buffer.set_string
      buffer
      ~column:(column + used_width)
      ~row
      ~max_width:(max_width - used_width)
      ~style:span.Span.style
      span.Span.content;
    used_width + Span.width span
  in
  ignore (List.fold_left draw_span Int.zero line.spans)
;;

let content_width = width

let render_aligned line buffer ~alignment ~column ~row ~width =
  let alignment = Option.value line.alignment ~default:alignment in
  let offset =
    Alignment.offset alignment ~available:width ~content:(content_width line)
  in
  render line buffer ~column:(column + offset) ~row ~max_width:(width - offset)
;;
