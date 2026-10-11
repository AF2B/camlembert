type graph_type =
  | Scatter
  | Connected

type legend_position =
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | Hidden

type axis =
  { title : string option
  ; labels : string list
  ; style : Style.t
  ; bounds : float * float
  }

type dataset =
  { name : string
  ; data : (float * float) list
  ; marker : Marker.t
  ; graph_type : graph_type
  ; style : Style.t
  }

type t =
  { block : Block.t option
  ; style : Style.t
  ; legend : legend_position
  ; x_axis : axis
  ; y_axis : axis
  ; datasets : dataset list
  }

let is_valid_range (lowest, highest) =
  Float.is_finite lowest && Float.is_finite highest && lowest < highest
;;

let axis ?title ?(labels = []) ?(style = Style.default) ~bounds () =
  if not (is_valid_range bounds)
  then invalid_arg "Chart.axis: bounds must be finite, the lowest one first";
  { title; labels; style; bounds }
;;

let dataset
      ?(marker = Marker.Braille)
      ?(graph_type = Scatter)
      ?(style = Style.default)
      ~name
      data
  =
  { name; data; marker; graph_type; style }
;;

let make
      ?block
      ?(style = Style.default)
      ?(legend = Top_right)
      ~x_axis
      ~y_axis
      datasets
  =
  { block; style; legend; x_axis; y_axis; datasets }
;;

(* Symbols of the axes. *)
let vertical_line = "\u{2502}"
let horizontal_line = "\u{2500}"
let corner = "\u{2514}"
let axis_thickness = Int.one
let title_rows = Int.one
let halving_divisor = 2
let legend_share = 3
let no_name = ""

(* The region the chart fills, and the part of it where the datasets go. *)
type layout =
  { area : Rect.t
  ; graph : Rect.t
  }

let widest_label axis =
  List.fold_left
    (fun widest label -> Int.max widest (Grapheme.width label))
    Int.zero
    axis.labels
;;

let rows_for_labels axis = if axis.labels = [] then Int.zero else Int.one

let rows_for_title axis =
  if Option.is_some axis.title then title_rows else Int.zero
;;

(* Takes from [area] the room of the axes, their labels and their titles. *)
let layout_of chart area =
  let { Rect.column; row; width; height } = area in
  let graph_left = column + widest_label chart.y_axis + axis_thickness in
  let graph_top = row + rows_for_title chart.y_axis in
  let graph_right = column + width in
  let graph_bottom =
    row
    + height
    - rows_for_title chart.x_axis
    - rows_for_labels chart.x_axis
    - axis_thickness
  in
  if graph_right > graph_left && graph_bottom > graph_top
  then
    Some
      { area
      ; graph =
          Rect.make
            ~column:graph_left
            ~row:graph_top
            ~width:(graph_right - graph_left)
            ~height:(graph_bottom - graph_top)
      }
  else None
;;

let draw_symbol buffer ~column ~row ~style symbol =
  Buffer.set_string buffer ~column ~row ~max_width:Int.one ~style symbol
;;

let draw_axes chart buffer layout =
  let { Rect.column; row; width; height } = layout.graph in
  let y_axis_column = pred column in
  let x_axis_row = row + height in
  List.init height (fun offset -> row + offset)
  |> List.iter (fun current_row ->
    draw_symbol
      buffer
      ~column:y_axis_column
      ~row:current_row
      ~style:chart.y_axis.style
      vertical_line);
  draw_symbol
    buffer
    ~column:y_axis_column
    ~row:x_axis_row
    ~style:chart.x_axis.style
    corner;
  List.init width (fun offset -> column + offset)
  |> List.iter (fun current_column ->
    draw_symbol
      buffer
      ~column:current_column
      ~row:x_axis_row
      ~style:chart.x_axis.style
      horizontal_line)
;;

(* How far from the start of [room] cells an item of [size] cells stands when
   [count] items are spread evenly, the first one flush with the start and the
   last one flush with the end. A single item sits in the middle. *)
let spread_offset ~count ~room ~size index =
  let free_cells = Int.max Int.zero (room - size) in
  if count <= Int.one
  then free_cells / halving_divisor
  else
    Float.to_int
      (Float.round
         (Float.of_int (index * free_cells) /. Float.of_int (pred count)))
;;

let draw_x_labels chart buffer layout =
  let { Rect.column; row; width; height } = layout.graph in
  let labels_row = row + height + axis_thickness in
  let count = List.length chart.x_axis.labels in
  List.iteri
    (fun index label ->
       let start =
         column
         + spread_offset ~count ~room:width ~size:(Grapheme.width label) index
       in
       Buffer.set_string
         buffer
         ~column:start
         ~row:labels_row
         ~max_width:(column + width - start)
         ~style:chart.x_axis.style
         label)
    chart.x_axis.labels
;;

(* The labels are placed counting from the top, the way [Canvas] places points,
   so a label and a point with the same value fall on the same row. *)
let draw_y_labels chart buffer layout =
  let { Rect.row; height; _ } = layout.graph in
  let labels_width = widest_label chart.y_axis in
  let count = List.length chart.y_axis.labels in
  List.iteri
    (fun index label ->
       Buffer.set_string
         buffer
         ~column:(layout.area.Rect.column + labels_width - Grapheme.width label)
         ~row:
           (row
            + spread_offset
                ~count
                ~room:height
                ~size:Int.one
                (pred count - index))
         ~max_width:labels_width
         ~style:chart.y_axis.style
         label)
    chart.y_axis.labels
;;

(* The title of the y axis goes above the axis, the one of the x axis below its
   labels and at its end. *)
let draw_titles chart buffer layout =
  let { Rect.column; row; width; _ } = layout.area in
  let { Rect.column = graph_column; width = graph_width; _ } = layout.graph in
  Option.iter
    (fun title ->
       Buffer.set_string
         buffer
         ~column
         ~row
         ~max_width:width
         ~style:chart.y_axis.style
         title)
    chart.y_axis.title;
  Option.iter
    (fun title ->
       let start =
         Int.max graph_column (graph_column + graph_width - Grapheme.width title)
       in
       Buffer.set_string
         buffer
         ~column:start
         ~row:(Rect.bottom layout.area - title_rows)
         ~max_width:(graph_column + graph_width - start)
         ~style:chart.x_axis.style
         title)
    chart.x_axis.title
;;

(* One segment from each point to the next one. *)
let segments_of dataset =
  let points = List.to_seq dataset.data in
  Seq.zip points (Seq.drop Int.one points)
  |> Seq.map (fun ((x1, y1), (x2, y2)) ->
    Canvas.Segment { x1; y1; x2; y2; style = dataset.style })
  |> List.of_seq
;;

let shapes_of dataset =
  let points =
    Canvas.Points { coordinates = dataset.data; style = dataset.style }
  in
  match dataset.graph_type with
  | Scatter -> [ points ]
  | Connected -> points :: segments_of dataset
;;

let draw_datasets chart buffer layout =
  List.iter
    (fun dataset ->
       Canvas.widget
         (Canvas.make
            ~marker:dataset.marker
            ~x_bounds:chart.x_axis.bounds
            ~y_bounds:chart.y_axis.bounds
            (shapes_of dataset))
         layout.graph
         buffer)
    chart.datasets
;;

(* The upper left cell of the legend, if the position shows a legend. *)
let legend_origin position graph ~legend_width ~legend_height =
  let { Rect.column; row; width; height } = graph in
  let left = column in
  let right = column + width - legend_width in
  let top = row in
  let bottom = row + height - legend_height in
  match position with
  | Top_left -> Some (left, top)
  | Top_right -> Some (right, top)
  | Bottom_left -> Some (left, bottom)
  | Bottom_right -> Some (right, bottom)
  | Hidden -> None
;;

let draw_legend chart buffer layout =
  let named =
    List.filter (fun dataset -> dataset.name <> no_name) chart.datasets
  in
  let { Rect.width = graph_width; height = graph_height; _ } = layout.graph in
  let legend_width =
    List.fold_left
      (fun widest dataset -> Int.max widest (Grapheme.width dataset.name))
      Int.zero
      named
  in
  let legend_height = List.length named in
  let fits =
    legend_height > Int.zero
    && legend_width * legend_share <= graph_width
    && legend_height * legend_share <= graph_height
  in
  match
    legend_origin chart.legend layout.graph ~legend_width ~legend_height
  with
  | Some (column, row) when fits ->
    let legend_area =
      Rect.make ~column ~row ~width:legend_width ~height:legend_height
    in
    Buffer.clear buffer legend_area;
    Buffer.set_style buffer legend_area chart.style;
    List.iteri
      (fun index (dataset : dataset) ->
         Buffer.set_string
           buffer
           ~column
           ~row:(row + index)
           ~max_width:legend_width
           ~style:dataset.style
           dataset.name)
      named
  | Some _ | None -> ()
;;

let widget chart area buffer =
  let inner =
    match chart.block with
    | None -> area
    | Some block ->
      Block.widget block area buffer;
      Block.inner block area
  in
  Buffer.set_style buffer inner chart.style;
  Option.iter
    (fun layout ->
       draw_axes chart buffer layout;
       draw_x_labels chart buffer layout;
       draw_y_labels chart buffer layout;
       draw_titles chart buffer layout;
       draw_datasets chart buffer layout;
       draw_legend chart buffer layout)
    (layout_of chart inner)
;;
