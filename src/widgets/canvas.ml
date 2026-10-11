type shape =
  | Points of
      { coordinates : (float * float) list
      ; style : Style.t
      }
  | Segment of
      { x1 : float
      ; y1 : float
      ; x2 : float
      ; y2 : float
      ; style : Style.t
      }
  | Rectangle of
      { x : float
      ; y : float
      ; width : float
      ; height : float
      ; style : Style.t
      }
  | Circle of
      { x : float
      ; y : float
      ; radius : float
      ; style : Style.t
      }

type label =
  { x : float
  ; y : float
  ; text : Line.t
  }

type t =
  { block : Block.t option
  ; style : Style.t
  ; marker : Marker.t
  ; labels : label list
  ; x_bounds : float * float
  ; y_bounds : float * float
  ; shapes : shape list
  }

let is_valid_range (lowest, highest) =
  Float.is_finite lowest && Float.is_finite highest && lowest < highest
;;

let make
      ?block
      ?(style = Style.default)
      ?(marker = Marker.Braille)
      ?(labels = [])
      ~x_bounds
      ~y_bounds
      shapes
  =
  if not (is_valid_range x_bounds && is_valid_range y_bounds)
  then invalid_arg "Canvas.make: bounds must be finite, the lowest one first";
  { block; style; marker; labels; x_bounds; y_bounds; shapes }
;;

(* The dots of the canvas, as a grid, and the style of each cell, which is the
   style of the last dot painted in it. *)
type grid =
  { marker : Marker.t
  ; cells_wide : int
  ; cells_tall : int
  ; dots_wide : int
  ; dots_tall : int
  ; dots : bool array
  ; cell_styles : Style.t option array
  }

let create_grid marker ~cells_wide ~cells_tall =
  let dots_per_column, dots_per_row = Marker.dots_per_cell marker in
  let dots_wide = cells_wide * dots_per_column in
  let dots_tall = cells_tall * dots_per_row in
  { marker
  ; cells_wide
  ; cells_tall
  ; dots_wide
  ; dots_tall
  ; dots = Array.make (dots_wide * dots_tall) false
  ; cell_styles = Array.make (cells_wide * cells_tall) None
  }
;;

let paint grid ~column ~row style =
  let dots_per_column, dots_per_row = Marker.dots_per_cell grid.marker in
  let cell =
    (row / dots_per_row * grid.cells_wide) + (column / dots_per_column)
  in
  grid.dots.((row * grid.dots_wide) + column) <- true;
  grid.cell_styles.(cell) <- Some style
;;

(* Where a value is between two others, as a step among [steps], the first one
   being at [low] and the last one at [high]. *)
let step_of ~low ~high ~steps value =
  let last_step = pred steps in
  let step =
    Float.to_int
      (Float.round ((value -. low) /. (high -. low) *. Float.of_int last_step))
  in
  Int.max Int.zero (Int.min last_step step)
;;

(* The dot a point falls on. Rows count from the top, while y grows upwards. *)
let dot_at canvas grid (x, y) =
  let x_low, x_high = canvas.x_bounds in
  let y_low, y_high = canvas.y_bounds in
  ( step_of ~low:x_low ~high:x_high ~steps:grid.dots_wide x
  , step_of ~low:y_high ~high:y_low ~steps:grid.dots_tall y )
;;

let is_inside canvas (x, y) =
  let x_low, x_high = canvas.x_bounds in
  let y_low, y_high = canvas.y_bounds in
  x >= x_low && x <= x_high && y >= y_low && y <= y_high
;;

let has_dots grid = grid.dots_wide > Int.zero && grid.dots_tall > Int.zero

let paint_point canvas grid style point =
  if has_dots grid && is_inside canvas point
  then (
    let column, row = dot_at canvas grid point in
    paint grid ~column ~row style)
;;

(* The dots of a line from one dot to another, by Bresenham's algorithm. *)
let dots_between (column_from, row_from) (column_to, row_to) =
  let across = abs (column_to - column_from) in
  let down = Int.neg (abs (row_to - row_from)) in
  let direction from towards =
    if from < towards then Int.one else Int.neg Int.one
  in
  let step_column = direction column_from column_to in
  let step_row = direction row_from row_to in
  let rec walk column row error dots =
    let dots = (column, row) :: dots in
    if column = column_to && row = row_to
    then List.rev dots
    else (
      let doubled = error + error in
      let error, column =
        if doubled >= down
        then error + down, column + step_column
        else error, column
      in
      let error, row =
        if doubled <= across then error + across, row + step_row else error, row
      in
      walk column row error dots)
  in
  walk column_from row_from (across + down) []
;;

let no_direction = 0.0
let segment_start = 0.0
let segment_end = 1.0

(* Cuts a segment to the part inside the bounds (Liang and Barsky), so that a
   segment reaching far outside is not walked dot by dot. *)
let clip_segment canvas (x1, y1) (x2, y2) =
  let x_low, x_high = canvas.x_bounds in
  let y_low, y_high = canvas.y_bounds in
  let delta_x = x2 -. x1 in
  let delta_y = y2 -. y1 in
  let edges =
    [ Float.neg delta_x, x1 -. x_low
    ; delta_x, x_high -. x1
    ; Float.neg delta_y, y1 -. y_low
    ; delta_y, y_high -. y1
    ]
  in
  let narrow window (direction, distance) =
    match window with
    | None -> None
    | Some (entering, leaving) ->
      if direction = no_direction
      then if distance < no_direction then None else window
      else (
        let ratio = distance /. direction in
        if direction < no_direction
        then
          if ratio > leaving
          then None
          else Some (Float.max entering ratio, leaving)
        else if ratio < entering
        then None
        else Some (entering, Float.min leaving ratio))
  in
  List.fold_left narrow (Some (segment_start, segment_end)) edges
  |> Option.map (fun (entering, leaving) ->
    ( (x1 +. (entering *. delta_x), y1 +. (entering *. delta_y))
    , (x1 +. (leaving *. delta_x), y1 +. (leaving *. delta_y)) ))
;;

let paint_segment canvas grid style from towards =
  if has_dots grid
  then
    clip_segment canvas from towards
    |> Option.iter (fun (clipped_from, clipped_to) ->
      dots_between
        (dot_at canvas grid clipped_from)
        (dot_at canvas grid clipped_to)
      |> List.iter (fun (column, row) -> paint grid ~column ~row style))
;;

let paint_rectangle canvas grid style ~x ~y ~width ~height =
  let right = x +. width in
  let top = y +. height in
  List.iter
    (fun (from, towards) -> paint_segment canvas grid style from towards)
    [ (x, y), (right, y)
    ; (right, y), (right, top)
    ; (right, top), (x, top)
    ; (x, top), (x, y)
    ]
;;

let full_turn = 2.0 *. Float.pi
let samples_per_dot = 2.0
let fewest_samples = 8

(* A circle larger than any terminal is drawn with gaps rather than slowly. *)
let most_samples = 100_000

let overlaps_bounds canvas ~x ~y ~radius =
  let x_low, x_high = canvas.x_bounds in
  let y_low, y_high = canvas.y_bounds in
  x +. radius >= x_low
  && x -. radius <= x_high
  && y +. radius >= y_low
  && y -. radius <= y_high
;;

(* Walks around the circle with enough steps that no dot is skipped. *)
let paint_circle canvas grid style ~x ~y ~radius =
  let x_low, x_high = canvas.x_bounds in
  let y_low, y_high = canvas.y_bounds in
  let radius_in_dots =
    Float.max
      (radius /. (x_high -. x_low) *. Float.of_int grid.dots_wide)
      (radius /. (y_high -. y_low) *. Float.of_int grid.dots_tall)
  in
  if
    has_dots grid
    && Float.is_finite radius_in_dots
    && overlaps_bounds canvas ~x ~y ~radius
  then (
    let samples =
      Int.min
        most_samples
        (Int.max
           fewest_samples
           (Float.to_int
              (Float.ceil (full_turn *. radius_in_dots *. samples_per_dot))))
    in
    List.init samples (fun sample ->
      let angle = full_turn *. Float.of_int sample /. Float.of_int samples in
      x +. (radius *. cos angle), y +. (radius *. sin angle))
    |> List.iter (paint_point canvas grid style))
;;

let paint_shape canvas grid = function
  | Points { coordinates; style } ->
    List.iter (paint_point canvas grid style) coordinates
  | Segment { x1; y1; x2; y2; style } ->
    paint_segment canvas grid style (x1, y1) (x2, y2)
  | Rectangle { x; y; width; height; style } ->
    paint_rectangle canvas grid style ~x ~y ~width ~height
  | Circle { x; y; radius; style } ->
    paint_circle canvas grid style ~x ~y ~radius
;;

let draw_cells grid buffer inner =
  let dots_per_column, dots_per_row = Marker.dots_per_cell grid.marker in
  let draw_cell index =
    Option.iter
      (fun style ->
         let cell_column = index mod grid.cells_wide in
         let cell_row = index / grid.cells_wide in
         let painted ~column ~row =
           grid.dots.((((cell_row * dots_per_row) + row) * grid.dots_wide)
                      + (cell_column * dots_per_column)
                      + column)
         in
         Buffer.set_string
           buffer
           ~column:(inner.Rect.column + cell_column)
           ~row:(inner.Rect.row + cell_row)
           ~max_width:Int.one
           ~style
           (Marker.symbol grid.marker ~painted))
      grid.cell_styles.(index)
  in
  List.init (Array.length grid.cell_styles) Fun.id |> List.iter draw_cell
;;

let draw_label canvas buffer inner label =
  if is_inside canvas (label.x, label.y)
  then (
    let x_low, x_high = canvas.x_bounds in
    let y_low, y_high = canvas.y_bounds in
    let column =
      inner.Rect.column
      + step_of ~low:x_low ~high:x_high ~steps:inner.Rect.width label.x
    in
    let row =
      inner.Rect.row
      + step_of ~low:y_high ~high:y_low ~steps:inner.Rect.height label.y
    in
    Line.render
      label.text
      buffer
      ~column
      ~row
      ~max_width:(Rect.right inner - column))
;;

let widget canvas area buffer =
  let inner =
    match canvas.block with
    | None -> area
    | Some block ->
      Block.widget block area buffer;
      Block.inner block area
  in
  Buffer.set_style buffer inner canvas.style;
  if not (Rect.is_empty inner)
  then (
    let grid =
      create_grid
        canvas.marker
        ~cells_wide:inner.Rect.width
        ~cells_tall:inner.Rect.height
    in
    List.iter (paint_shape canvas grid) canvas.shapes;
    draw_cells grid buffer inner;
    List.iter (draw_label canvas buffer inner) canvas.labels)
;;
