(** Helpers shared by several test suites. *)

module Buffer = Camlembert.Buffer
module Cell = Camlembert.Cell
module Rect = Camlembert.Rect

(** Writes generated edits into a buffer, in order. *)
let write_edits buffer edits =
  List.iter
    (fun { Generators.column; row; symbol; style } ->
       Buffer.set_string
         buffer
         ~column
         ~row
         ~max_width:Generators.screen_width
         ~style
         symbol)
    edits
;;

(** Builds a buffer covering the test screen from generated edits. *)
let build_buffer edits =
  let buffer = Buffer.create Generators.screen in
  write_edits buffer edits;
  buffer
;;

(** Tells whether two buffers hold the same cells everywhere in [region]. *)
let same_cells_in region first second =
  let same = ref true in
  Rect.iter_positions region (fun ~column ~row ->
    let first_cell = Buffer.get first ~column ~row in
    let second_cell = Buffer.get second ~column ~row in
    if not (Cell.equal first_cell second_cell) then same := false);
  !same
;;

(** Tells whether two buffers of the test screen hold exactly the same cells. *)
let same_cells first second = same_cells_in Generators.screen first second

(** Tells whether two buffers of the test screen hold the same cells everywhere
    except inside [hole]. *)
let same_cells_outside hole first second =
  let same = ref true in
  Rect.iter_positions Generators.screen (fun ~column ~row ->
    if not (Rect.contains hole ~column ~row)
    then (
      let first_cell = Buffer.get first ~column ~row in
      let second_cell = Buffer.get second ~column ~row in
      if not (Cell.equal first_cell second_cell) then same := false));
  !same
;;

(** Tells whether drawing a widget in [region], on a buffer holding the generated
    edits, leaves every cell outside [region] untouched. *)
let leaves_the_outside_untouched widget ~region ~edits =
  let untouched = build_buffer edits in
  let drawn = build_buffer edits in
  widget region drawn;
  same_cells_outside region untouched drawn
;;
