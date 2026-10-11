module Constraint = Camlembert.Constraint
module Layout = Camlembert.Layout
module Rect = Camlembert.Rect

let origin = 0
let cross_axis_size = 4
let no_minimum = 0
let no_cells = 0
let single_weight = 1
let double_weight = 2
let one_term = 1
let two_terms = 2
let thirds = 3
let no_denominator = 0
let rect_testable = Alcotest.testable Rect.pp Rect.equal
let region ~width ~height = Rect.make ~column:origin ~row:origin ~width ~height

(* A region [length] cells wide, for layouts that run left to right. *)
let wide_region length = region ~width:length ~height:cross_axis_size

(* A region [length] cells tall, for layouts that run top to bottom. *)
let tall_region length = region ~width:cross_axis_size ~height:length
let widths rects = List.map (fun rect -> rect.Rect.width) rects
let heights rects = List.map (fun rect -> rect.Rect.height) rects

let split_widths ~region_width constraints =
  widths (Layout.horizontal constraints (wide_region region_width))
;;

let test_header_body_footer () =
  let total_rows = 10 in
  let header_rows = 3 in
  let footer_rows = 1 in
  let body_rows = total_rows - header_rows - footer_rows in
  let rects =
    Layout.vertical
      [ Constraint.Length header_rows
      ; Constraint.Min no_minimum
      ; Constraint.Length footer_rows
      ]
      (tall_region total_rows)
  in
  Alcotest.(check (list int))
    "heights"
    [ header_rows; body_rows; footer_rows ]
    (heights rects)
;;

let test_percentages () =
  let region_width = 100 in
  let sidebar_share = 30 in
  let content_share = 70 in
  Alcotest.(check (list int))
    "widths"
    [ sidebar_share; content_share ]
    (split_widths
       ~region_width
       [ Constraint.Percentage sidebar_share
       ; Constraint.Percentage content_share
       ])
;;

let test_ratios () =
  let one_third = 30 in
  let two_thirds = 60 in
  let region_width = one_third + two_thirds in
  Alcotest.(check (list int))
    "widths"
    [ one_third; two_thirds ]
    (split_widths
       ~region_width
       [ Constraint.Ratio { numerator = one_term; denominator = thirds }
       ; Constraint.Ratio { numerator = two_terms; denominator = thirds }
       ])
;;

let test_ratio_without_a_denominator_takes_nothing () =
  let region_width = 10 in
  Alcotest.(check (list int))
    "widths"
    [ no_cells; region_width ]
    (split_widths
       ~region_width
       [ Constraint.Ratio { numerator = one_term; denominator = no_denominator }
       ; Constraint.Fill single_weight
       ])
;;

let test_fill_shares_the_leftover_by_weight () =
  let light_share = 30 in
  let heavy_share = 60 in
  let region_width = light_share + heavy_share in
  Alcotest.(check (list int))
    "widths"
    [ light_share; heavy_share ]
    (split_widths
       ~region_width
       [ Constraint.Fill single_weight; Constraint.Fill double_weight ])
;;

let test_rounding_leftover_goes_to_the_last_item () =
  let smaller_half = 5 in
  let larger_half = 6 in
  let region_width = smaller_half + larger_half in
  Alcotest.(check (list int))
    "widths"
    [ smaller_half; larger_half ]
    (split_widths
       ~region_width
       [ Constraint.Fill single_weight; Constraint.Fill single_weight ])
;;

let test_min_items_grow_when_there_is_no_fill () =
  let sidebar = 20 in
  let panel_minimum = 10 in
  let region_width = 100 in
  let grown_panel = (region_width - sidebar) / 2 in
  Alcotest.(check (list int))
    "widths"
    [ sidebar; grown_panel; grown_panel ]
    (split_widths
       ~region_width
       [ Constraint.Length sidebar
       ; Constraint.Min panel_minimum
       ; Constraint.Min panel_minimum
       ])
;;

let test_fill_takes_the_leftover_before_min () =
  let sidebar_minimum = 20 in
  let region_width = 100 in
  Alcotest.(check (list int))
    "widths"
    [ sidebar_minimum; region_width - sidebar_minimum ]
    (split_widths
       ~region_width
       [ Constraint.Min sidebar_minimum; Constraint.Fill single_weight ])
;;

let test_max_is_a_ceiling_not_a_target () =
  let toolbar_maximum = 3 in
  let region_width = 10 in
  Alcotest.(check (list int))
    "widths"
    [ toolbar_maximum; region_width - toolbar_maximum ]
    (split_widths
       ~region_width
       [ Constraint.Max toolbar_maximum; Constraint.Fill single_weight ])
;;

let test_leftover_stays_unused_without_fill_or_min () =
  let first_length = 20 in
  let second_length = 30 in
  let region_width = 100 in
  Alcotest.(check (list int))
    "widths"
    [ first_length; second_length ]
    (split_widths
       ~region_width
       [ Constraint.Length first_length; Constraint.Length second_length ])
;;

let test_last_items_shrink_first_when_space_is_short () =
  let item_length = 6 in
  let region_width = 8 in
  let fitting_remainder = region_width - item_length in
  Alcotest.(check (list int))
    "widths"
    [ item_length; fitting_remainder; no_cells ]
    (split_widths
       ~region_width
       [ Constraint.Length item_length
       ; Constraint.Length item_length
       ; Constraint.Length item_length
       ])
;;

let test_no_constraints_give_no_regions () =
  Alcotest.(check (list rect_testable))
    "regions"
    []
    (Layout.horizontal [] (wide_region 10))
;;

let test_regions_follow_each_other_inside_the_area () =
  let area_column = 5 in
  let area_row = 2 in
  let area_width = 7 in
  let area_height = 10 in
  let header_height = 3 in
  let area =
    Rect.make
      ~column:area_column
      ~row:area_row
      ~width:area_width
      ~height:area_height
  in
  let expected =
    [ Rect.make
        ~column:area_column
        ~row:area_row
        ~width:area_width
        ~height:header_height
    ; Rect.make
        ~column:area_column
        ~row:(area_row + header_height)
        ~width:area_width
        ~height:(area_height - header_height)
    ]
  in
  Alcotest.(check (list rect_testable))
    "regions"
    expected
    (Layout.vertical
       [ Constraint.Length header_height; Constraint.Fill single_weight ]
       area)
;;

let extent ~direction rect =
  match direction with
  | Layout.Horizontal -> rect.Rect.width
  | Layout.Vertical -> rect.Rect.height
;;

let start ~direction rect =
  match direction with
  | Layout.Horizontal -> rect.Rect.column
  | Layout.Vertical -> rect.Rect.row
;;

let same_cross_axis ~direction first second =
  match direction with
  | Layout.Horizontal ->
    first.Rect.row = second.Rect.row && first.Rect.height = second.Rect.height
  | Layout.Vertical ->
    first.Rect.column = second.Rect.column
    && first.Rect.width = second.Rect.width
;;

(* Tells whether the regions sit one after another, from the start of the area. *)
let tile_the_area ~direction area rects =
  let step (expected_start, tiled) rect =
    ( expected_start + extent ~direction rect
    , tiled
      && start ~direction rect = expected_start
      && same_cross_axis ~direction area rect )
  in
  List.fold_left step (start ~direction area, true) rects |> snd
;;

let sum = List.fold_left ( + ) Int.zero

let print_direction = function
  | Layout.Horizontal -> "Horizontal"
  | Layout.Vertical -> "Vertical"
;;

let layout_scenario =
  QCheck2.Gen.(
    triple
      Generators.layout_direction
      Generators.layout_constraints
      Generators.rect)
;;

let print_layout_scenario =
  QCheck2.Print.(
    triple print_direction Generators.print_constraints Generators.print_rect)
;;

let property_one_region_per_constraint =
  QCheck2.Test.make
    ~name:"there is one region per constraint"
    ~print:print_layout_scenario
    layout_scenario
    (fun (direction, constraints, area) ->
       List.length (Layout.split ~direction ~constraints area)
       = List.length constraints)
;;

let property_regions_tile_the_area_without_overflowing =
  QCheck2.Test.make
    ~name:"regions follow each other and never exceed the area"
    ~print:print_layout_scenario
    layout_scenario
    (fun (direction, constraints, area) ->
       let rects = Layout.split ~direction ~constraints area in
       tile_the_area ~direction area rects
       && sum (List.map (extent ~direction) rects) <= extent ~direction area)
;;

let property_fill_absorbs_what_is_left =
  let max_length_count = 4 in
  let max_length = 10 in
  let max_extra = 20 in
  QCheck2.Test.make
    ~name:"a trailing fill item receives exactly the leftover space"
    ~print:QCheck2.Print.(pair (list int) int)
    QCheck2.Gen.(
      pair
        (list_size
           (int_range Int.zero max_length_count)
           (int_range Int.zero max_length))
        (int_range Int.zero max_extra))
    (fun (lengths, extra) ->
       let constraints =
         List.map (fun length -> Constraint.Length length) lengths
         @ [ Constraint.Fill Int.one ]
       in
       let rects =
         Layout.horizontal constraints (wide_region (sum lengths + extra))
       in
       List.nth (widths rects) (List.length lengths) = extra)
;;

let () =
  Alcotest.run
    "Layout"
    [ ( "sizes"
      , [ Alcotest.test_case
            "header, body, footer"
            `Quick
            test_header_body_footer
        ; Alcotest.test_case "percentages" `Quick test_percentages
        ; Alcotest.test_case "ratios" `Quick test_ratios
        ; Alcotest.test_case
            "ratio without denominator"
            `Quick
            test_ratio_without_a_denominator_takes_nothing
        ; Alcotest.test_case "max" `Quick test_max_is_a_ceiling_not_a_target
        ] )
    ; ( "leftover space"
      , [ Alcotest.test_case
            "fill by weight"
            `Quick
            test_fill_shares_the_leftover_by_weight
        ; Alcotest.test_case
            "rounding goes last"
            `Quick
            test_rounding_leftover_goes_to_the_last_item
        ; Alcotest.test_case
            "min grows without fill"
            `Quick
            test_min_items_grow_when_there_is_no_fill
        ; Alcotest.test_case
            "fill wins over min"
            `Quick
            test_fill_takes_the_leftover_before_min
        ; Alcotest.test_case
            "unused without receivers"
            `Quick
            test_leftover_stays_unused_without_fill_or_min
        ] )
    ; ( "not enough space"
      , [ Alcotest.test_case
            "last items shrink first"
            `Quick
            test_last_items_shrink_first_when_space_is_short
        ] )
    ; ( "regions"
      , [ Alcotest.test_case
            "no constraints"
            `Quick
            test_no_constraints_give_no_regions
        ; Alcotest.test_case
            "positions"
            `Quick
            test_regions_follow_each_other_inside_the_area
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_one_region_per_constraint
          ; property_regions_tile_the_area_without_overflowing
          ; property_fill_absorbs_what_is_left
          ] )
    ]
;;
