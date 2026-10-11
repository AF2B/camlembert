module Eighths = Camlembert.Eighths

let empty = 0
let half = 4
let full = 8
let beyond_full = 20
let below_empty = -3
let four_cells = 4
let ten_cells = 10
let slightly_over_half = 0.55

(* Eighths of the 80 that ten cells have. *)
let eighths_of_slightly_over_half = 44
let nothing = 0.0
let half_the_run = 0.5
let the_whole_run = 1.0
let above_the_run = 2.0
let below_the_run = -1.0
let halving_divisor = 2
let max_cells = 20

let test_blocks_grow_with_the_eighths () =
  Alcotest.(check string) "empty" " " (Eighths.lower_block empty);
  Alcotest.(check string)
    "half from the bottom"
    "\u{2584}"
    (Eighths.lower_block half);
  Alcotest.(check string) "full" "\u{2588}" (Eighths.lower_block full);
  Alcotest.(check string)
    "half from the left"
    "\u{258C}"
    (Eighths.left_block half);
  Alcotest.(check string)
    "full from the left"
    "\u{2588}"
    (Eighths.left_block full)
;;

let test_eighths_outside_of_the_range_are_brought_into_it () =
  Alcotest.(check string) "above" "\u{2588}" (Eighths.lower_block beyond_full);
  Alcotest.(check string) "below" " " (Eighths.lower_block below_empty);
  Alcotest.(check string)
    "left, above"
    "\u{2588}"
    (Eighths.left_block beyond_full);
  Alcotest.(check string) "left, below" " " (Eighths.left_block below_empty)
;;

let test_a_fraction_of_a_run_of_cells () =
  Alcotest.(check int)
    "nothing"
    empty
    (Eighths.filled ~fraction:nothing ~cells:four_cells);
  Alcotest.(check int)
    "half"
    (four_cells * Eighths.per_cell / halving_divisor)
    (Eighths.filled ~fraction:half_the_run ~cells:four_cells);
  Alcotest.(check int)
    "all"
    (four_cells * Eighths.per_cell)
    (Eighths.filled ~fraction:the_whole_run ~cells:four_cells);
  Alcotest.(check int)
    "rounded to the nearest eighth"
    eighths_of_slightly_over_half
    (Eighths.filled ~fraction:slightly_over_half ~cells:ten_cells)
;;

let test_fractions_outside_of_zero_to_one_are_brought_into_it () =
  Alcotest.(check int)
    "above"
    (four_cells * Eighths.per_cell)
    (Eighths.filled ~fraction:above_the_run ~cells:four_cells);
  Alcotest.(check int)
    "below"
    empty
    (Eighths.filled ~fraction:below_the_run ~cells:four_cells)
;;

let property_a_larger_fraction_never_fills_less =
  QCheck2.Test.make
    ~name:"a larger fraction never fills fewer eighths"
    ~print:QCheck2.Print.(triple float float int)
    QCheck2.Gen.(
      triple Generators.ratio Generators.ratio (int_range Int.zero max_cells))
    (fun (first, second, cells) ->
       let lower = Float.min first second in
       let higher = Float.max first second in
       let filled fraction = Eighths.filled ~fraction ~cells in
       filled lower <= filled higher
       && filled higher <= cells * Eighths.per_cell)
;;

let () =
  Alcotest.run
    "Eighths"
    [ ( "blocks"
      , [ Alcotest.test_case "growing" `Quick test_blocks_grow_with_the_eighths
        ; Alcotest.test_case
            "range"
            `Quick
            test_eighths_outside_of_the_range_are_brought_into_it
        ] )
    ; ( "filled"
      , [ Alcotest.test_case
            "fractions"
            `Quick
            test_a_fraction_of_a_run_of_cells
        ; Alcotest.test_case
            "range"
            `Quick
            test_fractions_outside_of_zero_to_one_are_brought_into_it
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest
            property_a_larger_fraction_never_fills_less
        ] )
    ]
;;
