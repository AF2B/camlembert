module Cell = Camlembert.Cell
module Color = Camlembert.Color
module Style = Camlembert.Style

let letter = "a"
let other_letter = "b"
let wide_character = "\u{6F22}"
let one_column = 1
let two_columns = 2
let cell_testable = Alcotest.testable Cell.pp Cell.equal
let red_style = Style.make ~foreground:Color.Red ()
let blue_background = Style.make ~background:Color.Blue ()

let test_empty_is_a_blank_space () =
  Alcotest.check cell_testable "empty" (Cell.make " ") Cell.empty
;;

let test_make_defaults_to_the_default_style () =
  Alcotest.check
    cell_testable
    "default style"
    { Cell.symbol = letter; style = Style.default }
    (Cell.make letter)
;;

let test_with_symbol_keeps_the_style () =
  let styled = Cell.make ~style:red_style letter in
  Alcotest.check
    cell_testable
    "style kept"
    (Cell.make ~style:red_style other_letter)
    (Cell.with_symbol styled other_letter)
;;

let test_patch_style_layers_over_the_existing_style () =
  let styled = Cell.make ~style:red_style letter in
  let expected_style = Style.patch red_style blue_background in
  Alcotest.check
    cell_testable
    "layered"
    (Cell.make ~style:expected_style letter)
    (Cell.patch_style styled blue_background)
;;

let test_width_follows_the_symbol () =
  Alcotest.(check int) "narrow" one_column (Cell.width (Cell.make letter));
  Alcotest.(check int)
    "wide"
    two_columns
    (Cell.width (Cell.make wide_character))
;;

let test_equality_compares_symbol_and_style () =
  let styled = Cell.make ~style:red_style letter in
  Alcotest.(check bool)
    "same"
    true
    (Cell.equal styled (Cell.make ~style:red_style letter));
  Alcotest.(check bool)
    "other symbol"
    false
    (Cell.equal styled (Cell.make ~style:red_style other_letter));
  Alcotest.(check bool)
    "other style"
    false
    (Cell.equal styled (Cell.make letter))
;;

let () =
  Alcotest.run
    "Cell"
    [ ( "cell"
      , [ Alcotest.test_case "empty" `Quick test_empty_is_a_blank_space
        ; Alcotest.test_case
            "default style"
            `Quick
            test_make_defaults_to_the_default_style
        ; Alcotest.test_case
            "with_symbol"
            `Quick
            test_with_symbol_keeps_the_style
        ; Alcotest.test_case
            "patch_style"
            `Quick
            test_patch_style_layers_over_the_existing_style
        ; Alcotest.test_case "width" `Quick test_width_follows_the_symbol
        ; Alcotest.test_case
            "equality"
            `Quick
            test_equality_compares_symbol_and_style
        ] )
    ]
;;
