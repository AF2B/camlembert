module Color = Camlembert.Color

let ansi_palette_size = 16
let first_palette_position = 0
let extended_palette_index = 200
let red_channel = 10
let green_channel = 20
let blue_channel = 30
let color_testable = Alcotest.testable Color.pp Color.equal

let test_palette_has_sixteen_distinct_colors () =
  Alcotest.(check int) "size" ansi_palette_size (List.length Color.ansi_palette);
  Alcotest.(check int)
    "distinct colors"
    ansi_palette_size
    (List.length (List.sort_uniq Stdlib.compare Color.ansi_palette))
;;

let test_palette_index_follows_the_ansi_order () =
  Alcotest.(check (option int))
    "first color"
    (Some first_palette_position)
    (Color.palette_index Color.Black);
  Alcotest.(check (option int))
    "last color"
    (Some (pred ansi_palette_size))
    (Color.palette_index Color.White)
;;

let test_palette_index_finds_every_named_color () =
  let check_round_trip color =
    match Color.palette_index color with
    | None -> Alcotest.fail "named color without a palette index"
    | Some index ->
      Alcotest.check
        color_testable
        "round trip"
        color
        (List.nth Color.ansi_palette index)
  in
  List.iter check_round_trip Color.ansi_palette
;;

let test_palette_index_of_indexed_color () =
  Alcotest.(check (option int))
    "indexed"
    (Some extended_palette_index)
    (Color.palette_index (Color.Indexed extended_palette_index))
;;

let test_colors_outside_the_palette_have_no_index () =
  let rgb =
    Color.Rgb { red = red_channel; green = green_channel; blue = blue_channel }
  in
  Alcotest.(check (option int)) "reset" None (Color.palette_index Color.Reset);
  Alcotest.(check (option int)) "rgb" None (Color.palette_index rgb)
;;

let () =
  Alcotest.run
    "Color"
    [ ( "palette"
      , [ Alcotest.test_case
            "sixteen distinct colors"
            `Quick
            test_palette_has_sixteen_distinct_colors
        ; Alcotest.test_case
            "ANSI order"
            `Quick
            test_palette_index_follows_the_ansi_order
        ; Alcotest.test_case
            "every named color"
            `Quick
            test_palette_index_finds_every_named_color
        ; Alcotest.test_case
            "indexed color"
            `Quick
            test_palette_index_of_indexed_color
        ; Alcotest.test_case
            "outside the palette"
            `Quick
            test_colors_outside_the_palette_have_no_index
        ] )
    ]
;;
