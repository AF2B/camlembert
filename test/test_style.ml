module Color = Camlembert.Color
module Style = Camlembert.Style

let style_testable = Alcotest.testable Style.pp Style.equal

let test_default_changes_nothing () =
  Alcotest.(check bool)
    "no colors"
    true
    (Style.default.foreground = None && Style.default.background = None);
  Alcotest.(check bool)
    "no effects"
    true
    (Style.Modifier_set.is_empty Style.default.modifiers)
;;

let test_modifier_order_and_repeats_do_not_matter () =
  let ordered = Style.make ~modifiers:[ Style.Bold; Style.Italic ] () in
  let shuffled =
    Style.make ~modifiers:[ Style.Italic; Style.Bold; Style.Italic ] ()
  in
  Alcotest.check style_testable "same style" ordered shuffled
;;

let test_patch_prefers_the_overlay_colors () =
  let base = Style.make ~foreground:Color.Red ~background:Color.Blue () in
  let overlay = Style.make ~foreground:Color.Green () in
  let expected = Style.make ~foreground:Color.Green ~background:Color.Blue () in
  Alcotest.check style_testable "colors" expected (Style.patch base overlay)
;;

let test_patch_combines_the_modifiers () =
  let base = Style.make ~modifiers:[ Style.Bold ] () in
  let overlay = Style.make ~modifiers:[ Style.Underlined ] () in
  let expected = Style.make ~modifiers:[ Style.Bold; Style.Underlined ] () in
  Alcotest.check style_testable "modifiers" expected (Style.patch base overlay)
;;

let property_default_is_a_neutral_element =
  QCheck2.Test.make
    ~name:"patching with the default style changes nothing"
    ~print:Generators.print_style
    Generators.style
    (fun style ->
       Style.equal (Style.patch style Style.default) style
       && Style.equal (Style.patch Style.default style) style)
;;

let property_patch_is_idempotent =
  QCheck2.Test.make
    ~name:"patching a style with itself changes nothing"
    ~print:Generators.print_style
    Generators.style
    (fun style -> Style.equal (Style.patch style style) style)
;;

let property_patch_is_associative =
  QCheck2.Test.make
    ~name:"patch is associative"
    ~print:
      QCheck2.Print.(
        triple
          Generators.print_style
          Generators.print_style
          Generators.print_style)
    QCheck2.Gen.(triple Generators.style Generators.style Generators.style)
    (fun (first, second, third) ->
       Style.equal
         (Style.patch (Style.patch first second) third)
         (Style.patch first (Style.patch second third)))
;;

let () =
  Alcotest.run
    "Style"
    [ ( "construction"
      , [ Alcotest.test_case "default" `Quick test_default_changes_nothing
        ; Alcotest.test_case
            "modifiers are a set"
            `Quick
            test_modifier_order_and_repeats_do_not_matter
        ] )
    ; ( "patch"
      , [ Alcotest.test_case
            "overlay colors win"
            `Quick
            test_patch_prefers_the_overlay_colors
        ; Alcotest.test_case
            "modifiers are combined"
            `Quick
            test_patch_combines_the_modifiers
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_default_is_a_neutral_element
          ; property_patch_is_idempotent
          ; property_patch_is_associative
          ] )
    ]
;;
