module Rect = Camlembert.Rect

let origin = 0
let block_size = 10
let overlap_offset = 5
let gap_between_blocks = 3
let negative_value = -1
let top_margin = 1
let bottom_margin = 2
let left_margin = 3
let right_margin = 4
let screen_width = 80
let screen_height = 24
let tiny_size = 2
let rect_testable = Alcotest.testable Rect.pp Rect.equal
let square ~column ~row ~size = Rect.make ~column ~row ~width:size ~height:size
let block = square ~column:origin ~row:origin ~size:block_size

let test_make_rejects_negative_values () =
  let rejects ~column ~row ~width ~height =
    Alcotest.check_raises
      "negative argument"
      (Invalid_argument "Rect.make: negative argument")
      (fun () -> ignore (Rect.make ~column ~row ~width ~height))
  in
  rejects
    ~column:negative_value
    ~row:origin
    ~width:block_size
    ~height:block_size;
  rejects
    ~column:origin
    ~row:negative_value
    ~width:block_size
    ~height:block_size;
  rejects ~column:origin ~row:origin ~width:negative_value ~height:block_size;
  rejects ~column:origin ~row:origin ~width:block_size ~height:negative_value
;;

let test_area_is_width_times_height () =
  let screen =
    Rect.make
      ~column:origin
      ~row:origin
      ~width:screen_width
      ~height:screen_height
  in
  Alcotest.(check int) "area" (screen_width * screen_height) (Rect.area screen)
;;

let test_edges_lie_just_outside () =
  let moved =
    square ~column:overlap_offset ~row:gap_between_blocks ~size:block_size
  in
  Alcotest.(check int) "right" (overlap_offset + block_size) (Rect.right moved);
  Alcotest.(check int)
    "bottom"
    (gap_between_blocks + block_size)
    (Rect.bottom moved)
;;

let test_is_empty () =
  let no_width =
    Rect.make ~column:origin ~row:origin ~width:origin ~height:block_size
  in
  let no_height =
    Rect.make ~column:origin ~row:origin ~width:block_size ~height:origin
  in
  Alcotest.(check bool) "zero width" true (Rect.is_empty no_width);
  Alcotest.(check bool) "zero height" true (Rect.is_empty no_height);
  Alcotest.(check bool) "regular block" false (Rect.is_empty block)
;;

let test_contains_excludes_the_far_edges () =
  let last = pred block_size in
  Alcotest.(check bool)
    "first cell"
    true
    (Rect.contains block ~column:origin ~row:origin);
  Alcotest.(check bool)
    "last cell"
    true
    (Rect.contains block ~column:last ~row:last);
  Alcotest.(check bool)
    "past the right edge"
    false
    (Rect.contains block ~column:block_size ~row:origin);
  Alcotest.(check bool)
    "past the bottom edge"
    false
    (Rect.contains block ~column:origin ~row:block_size);
  Alcotest.(check bool)
    "before the origin"
    false
    (Rect.contains block ~column:negative_value ~row:origin)
;;

let test_intersection_of_overlapping_rects () =
  let shifted =
    square ~column:overlap_offset ~row:overlap_offset ~size:block_size
  in
  let expected =
    square
      ~column:overlap_offset
      ~row:overlap_offset
      ~size:(block_size - overlap_offset)
  in
  Alcotest.check
    rect_testable
    "overlap"
    expected
    (Rect.intersection block shifted)
;;

let test_intersection_of_disjoint_rects_is_empty () =
  let far_away =
    square
      ~column:(Rect.right block + gap_between_blocks)
      ~row:origin
      ~size:block_size
  in
  Alcotest.(check bool)
    "empty"
    true
    (Rect.is_empty (Rect.intersection block far_away))
;;

let test_shrink_removes_margins () =
  let expected =
    Rect.make
      ~column:left_margin
      ~row:top_margin
      ~width:(block_size - left_margin - right_margin)
      ~height:(block_size - top_margin - bottom_margin)
  in
  let shrunk =
    Rect.shrink
      block
      ~top:top_margin
      ~bottom:bottom_margin
      ~left:left_margin
      ~right:right_margin
  in
  Alcotest.check rect_testable "inner" expected shrunk
;;

let test_shrink_beyond_the_size_is_empty () =
  let shrunk =
    Rect.shrink
      block
      ~top:block_size
      ~bottom:block_size
      ~left:block_size
      ~right:block_size
  in
  Alcotest.(check bool) "empty" true (Rect.is_empty shrunk)
;;

let test_shrink_rejects_negative_margins () =
  Alcotest.check_raises
    "negative margin"
    (Invalid_argument "Rect.shrink: negative margin")
    (fun () ->
       ignore
         (Rect.shrink
            block
            ~top:negative_value
            ~bottom:origin
            ~left:origin
            ~right:origin))
;;

let test_iter_positions_goes_row_by_row () =
  let visited = ref [] in
  let tiny = square ~column:origin ~row:origin ~size:tiny_size in
  Rect.iter_positions tiny (fun ~column ~row ->
    visited := (column, row) :: !visited);
  let second = succ origin in
  Alcotest.(check (list (pair int int)))
    "order"
    [ origin, origin; second, origin; origin, second; second, second ]
    (List.rev !visited)
;;

let count_positions rect =
  let count = ref Int.zero in
  Rect.iter_positions rect (fun ~column:_ ~row:_ -> incr count);
  !count
;;

let all_positions_satisfy rect predicate =
  let satisfied = ref true in
  Rect.iter_positions rect (fun ~column ~row ->
    if not (predicate ~column ~row) then satisfied := false);
  !satisfied
;;

let property_intersection_is_commutative =
  QCheck2.Test.make
    ~name:"intersection is commutative"
    ~print:QCheck2.Print.(pair Generators.print_rect Generators.print_rect)
    QCheck2.Gen.(pair Generators.rect Generators.rect)
    (fun (first, second) ->
       Rect.equal
         (Rect.intersection first second)
         (Rect.intersection second first))
;;

let property_intersection_lies_inside_both =
  QCheck2.Test.make
    ~name:"intersection lies inside both rects"
    ~print:QCheck2.Print.(pair Generators.print_rect Generators.print_rect)
    QCheck2.Gen.(pair Generators.rect Generators.rect)
    (fun (first, second) ->
       all_positions_satisfy
         (Rect.intersection first second)
         (fun ~column ~row ->
            Rect.contains first ~column ~row
            && Rect.contains second ~column ~row))
;;

let property_intersection_with_itself_is_identity =
  QCheck2.Test.make
    ~name:"intersection with itself is the same rect"
    ~print:Generators.print_rect
    Generators.rect
    (fun rect -> Rect.equal (Rect.intersection rect rect) rect)
;;

let property_area_counts_positions =
  QCheck2.Test.make
    ~name:"area equals the number of positions"
    ~print:Generators.print_rect
    Generators.rect
    (fun rect -> Rect.area rect = count_positions rect)
;;

let property_shrunk_rect_lies_inside_original =
  QCheck2.Test.make
    ~name:"shrunk rect lies inside the original"
    ~print:QCheck2.Print.(pair Generators.print_rect Generators.print_margins)
    QCheck2.Gen.(pair Generators.rect Generators.margins)
    (fun (rect, (top, bottom, left, right)) ->
       all_positions_satisfy
         (Rect.shrink rect ~top ~bottom ~left ~right)
         (fun ~column ~row -> Rect.contains rect ~column ~row))
;;

let () =
  Alcotest.run
    "Rect"
    [ ( "construction"
      , [ Alcotest.test_case
            "rejects negative values"
            `Quick
            test_make_rejects_negative_values
        ] )
    ; ( "measures"
      , [ Alcotest.test_case "area" `Quick test_area_is_width_times_height
        ; Alcotest.test_case "edges" `Quick test_edges_lie_just_outside
        ; Alcotest.test_case "is_empty" `Quick test_is_empty
        ; Alcotest.test_case
            "contains"
            `Quick
            test_contains_excludes_the_far_edges
        ] )
    ; ( "intersection"
      , [ Alcotest.test_case
            "overlapping"
            `Quick
            test_intersection_of_overlapping_rects
        ; Alcotest.test_case
            "disjoint"
            `Quick
            test_intersection_of_disjoint_rects_is_empty
        ] )
    ; ( "shrink"
      , [ Alcotest.test_case
            "removes margins"
            `Quick
            test_shrink_removes_margins
        ; Alcotest.test_case
            "beyond the size"
            `Quick
            test_shrink_beyond_the_size_is_empty
        ; Alcotest.test_case
            "negative margins"
            `Quick
            test_shrink_rejects_negative_margins
        ] )
    ; ( "iteration"
      , [ Alcotest.test_case
            "row by row"
            `Quick
            test_iter_positions_goes_row_by_row
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_intersection_is_commutative
          ; property_intersection_lies_inside_both
          ; property_intersection_with_itself_is_identity
          ; property_area_counts_positions
          ; property_shrunk_rect_lies_inside_original
          ] )
    ]
;;
