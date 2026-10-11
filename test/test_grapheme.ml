module Grapheme = Camlembert.Grapheme

let letters = [ "a"; "b"; "c" ]
let plain_text = String.concat "" letters
let e_with_acute = "e\u{0301}"
let carriage_return_line_feed = "\r\n"
let cjk_text = "\u{6F22}\u{5B57}"
let family_emoji = "\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}"
let brazilian_flag = "\u{1F1E7}\u{1F1F7}"
let line_feed = "\n"
let letter_then_ideograph = "a\u{6F22}"
let columns_per_wide_cluster = 2
let columns_for_two_wide_clusters = 4
let columns_for_letter_then_ideograph = 3
let no_columns = 0
let one_column = 1
let one_cluster = 1

let test_ascii_splits_into_single_characters () =
  Alcotest.(check (list string))
    "letters"
    letters
    (Grapheme.clusters plain_text)
;;

let test_empty_text_has_no_clusters () =
  Alcotest.(check (list string)) "empty" [] (Grapheme.clusters "")
;;

let test_combining_mark_stays_with_its_base () =
  Alcotest.(check (list string))
    "e with acute"
    [ e_with_acute ]
    (Grapheme.clusters e_with_acute)
;;

let test_line_break_pair_is_one_cluster () =
  Alcotest.(check int)
    "crlf"
    one_cluster
    (List.length (Grapheme.clusters carriage_return_line_feed))
;;

let test_emoji_sequences_are_one_cluster () =
  Alcotest.(check int)
    "family"
    one_cluster
    (List.length (Grapheme.clusters family_emoji));
  Alcotest.(check int)
    "flag"
    one_cluster
    (List.length (Grapheme.clusters brazilian_flag))
;;

let test_width_of_ascii_is_its_length () =
  Alcotest.(check int)
    "plain text"
    (String.length plain_text)
    (Grapheme.width plain_text);
  Alcotest.(check int) "empty" no_columns (Grapheme.width "")
;;

let test_wide_characters_take_two_columns () =
  Alcotest.(check int)
    "cjk"
    columns_for_two_wide_clusters
    (Grapheme.width cjk_text);
  Alcotest.(check int)
    "mixed"
    columns_for_letter_then_ideograph
    (Grapheme.width letter_then_ideograph)
;;

let test_combining_marks_take_no_extra_column () =
  Alcotest.(check int) "e with acute" one_column (Grapheme.width e_with_acute)
;;

let test_emoji_sequences_take_two_columns () =
  Alcotest.(check int)
    "family"
    columns_per_wide_cluster
    (Grapheme.width family_emoji);
  Alcotest.(check int)
    "flag"
    columns_per_wide_cluster
    (Grapheme.width brazilian_flag)
;;

let test_control_characters_take_no_column () =
  Alcotest.(check int) "line feed" no_columns (Grapheme.width line_feed)
;;

let property_ascii_takes_one_column_per_character =
  QCheck2.Test.make
    ~name:"printable ASCII takes one column per character"
    ~print:QCheck2.Print.string
    Generators.printable_ascii_text
    (fun text ->
       Grapheme.width text = String.length text
       && List.length (Grapheme.clusters text) = String.length text)
;;

let property_splitting_loses_nothing =
  QCheck2.Test.make
    ~name:"clusters rebuild the original text and are never empty"
    ~print:QCheck2.Print.string
    Generators.mixed_text
    (fun text ->
       let clusters = Grapheme.clusters text in
       String.concat "" clusters = text
       && List.for_all (fun cluster -> cluster <> "") clusters)
;;

let property_width_is_bounded_by_the_cluster_count =
  QCheck2.Test.make
    ~name:"width stays between zero and two columns per cluster"
    ~print:QCheck2.Print.string
    Generators.mixed_text
    (fun text ->
       let width = Grapheme.width text in
       let widest_possible =
         columns_per_wide_cluster * List.length (Grapheme.clusters text)
       in
       width >= no_columns && width <= widest_possible)
;;

let () =
  Alcotest.run
    "Grapheme"
    [ ( "clusters"
      , [ Alcotest.test_case
            "ASCII"
            `Quick
            test_ascii_splits_into_single_characters
        ; Alcotest.test_case "empty text" `Quick test_empty_text_has_no_clusters
        ; Alcotest.test_case
            "combining mark"
            `Quick
            test_combining_mark_stays_with_its_base
        ; Alcotest.test_case
            "line break pair"
            `Quick
            test_line_break_pair_is_one_cluster
        ; Alcotest.test_case
            "emoji sequences"
            `Quick
            test_emoji_sequences_are_one_cluster
        ] )
    ; ( "width"
      , [ Alcotest.test_case "ASCII" `Quick test_width_of_ascii_is_its_length
        ; Alcotest.test_case
            "wide characters"
            `Quick
            test_wide_characters_take_two_columns
        ; Alcotest.test_case
            "combining marks"
            `Quick
            test_combining_marks_take_no_extra_column
        ; Alcotest.test_case
            "emoji sequences"
            `Quick
            test_emoji_sequences_take_two_columns
        ; Alcotest.test_case
            "control characters"
            `Quick
            test_control_characters_take_no_column
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_ascii_takes_one_column_per_character
          ; property_splitting_loses_nothing
          ; property_width_is_bounded_by_the_cluster_count
          ] )
    ]
;;
