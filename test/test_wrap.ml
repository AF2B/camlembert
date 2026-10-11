module Alignment = Camlembert.Alignment
module Color = Camlembert.Color
module Grapheme = Camlembert.Grapheme
module Line = Camlembert.Line
module Span = Camlembert.Span
module Style = Camlembert.Style
module Text = Camlembert.Text
module Wrap = Camlembert.Wrap

let wide_text = "\u{6F22}\u{5B57}\u{6F22}\u{5B57}"
let row_for_two_wide_characters = 5
let row_narrower_than_a_wide_character = 1
let row_for_five_characters = 5
let row_for_three_characters = 3
let row_for_seven_characters = 7
let row_for_two_characters = 2
let ten_letters = "abcdefghij"
let red_style = Style.make ~foreground:Color.Red ()
let blue_style = Style.make ~foreground:Color.Blue ()

let text_of_line (line : Line.t) =
  String.concat "" (List.map (fun (span : Span.t) -> span.content) line.spans)
;;

let rows mode ~width text =
  List.map text_of_line (Wrap.line mode ~width (Line.of_string text))
;;

let test_a_line_that_fits_stays_whole () =
  let sentence = "hello world" in
  Alcotest.(check (list string))
    "one row"
    [ sentence ]
    (rows Wrap.Word ~width:(String.length sentence) sentence)
;;

let test_words_are_broken_without_the_spaces_between_them () =
  Alcotest.(check (list string))
    "two rows"
    [ "hello"; "world" ]
    (rows Wrap.Word ~width:(String.length "hello") "hello world");
  Alcotest.(check (list string))
    "words stay together"
    [ "the quick"; "brown fox" ]
    (rows Wrap.Word ~width:(String.length "the quick") "the quick brown fox")
;;

let test_a_word_longer_than_a_row_is_cut () =
  Alcotest.(check (list string))
    "cut"
    [ "abcde"; "fghij"; "klmno" ]
    (rows Wrap.Word ~width:row_for_five_characters "abcdefghijklmno");
  Alcotest.(check (list string))
    "after a short word"
    [ "hi"; "abcde"; "fghij" ]
    (rows Wrap.Word ~width:row_for_five_characters ("hi " ^ ten_letters))
;;

let test_leading_spaces_are_kept () =
  Alcotest.(check (list string))
    "indented"
    [ "  hi"; "there" ]
    (rows Wrap.Word ~width:row_for_seven_characters "  hi there")
;;

let test_empty_and_blank_lines_give_one_empty_row () =
  Alcotest.(check (list string))
    "empty"
    [ "" ]
    (rows Wrap.Word ~width:row_for_five_characters "");
  Alcotest.(check (list string))
    "blank"
    [ "" ]
    (rows Wrap.Word ~width:row_for_three_characters "     ")
;;

let test_character_mode_breaks_anywhere () =
  Alcotest.(check (list string))
    "full rows"
    [ "abc"; "def"; "g" ]
    (rows Wrap.Character ~width:row_for_three_characters "abcdefg");
  Alcotest.(check (list string))
    "spaces are kept"
    [ "ab "; "cd" ]
    (rows Wrap.Character ~width:row_for_three_characters "ab cd")
;;

let test_wide_characters_are_never_cut () =
  let expected = [ "\u{6F22}\u{5B57}"; "\u{6F22}\u{5B57}" ] in
  Alcotest.(check (list string))
    "word mode"
    expected
    (rows Wrap.Word ~width:row_for_two_wide_characters wide_text);
  Alcotest.(check (list string))
    "character mode"
    expected
    (rows Wrap.Character ~width:row_for_two_wide_characters wide_text)
;;

let test_a_character_wider_than_the_row_gets_its_own_row () =
  Alcotest.(check (list string))
    "own row"
    [ "\u{6F22}" ]
    (rows Wrap.Character ~width:row_narrower_than_a_wide_character "\u{6F22}")
;;

let test_widths_below_one_count_as_one () =
  Alcotest.(check (list string))
    "zero"
    [ "a"; "b" ]
    (rows Wrap.Character ~width:Int.zero "ab");
  Alcotest.(check (list string))
    "negative"
    [ "a"; "b" ]
    (rows Wrap.Character ~width:(Int.neg Int.one) "ab")
;;

let test_styles_stay_with_their_text () =
  let line =
    Line.make
      [ Span.make ~style:red_style "hello "
      ; Span.make ~style:blue_style "world"
      ]
  in
  let expected_rows =
    [ [ Span.make ~style:red_style "hello" ]
    ; [ Span.make ~style:blue_style "world" ]
    ]
  in
  let spans_of_rows =
    List.map
      (fun (row : Line.t) -> row.spans)
      (Wrap.line Wrap.Word ~width:row_for_five_characters line)
  in
  Alcotest.(check bool) "styled spans" true (spans_of_rows = expected_rows)
;;

let test_alignment_is_kept_on_every_row () =
  let line = Line.of_string ~alignment:Alignment.Right "hello world" in
  let alignments =
    List.map
      (fun (row : Line.t) -> row.alignment)
      (Wrap.line Wrap.Word ~width:row_for_five_characters line)
  in
  Alcotest.(check bool)
    "right on both rows"
    true
    (alignments = [ Some Alignment.Right; Some Alignment.Right ])
;;

let test_text_wraps_line_by_line () =
  let lines = Text.of_string "ab cd\nef" in
  Alcotest.(check (list string))
    "rows of all lines"
    [ "ab"; "cd"; "ef" ]
    (List.map
       text_of_line
       (Wrap.text Wrap.Word ~width:row_for_two_characters lines))
;;

let modes = [ Wrap.Word; Wrap.Character ]
let without_spaces text = String.concat "" (String.split_on_char ' ' text)
let print_wrap_case = QCheck2.Print.(pair string int)

let wrap_case =
  QCheck2.Gen.(pair Generators.wrappable_text Generators.row_width)
;;

let property_rows_fit_the_width =
  QCheck2.Test.make
    ~name:"every row fits the width"
    ~print:print_wrap_case
    wrap_case
    (fun (text, width) ->
       List.for_all
         (fun mode ->
            Wrap.line mode ~width (Line.of_string text)
            |> List.for_all (fun row -> Line.width row <= width))
         modes)
;;

let property_character_mode_loses_nothing =
  QCheck2.Test.make
    ~name:"character mode keeps every character"
    ~print:print_wrap_case
    wrap_case
    (fun (text, width) ->
       String.concat "" (rows Wrap.Character ~width text) = text)
;;

let property_word_mode_keeps_every_word =
  QCheck2.Test.make
    ~name:"word mode keeps everything but spaces"
    ~print:print_wrap_case
    wrap_case
    (fun (text, width) ->
       without_spaces (String.concat "" (rows Wrap.Word ~width text))
       = without_spaces text)
;;

let property_there_is_always_a_row =
  QCheck2.Test.make
    ~name:"a line gives at least one row"
    ~print:print_wrap_case
    wrap_case
    (fun (text, width) ->
       List.for_all
         (fun mode -> Wrap.line mode ~width (Line.of_string text) <> [])
         modes)
;;

let property_wrapping_twice_changes_nothing =
  QCheck2.Test.make
    ~name:"wrapping rows that already fit changes nothing"
    ~print:print_wrap_case
    wrap_case
    (fun (text, width) ->
       List.for_all
         (fun mode ->
            let once = rows mode ~width text in
            List.concat_map (rows mode ~width) once = once)
         modes)
;;

let () =
  Alcotest.run
    "Wrap"
    [ ( "word mode"
      , [ Alcotest.test_case "fits" `Quick test_a_line_that_fits_stays_whole
        ; Alcotest.test_case
            "between words"
            `Quick
            test_words_are_broken_without_the_spaces_between_them
        ; Alcotest.test_case
            "long words"
            `Quick
            test_a_word_longer_than_a_row_is_cut
        ; Alcotest.test_case "indentation" `Quick test_leading_spaces_are_kept
        ; Alcotest.test_case
            "empty lines"
            `Quick
            test_empty_and_blank_lines_give_one_empty_row
        ] )
    ; ( "character mode"
      , [ Alcotest.test_case
            "anywhere"
            `Quick
            test_character_mode_breaks_anywhere
        ] )
    ; ( "widths"
      , [ Alcotest.test_case
            "wide characters"
            `Quick
            test_wide_characters_are_never_cut
        ; Alcotest.test_case
            "wider than the row"
            `Quick
            test_a_character_wider_than_the_row_gets_its_own_row
        ; Alcotest.test_case
            "below one"
            `Quick
            test_widths_below_one_count_as_one
        ] )
    ; ( "styles and lines"
      , [ Alcotest.test_case "styles" `Quick test_styles_stay_with_their_text
        ; Alcotest.test_case
            "alignment"
            `Quick
            test_alignment_is_kept_on_every_row
        ; Alcotest.test_case "text" `Quick test_text_wraps_line_by_line
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ property_rows_fit_the_width
          ; property_character_mode_loses_nothing
          ; property_word_mode_keeps_every_word
          ; property_there_is_always_a_row
          ; property_wrapping_twice_changes_nothing
          ] )
    ]
;;
