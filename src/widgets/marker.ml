type t =
  | Braille
  | Half_block
  | Block
  | Dot

let braille_columns = 2
let braille_rows = 4
let half_block_rows = 2
let one_dot = 1

let dots_per_cell = function
  | Braille -> braille_columns, braille_rows
  | Half_block -> one_dot, half_block_rows
  | Block | Dot -> one_dot, one_dot
;;

(* The Braille patterns block starts with an empty pattern. Each of the eight
   dots of a pattern adds one bit, numbered as in the Unicode standard. *)
let braille_base = 0x2800
let dot_1 = 0x01
let dot_2 = 0x02
let dot_3 = 0x04
let dot_4 = 0x08
let dot_5 = 0x10
let dot_6 = 0x20
let dot_7 = 0x40
let dot_8 = 0x80
let utf_8_longest_character = 4
let first_column = 0
let second_column = 1
let first_row = 0
let second_row = 1
let third_row = 2
let fourth_row = 3

(* The dots of a pattern, by column and by row inside the cell. The seventh
   and eighth dots were added to the standard after the first six, which is why
   they sit in the fourth row. *)
let braille_dots =
  [ (first_column, first_row), dot_1
  ; (first_column, second_row), dot_2
  ; (first_column, third_row), dot_3
  ; (first_column, fourth_row), dot_7
  ; (second_column, first_row), dot_4
  ; (second_column, second_row), dot_5
  ; (second_column, third_row), dot_6
  ; (second_column, fourth_row), dot_8
  ]
;;

let braille_symbol ~painted =
  let bits =
    List.fold_left
      (fun total ((column, row), bit) ->
         if painted ~column ~row then total lor bit else total)
      Int.zero
      braille_dots
  in
  let buffer = Stdlib.Buffer.create utf_8_longest_character in
  Stdlib.Buffer.add_utf_8_uchar buffer (Uchar.of_int (braille_base + bits));
  Stdlib.Buffer.contents buffer
;;

let upper_half_block = "\u{2580}"
let lower_half_block = "\u{2584}"
let full_block = "\u{2588}"
let bullet = "\u{2022}"
let blank = " "

let half_block_symbol ~painted =
  match
    ( painted ~column:first_column ~row:first_row
    , painted ~column:first_column ~row:second_row )
  with
  | true, true -> full_block
  | true, false -> upper_half_block
  | false, true -> lower_half_block
  | false, false -> blank
;;

let symbol marker ~painted =
  match marker with
  | Braille -> braille_symbol ~painted
  | Half_block -> half_block_symbol ~painted
  | Block -> full_block
  | Dot -> bullet
;;
