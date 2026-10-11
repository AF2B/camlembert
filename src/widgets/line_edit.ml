(* The characters before the cursor, the nearest one first, and the characters
   after it, in reading order. [offset] counts the characters scrolled out of
   view at the left. *)
type t =
  { mutable before : string list
  ; mutable after : string list
  ; mutable offset : int
  }

let narrowest_field = Int.one
let sum = List.fold_left ( + ) Int.zero

(* Characters that take no column, such as line feeds, are not kept. *)
let characters_of text =
  Grapheme.clusters text
  |> List.filter (fun character -> Grapheme.width character > Int.zero)
;;

let create ?(text = "") () =
  { before = List.rev (characters_of text); after = []; offset = Int.zero }
;;

let characters edit = List.rev_append edit.before edit.after
let text edit = String.concat "" (characters edit)
let cursor edit = List.length edit.before

let set_text edit new_text =
  edit.before <- List.rev (characters_of new_text);
  edit.after <- [];
  edit.offset <- Int.zero
;;

let clear edit = set_text edit ""

let insert edit new_text =
  edit.before <- List.rev_append (characters_of new_text) edit.before
;;

let delete_before edit =
  match edit.before with
  | [] -> ()
  | _ :: rest -> edit.before <- rest
;;

let delete_after edit =
  match edit.after with
  | [] -> ()
  | _ :: rest -> edit.after <- rest
;;

let delete_to_start edit = edit.before <- []
let delete_to_end edit = edit.after <- []

let move_left edit =
  match edit.before with
  | [] -> ()
  | nearest :: rest ->
    edit.before <- rest;
    edit.after <- nearest :: edit.after
;;

let move_right edit =
  match edit.after with
  | [] -> ()
  | next :: rest ->
    edit.before <- next :: edit.before;
    edit.after <- rest
;;

let move_to_start edit =
  edit.after <- List.rev_append edit.before edit.after;
  edit.before <- []
;;

let move_to_end edit =
  edit.before <- List.rev_append edit.after edit.before;
  edit.after <- []
;;

(* Keys that do something when typed while holding control. *)
let start_key = "a"
let end_key = "e"
let backward_key = "b"
let forward_key = "f"
let backspace_key = "h"
let delete_key = "d"
let kill_to_end_key = "k"
let kill_to_start_key = "u"

let control_shortcuts =
  [ start_key, move_to_start
  ; end_key, move_to_end
  ; backward_key, move_left
  ; forward_key, move_right
  ; backspace_key, delete_before
  ; delete_key, delete_after
  ; kill_to_end_key, delete_to_end
  ; kill_to_start_key, delete_to_start
  ]
;;

let editing_keys =
  [ Event.Backspace, delete_before
  ; Event.Delete, delete_after
  ; Event.Left, move_left
  ; Event.Right, move_right
  ; Event.Home, move_to_start
  ; Event.End, move_to_end
  ]
;;

let apply_action edit action =
  action edit;
  true
;;

let handle edit = function
  | Event.Key
      { key = Event.Char letter
      ; modifiers = { Event.control = true; alt = false; _ }
      } ->
    List.assoc_opt letter control_shortcuts
    |> Option.fold ~none:false ~some:(apply_action edit)
  | Event.Key
      { key = Event.Char typed
      ; modifiers = { Event.control = false; alt = false; _ }
      } ->
    insert edit typed;
    true
  | Event.Key { key; modifiers = { Event.control = false; alt = false; _ } } ->
    List.assoc_opt key editing_keys
    |> Option.fold ~none:false ~some:(apply_action edit)
  | Event.Paste pasted ->
    insert edit pasted;
    true
  | Event.Key _ | Event.Resize _ | Event.Mouse _ -> false
;;

(* Columns taken by the characters from position [first] up to [last], which
   is not included. *)
let columns_between widths ~first ~last =
  List.filteri (fun index _ -> index >= first && index < last) widths |> sum
;;

let widths edit = List.map Grapheme.width (characters edit)

let reveal edit ~width =
  let width = Int.max narrowest_field width in
  let cursor_position = cursor edit in
  let widths = widths edit in
  let rec scroll_until_visible offset =
    if
      offset < cursor_position
      && columns_between widths ~first:offset ~last:cursor_position >= width
    then scroll_until_visible (succ offset)
    else offset
  in
  edit.offset <- scroll_until_visible (Int.min edit.offset cursor_position)
;;

let visible edit ~width =
  let rec take_fitting remaining = function
    | character :: rest when Grapheme.width character <= remaining ->
      character :: take_fitting (remaining - Grapheme.width character) rest
    | _ -> []
  in
  characters edit
  |> List.filteri (fun index _ -> index >= edit.offset)
  |> take_fitting width
  |> String.concat ""
;;

let cursor_column edit =
  columns_between
    (widths edit)
    ~first:(Int.min edit.offset (cursor edit))
    ~last:(cursor edit)
;;
