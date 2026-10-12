# Changelog

All notable changes to this project are documented in this file.

Versioning follows the author's other OCaml libraries: releases are tagged
sequentially as `v1`, `v2`, `v3`, and so on.

## Unreleased

## v1 - 2026-10-11

First release. Camlembert brings the model of Ratatui to OCaml: widgets draw
into a cell buffer, the buffer is compared with the previous frame, and only
the cells that changed reach the terminal. It needs OCaml 5.1 or later, `uucp`
and `uuseg`, and the Unix backend needs a Unix terminal and a C compiler.

### Added

#### Drawing

- `Rect`: a rectangular region of the terminal, with helpers to measure it,
  intersect it with another one, shrink it and center a smaller one in it
  (`Rect.centered`), for popups.
- `Position`: a cell of the screen, found by its column and its row, with
  `Position.origin` for the top-left corner.
- `Color`: the 16 named ANSI colors, the 256-color palette and 24-bit RGB.
- `Style`: foreground, background and text effects of a cell, with `patch` to
  layer one style over another.
- `Grapheme`: splits text into user-perceived characters and measures how many
  terminal columns it takes, so CJK, emoji and combining marks line up.
- `Cell` and `Buffer`: the grid widgets draw into, with `Buffer.diff` to find
  the cells that changed between two frames, wide characters included.
- `Eighths`: fractions of a cell, in eighths, drawn with block characters.
  The gauges and the charts use them to be more precise than a whole cell.

#### Terminal, layout and composition

- `Terminal`, `Frame` and `Widget`: draw a frame by rendering widgets into it;
  only the cells that changed since the previous frame reach the backend, and
  a resized screen is redrawn from scratch.
- `Layout` and `Constraint`: split a region into rows or columns using fixed
  lengths, percentages, ratios, minimum and maximum sizes, and weighted fill,
  with optional spacing between the items.
- `Widget.split` draws several widgets in one region, each in the share its
  constraint asks for, and `Stateful_widget.with_state` turns a stateful
  widget into a plain one, so both can be composed.
  `Frame.render_stateful_widget` draws widgets that keep a state between
  frames.
- Text cursor: `Frame.set_cursor` shows the cursor at a `Position` once the
  frame is drawn, and hides it again when a frame does not ask for it.
- Inline viewport: `Terminal.create ~viewport:(Inline rows)` draws in some
  rows below the cursor instead of on the whole screen, leaving the lines
  above untouched and scrolling the screen when there is not enough room. The
  viewport is placed again after a resize, a hidden cursor waits on its first
  row, and `Terminal.finish` leaves the cursor below the last frame.
  `Terminal.insert_before` prints lines above the viewport, as a history that
  scrolls up with the screen, and shows the last frame again right away.

#### Text

- `Alignment`, `Span`, `Line` and `Text`: styled text made of spans and lines,
  with left, center and right alignment. `Line.patch_style` layers a style
  under the spans of a line.
- `Wrap`: breaks long lines into rows that fit a width, between words or at
  any character, keeping styles and never cutting a wide character in half.

#### Widgets

- `Block`: a frame with optional borders (plain, rounded, double or thick), a
  title, a footer, padding and a background style, and `Block.inner` to find
  where the content inside it goes.
- `Paragraph`: shows text line by line, aligned left, center or right, cut at
  the edges of its region and optionally framed by a `Block`. It can wrap long
  lines with `Wrap`, scroll down through the rows and count them
  (`Paragraph.line_count`).
- `Gauge`: a progress bar filled in steps of an eighth of a cell, with a label
  that stays readable over the filled part.
- `Line_gauge`: a progress bar drawn as a thin line with a label before it;
  the part that is done is thick and the rest is thin, so the bar reads even
  without colors.
- `Tabs`: a row of titles with the selected one highlighted and a divider
  between them.
- `Item_list` and `Selection`: a list of rows with a selected one, a
  highlight and an optional symbol before it. The list scrolls to keep the
  selection in view, and `Selection` moves it.
- `Table`: rows of cells lined up in columns whose widths come from layout
  constraints, with an optional header that stays on top, column spacing and
  the same selection, highlight and scrolling as `Item_list`.
- `Tabs.index_at`, `Item_list.index_at` and `Table.index_at` tell which title,
  item or row is under a cell, as drawn, so a mouse click can select it.
- `Scrollbar`: a track and a thumb along any edge of a region, showing how
  far a content is scrolled.
- `Clear` and `Buffer.clear`: blank a region, to draw a popup over other
  widgets.
- `Sparkline`: a small chart of bars, up to the height of its region, in
  steps of an eighth of a cell.
- `Bar_chart`: vertical bars with a label under each one and the value
  written inside the bar when it fits, in steps of an eighth of a cell.
- `Marker`: how finely a canvas or a chart draws: Braille patterns (two by
  four dots per cell), half blocks, full blocks or bullets.
- `Canvas`: a drawing surface with its own coordinates, where points,
  segments, rectangles and circles are drawn with Braille patterns, half
  blocks, blocks or bullets, with text labels written over them.
- `Chart`: datasets drawn between two axes, as separate points or joined by
  lines, with axis labels and titles and a legend, using the markers of
  `Canvas`.
- `Line_edit`: a single line of text being edited, with a cursor, insertion,
  deletion, moves, the usual readline shortcuts, pastes and scrolling that
  keeps the cursor in view. Characters are what the user sees as one, so wide
  characters and emoji sequences are edited as a whole.
- `Text_field`: a box where the user types one line, drawn from a
  `Line_edit`, with a placeholder while it is empty and a cursor position to
  give to `Frame.set_cursor`.

#### Input

- `Event` and `Input_parser`: keyboard events (characters, Enter, Tab,
  Backspace, Escape, cursor and editing keys, F1 to F12, with Shift, Control
  and Alt) and a parser that turns the bytes of a terminal into them, even
  when a sequence arrives in pieces.
- Mouse: presses, releases, drags and wheel turns arrive as `Event.Mouse`, with
  the cell they happened over and the modifier keys held, when
  `Unix_backend.with_terminal` is given `~features:[ Mouse ]`.
- Bracketed paste: pasted text arrives as one `Event.Paste` when
  `Unix_backend.with_terminal` is given `~features:[ Bracketed_paste ]`, even
  when it comes in many pieces.
- `Camlembert_unix.Input`: reads keyboard events and window resizes from the
  terminal, waiting as long as needed (`wait`) or for a limited time
  (`poll`). A lone Escape key is told apart from the start of a sequence, and
  a resize is noticed while waiting.

#### Backends

- `Backend`: the record of functions that connects the library to a terminal:
  drawing cells, blanking the screen from a cell, scrolling it, moving the
  cursor and telling where it is.
- `Test_backend`: an in-memory terminal that does the same, scrolls and keeps
  the rows that leave the screen, to test widgets without a real one.
- `camlembert.unix`: a backend for real terminals. `Unix_backend.with_terminal`
  runs a function with the terminal in raw mode on the alternate screen,
  draws through ANSI escape sequences (moving the cursor and changing the
  style only where needed) and puts the terminal back when it returns, raises
  or is interrupted with Ctrl-C. With `~viewport:(Inline rows)` it stays on
  the normal screen instead, asks the terminal where the cursor is, and
  leaves the last frame with the cursor below it when it ends. `Ansi` has the
  escape sequences as plain strings, and `Tty` has the size of the terminal and
  its raw mode.

#### Examples

- `examples/hello.ml`: a menu next to a greeting, built with `Layout`,
  `Block` and `Paragraph`.
- `examples/counter.ml`: an interactive counter that reacts to keys and
  resizes.
- `examples/dashboard.ml`: a four-tab dashboard with gauges, a line gauge, a
  sparkline, a line chart of two series, a radar drawn on a canvas, a filter
  to type in, a table with a footer, a padded scrolling log and a quit
  dialog, built by composing widgets with `Widget.split`. It works with the
  keyboard and with the mouse: click a tab or a row, turn the wheel, paste
  into the filter.
- `examples/inline.ml`: an installer that shows its progress in four rows
  below the prompt, prints each finished package above them with
  `Terminal.insert_before` and leaves the last frame in the terminal.

#### Project

- Over 450 tests, run with `dune test`: unit tests with Alcotest and
  property-based tests with QCheck.
- Every public function documents each of its parameters, and
  `dune build @doc` builds the API documentation.
- `tools/screenshots` takes the pictures used by the README: it runs the
  examples in a private `tmux`, draws their screens as terminal windows and
  converts them to PNG.
- MIT license, with the copyright notice of Ratatui, which this library
  derives from.
