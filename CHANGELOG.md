# Changelog

All notable changes to this project are documented in this file.

Versioning follows the author's other OCaml libraries: releases are tagged
sequentially as `v1`, `v2`, `v3`, and so on.

## Unreleased

### Added

- `Rect`: a rectangular region of the terminal, with helpers to measure it,
  intersect it with another one and shrink it.
- `Color`: the 16 named ANSI colors, the 256-color palette and 24-bit RGB.
- `Style`: foreground, background and text effects of a cell, with `patch` to
  layer one style over another.
- `Grapheme`: splits text into user-perceived characters and measures how many
  terminal columns it takes, so CJK, emoji and combining marks line up.
- `Cell` and `Buffer`: the grid widgets draw into, with `Buffer.diff` to find
  the cells that changed between two frames, wide characters included.
- `Terminal`, `Frame` and `Widget`: draw a frame by rendering widgets into it;
  only the cells that changed since the previous frame reach the backend, and
  a resized screen is redrawn from scratch.
- `Backend`: the record of functions that connects the library to a terminal,
  and `Test_backend`, an in-memory terminal to test widgets without a real
  one.
- Text cursor: `Frame.set_cursor` shows the cursor at a `Position` once the
  frame is drawn, and hides it again when a frame does not ask for it.
- `Layout` and `Constraint`: split a region into rows or columns using fixed
  lengths, percentages, ratios, minimum and maximum sizes, and weighted fill.
- `Alignment`, `Span`, `Line` and `Text`: styled text made of spans and lines,
  with left, center and right alignment.
- `Block`: a frame with optional borders (plain, rounded, double or thick), a
  title and a background style, and `Block.inner` to find where the content
  inside it goes.
- `Paragraph`: shows text line by line, aligned left, center or right, cut at
  the edges of its region and optionally framed by a `Block`. It can wrap long
  lines with `Wrap`, scroll down through the rows and count them
  (`Paragraph.line_count`).
- `Wrap`: breaks long lines into rows that fit a width, between words or at
  any character, keeping styles and never cutting a wide character in half.
- `Event` and `Input_parser`: keyboard events (characters, Enter, Tab,
  Backspace, Escape, cursor and editing keys, F1 to F12, with Shift, Control
  and Alt) and a parser that turns the bytes of a terminal into them, even
  when a sequence arrives in pieces.
- `camlembert.unix`: a backend for real terminals. `Unix_backend.with_terminal`
  runs a function with the terminal in raw mode on the alternate screen,
  draws through ANSI escape sequences (moving the cursor and changing the
  style only where needed) and puts the terminal back when it returns, raises
  or is interrupted with Ctrl-C.
- `Camlembert_unix.Input`: reads keyboard events and window resizes from the
  terminal, waiting as long as needed (`wait`) or for a limited time
  (`poll`). A lone Escape key is told apart from the start of a sequence, and
  a resize is noticed while waiting.
- `examples/hello.ml`: a menu next to a greeting, built with `Layout`,
  `Block` and `Paragraph`.
- `examples/counter.ml`: an interactive counter that reacts to keys and
  resizes.
