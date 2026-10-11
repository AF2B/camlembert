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
- `Backend`: the record of four functions that connects the library to a
  terminal, and `Test_backend`, an in-memory terminal to test widgets without
  a real one.
- `Layout` and `Constraint`: split a region into rows or columns using fixed
  lengths, percentages, ratios, minimum and maximum sizes, and weighted fill.
- `Alignment`, `Span`, `Line` and `Text`: styled text made of spans and lines,
  with left, center and right alignment.
