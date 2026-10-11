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
