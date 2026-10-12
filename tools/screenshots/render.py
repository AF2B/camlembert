#!/usr/bin/env python3
"""Draws the text of a terminal screen, with its colors, as a terminal window.

The input is what `tmux capture-pane -e -p` prints: text with ANSI color
sequences. The output is an SVG, which `rsvg-convert` turns into a PNG. Box
drawing, block elements and Braille patterns are drawn as shapes instead of
font glyphs, so borders and bars join up whatever the font is. Only the
standard library is used.

    render.py screen.ans screen.svg --title "dashboard"
"""

import argparse
import re
import sys
import unicodedata
from html import escape

# Dracula colors. Indexes 0 to 7 are the normal colors, 8 to 15 the bright ones.
BACKGROUND = "#282a36"
FOREGROUND = "#f8f8f2"
TITLE_BAR = "#21222c"
TITLE_TEXT = "#6272a4"
PALETTE = [
    "#21222c", "#ff5555", "#50fa7b", "#f1fa8c", "#bd93f9", "#ff79c6", "#8be9fd", "#f8f8f2",
    "#6272a4", "#ff6e6e", "#69ff94", "#ffffa5", "#d6acff", "#ff92df", "#a4ffff", "#ffffff",
]
WINDOW_BUTTONS = ["#ff5555", "#f1fa8c", "#50fa7b"]

# The font is 0.6 em wide, so a 15 px font fills 9 px cells. A cell twice as tall
# as it is wide makes the square dots of Braille patterns really square, so
# circles drawn on a canvas look round. The picture is zoomed by two, which
# keeps one pixel lines sharp at the middle of a cell.
FONT_FAMILY = "Adwaita Mono"
FONT_SIZE = 15
CELL_WIDTH = 9
CELL_HEIGHT = 18
BASELINE = 14.1

# Window around the screen.
MARGIN = 24
PADDING = 16
TITLE_BAR_HEIGHT = 34
CORNER_RADIUS = 10

# Strokes, in pixels.
LIGHT = 1.0
HEAVY = 3.0
DOUBLE_OFFSET = 2.0
BRAILLE_DOT_RADIUS = 1.5

NONE, LIGHT_LINE, HEAVY_LINE, DOUBLE_LINE = 0, 1, 2, 3

# (left, right, up, down) of the box drawing characters that are drawn with straight lines.
BOX = {
    "─": (1, 1, 0, 0), "│": (0, 0, 1, 1), "┌": (0, 1, 0, 1), "┐": (1, 0, 0, 1),
    "└": (0, 1, 1, 0), "┘": (1, 0, 1, 0), "├": (0, 1, 1, 1), "┤": (1, 0, 1, 1),
    "┬": (1, 1, 0, 1), "┴": (1, 1, 1, 0), "┼": (1, 1, 1, 1),
    "━": (2, 2, 0, 0), "┃": (0, 0, 2, 2), "┏": (0, 2, 0, 2), "┓": (2, 0, 0, 2),
    "┗": (0, 2, 2, 0), "┛": (2, 0, 2, 0), "┣": (0, 2, 2, 2), "┫": (2, 0, 2, 2),
    "┳": (2, 2, 0, 2), "┻": (2, 2, 2, 0), "╋": (2, 2, 2, 2),
}
DOUBLE = {"═", "║", "╔", "╗", "╚", "╝"}
ROUNDED = {"╭", "╮", "╯", "╰"}

BRAILLE_BASE = 0x2800
# Bit of each dot, as (column, row) inside the 2 by 4 grid.
BRAILLE_DOTS = {
    0x01: (0, 0), 0x02: (0, 1), 0x04: (0, 2), 0x40: (0, 3),
    0x08: (1, 0), 0x10: (1, 1), 0x20: (1, 2), 0x80: (1, 3),
}


class Style:
    """Colors and effects that apply to the next characters."""

    def __init__(self):
        self.reset()

    def reset(self):
        self.foreground = None
        self.background = None
        self.bold = self.dim = self.italic = self.underline = False
        self.reverse = self.strike = self.hidden = False

    def copy(self):
        clone = Style()
        clone.__dict__.update(self.__dict__)
        return clone


def indexed_color(index):
    if index < 16:
        return PALETTE[index]
    if index < 232:
        levels = [0, 95, 135, 175, 215, 255]
        index -= 16
        red, green, blue = levels[index // 36], levels[(index // 6) % 6], levels[index % 6]
        return "#%02x%02x%02x" % (red, green, blue)
    gray = 8 + 10 * (index - 232)
    return "#%02x%02x%02x" % (gray, gray, gray)


def apply_sgr(style, parameters):
    """Applies one Select Graphic Rendition sequence."""
    numbers = [int(part) if part else 0 for part in parameters.split(";")] if parameters else [0]
    position = 0
    while position < len(numbers):
        code = numbers[position]
        position += 1
        if code == 0:
            style.reset()
        elif code == 1:
            style.bold = True
        elif code == 2:
            style.dim = True
        elif code == 3:
            style.italic = True
        elif code == 4:
            style.underline = True
        elif code == 7:
            style.reverse = True
        elif code == 8:
            style.hidden = True
        elif code == 9:
            style.strike = True
        elif code == 22:
            style.bold = style.dim = False
        elif code == 23:
            style.italic = False
        elif code == 24:
            style.underline = False
        elif code == 27:
            style.reverse = False
        elif code == 28:
            style.hidden = False
        elif code == 29:
            style.strike = False
        elif 30 <= code <= 37:
            style.foreground = PALETTE[code - 30]
        elif code == 39:
            style.foreground = None
        elif 40 <= code <= 47:
            style.background = PALETTE[code - 40]
        elif code == 49:
            style.background = None
        elif 90 <= code <= 97:
            style.foreground = PALETTE[code - 90 + 8]
        elif 100 <= code <= 107:
            style.background = PALETTE[code - 100 + 8]
        elif code in (38, 48):
            target = "foreground" if code == 38 else "background"
            kind = numbers[position] if position < len(numbers) else None
            if kind == 5 and position + 1 < len(numbers):
                setattr(style, target, indexed_color(numbers[position + 1]))
                position += 2
            elif kind == 2 and position + 3 < len(numbers):
                red, green, blue = numbers[position + 1 : position + 4]
                setattr(style, target, "#%02x%02x%02x" % (red, green, blue))
                position += 4


def character_width(character):
    if unicodedata.combining(character):
        return 0
    return 2 if unicodedata.east_asian_width(character) in ("W", "F") else 1


SEQUENCE = re.compile(r"\x1b\[([0-9;:?]*)([@-~])")


def parse_line(line, style):
    """Turns a line with color sequences into a list of (character, style, width).

    The style is shared between lines: tmux only prints a color sequence when it
    changes, even from the end of one line to the start of the next.
    """
    cells = []
    position = 0
    while position < len(line):
        sequence = SEQUENCE.match(line, position)
        if sequence:
            if sequence.group(2) == "m":
                apply_sgr(style, sequence.group(1).replace(":", ";"))
            position = sequence.end()
            continue
        character = line[position]
        position += 1
        width = character_width(character)
        if width > 0 and character not in "\r":
            cells.append((character, style.copy(), width))
    return cells


def resolved_colors(style):
    foreground = style.foreground or FOREGROUND
    background = style.background or BACKGROUND
    if style.reverse:
        foreground, background = background, foreground
    return foreground, background


def number(value):
    return ("%.2f" % value).rstrip("0").rstrip(".")


def rectangle(x, y, width, height, color, opacity=None):
    extra = ' fill-opacity="%s"' % number(opacity) if opacity is not None else ""
    return '<rect x="%s" y="%s" width="%s" height="%s" fill="%s"%s/>' % (
        number(x), number(y), number(width), number(height), color, extra)


def box_shapes(character, x, y, color):
    """Straight, double and rounded lines, drawn from the middle of the cell."""
    width, height = CELL_WIDTH, CELL_HEIGHT
    center_x, center_y = x + width / 2, y + height / 2
    right, bottom = x + width, y + height
    shapes = []
    if character in BOX:
        left_style, right_style, up_style, down_style = BOX[character]
        thickness = {LIGHT_LINE: LIGHT, HEAVY_LINE: HEAVY}
        if right_style:
            t = thickness[right_style]
            shapes.append(rectangle(center_x - t / 2, center_y - t / 2, right - (center_x - t / 2), t, color))
        if left_style:
            t = thickness[left_style]
            shapes.append(rectangle(x, center_y - t / 2, (center_x + t / 2) - x, t, color))
        if down_style:
            t = thickness[down_style]
            shapes.append(rectangle(center_x - t / 2, center_y - t / 2, t, bottom - (center_y - t / 2), color))
        if up_style:
            t = thickness[up_style]
            shapes.append(rectangle(center_x - t / 2, y, t, (center_y + t / 2) - y, color))
        return shapes
    stroke = 'fill="none" stroke="%s" stroke-width="%s"' % (color, number(LIGHT))
    gap = DOUBLE_OFFSET
    paths = {
        "═": ["M%s %sH%s" % (x, center_y - gap, right), "M%s %sH%s" % (x, center_y + gap, right)],
        "║": ["M%s %sV%s" % (center_x - gap, y, bottom), "M%s %sV%s" % (center_x + gap, y, bottom)],
        "╔": ["M%s %sV%sH%s" % (center_x - gap, bottom, center_y - gap, right),
              "M%s %sV%sH%s" % (center_x + gap, bottom, center_y + gap, right)],
        "╗": ["M%s %sV%sH%s" % (center_x + gap, bottom, center_y - gap, x),
              "M%s %sV%sH%s" % (center_x - gap, bottom, center_y + gap, x)],
        "╚": ["M%s %sV%sH%s" % (center_x - gap, y, center_y + gap, right),
              "M%s %sV%sH%s" % (center_x + gap, y, center_y - gap, right)],
        "╝": ["M%s %sV%sH%s" % (center_x + gap, y, center_y + gap, x),
              "M%s %sV%sH%s" % (center_x - gap, y, center_y - gap, x)],
    }
    radius = width / 2
    arcs = {
        "╭": "M%s %sV%sA%s %s 0 0 1 %s %sH%s" % (center_x, bottom, center_y + radius, radius, radius, center_x + radius, center_y, right),
        "╮": "M%s %sV%sA%s %s 0 0 0 %s %sH%s" % (center_x, bottom, center_y + radius, radius, radius, center_x - radius, center_y, x),
        "╯": "M%s %sV%sA%s %s 0 0 1 %s %sH%s" % (center_x, y, center_y - radius, radius, radius, center_x - radius, center_y, x),
        "╰": "M%s %sV%sA%s %s 0 0 0 %s %sH%s" % (center_x, y, center_y - radius, radius, radius, center_x + radius, center_y, right),
    }
    if character in paths:
        return ['<path d="%s" %s/>' % (d, stroke) for d in paths[character]]
    return ['<path d="%s" %s/>' % (arcs[character], stroke)]


def block_shapes(character, x, y, color):
    """Block elements: halves, eighths and shades."""
    code = ord(character)
    width, height = CELL_WIDTH, CELL_HEIGHT
    if code == 0x2580:
        return [rectangle(x, y, width, height / 2, color)]
    if 0x2581 <= code <= 0x2588:
        filled = height * (code - 0x2580) / 8
        return [rectangle(x, y + height - filled, width, filled, color)]
    if 0x2589 <= code <= 0x258F:
        filled = width * (0x2590 - code) / 8
        return [rectangle(x, y, filled, height, color)]
    if code == 0x2590:
        return [rectangle(x + width / 2, y, width / 2, height, color)]
    if 0x2591 <= code <= 0x2593:
        return [rectangle(x, y, width, height, color, opacity=(code - 0x2590) / 4)]
    if code == 0x2594:
        return [rectangle(x, y, width, height / 8, color)]
    if code == 0x2595:
        return [rectangle(x + width * 7 / 8, y, width / 8, height, color)]
    return None


def braille_shapes(character, x, y, color):
    bits = ord(character) - BRAILLE_BASE
    shapes = []
    for bit, (column, row) in BRAILLE_DOTS.items():
        if bits & bit:
            cx = x + CELL_WIDTH * (0.25 + 0.5 * column)
            cy = y + CELL_HEIGHT * (0.125 + 0.25 * row)
            shapes.append('<circle cx="%s" cy="%s" r="%s" fill="%s"/>' % (
                number(cx), number(cy), number(BRAILLE_DOT_RADIUS), color))
    return shapes


def drawn_shapes(character, x, y, color):
    """Shapes for the characters drawn as vectors, or None for a font glyph."""
    code = ord(character)
    if character in BOX or character in DOUBLE or character in ROUNDED:
        return box_shapes(character, x, y, color)
    if 0x2580 <= code <= 0x2595:
        return block_shapes(character, x, y, color)
    if 0x2800 <= code <= 0x28FF:
        return braille_shapes(character, x, y, color)
    return None


def is_blank(cells):
    return all(character == " " and style.background is None and not style.reverse
               for character, style, _ in cells)


def render(lines, title, columns, trim):
    style = Style()
    rows = [parse_line(line, style) for line in lines]
    while trim and rows and is_blank(rows[-1]):
        rows.pop()
    width_in_cells = columns or max((sum(w for _, _, w in row) for row in rows), default=1)
    grid_width = width_in_cells * CELL_WIDTH
    grid_height = len(rows) * CELL_HEIGHT
    window_width = grid_width + 2 * PADDING
    window_height = TITLE_BAR_HEIGHT + grid_height + 2 * PADDING
    total_width = window_width + 2 * MARGIN
    total_height = window_height + 2 * MARGIN

    out = []
    out.append('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">' % (
        total_width, total_height, total_width, total_height))
    out.append('<defs><filter id="shadow" x="-10%%" y="-10%%" width="120%%" height="125%%">'
               '<feGaussianBlur in="SourceAlpha" stdDeviation="9"/><feOffset dy="8" result="blur"/>'
               '<feFlood flood-color="#000000" flood-opacity="0.45"/><feComposite in2="blur" operator="in"/>'
               '<feMerge><feMergeNode/><feMergeNode in="SourceGraphic"/></feMerge></filter>'
               '<clipPath id="window"><rect x="%d" y="%d" width="%d" height="%d" rx="%d"/></clipPath></defs>' % (
                   MARGIN, MARGIN, window_width, window_height, CORNER_RADIUS))
    out.append('<rect x="%d" y="%d" width="%d" height="%d" rx="%d" fill="%s" filter="url(#shadow)"/>' % (
        MARGIN, MARGIN, window_width, window_height, CORNER_RADIUS, BACKGROUND))
    out.append('<g clip-path="url(#window)">')
    out.append('<rect x="%d" y="%d" width="%d" height="%d" fill="%s"/>' % (
        MARGIN, MARGIN, window_width, TITLE_BAR_HEIGHT, TITLE_BAR))
    for index, color in enumerate(WINDOW_BUTTONS):
        out.append('<circle cx="%d" cy="%d" r="6" fill="%s"/>' % (MARGIN + 20 + index * 20, MARGIN + TITLE_BAR_HEIGHT / 2, color))
    out.append('<text x="%s" y="%s" fill="%s" font-family="%s" font-size="13" text-anchor="middle">%s</text>' % (
        MARGIN + window_width / 2, MARGIN + TITLE_BAR_HEIGHT / 2 + 4.5, TITLE_TEXT, FONT_FAMILY, escape(title)))
    out.append('</g>')

    origin_x = MARGIN + PADDING
    origin_y = MARGIN + TITLE_BAR_HEIGHT + PADDING
    backgrounds, shapes, texts = [], [], []
    for row_index, row in enumerate(rows):
        y = origin_y + row_index * CELL_HEIGHT
        column = 0
        run = None
        for character, style, width in row:
            foreground, background = resolved_colors(style)
            x = origin_x + column * CELL_WIDTH
            if background != BACKGROUND:
                if run and run[0] == background and run[2] == column:
                    run[2] = column + width
                else:
                    if run:
                        backgrounds.append(run)
                    run = [background, column, column + width]
            elif run:
                backgrounds.append(run)
                run = None
            if run and run[2] < column + width:
                run[2] = column + width
            if not style.hidden and character != " ":
                opacity = ' opacity="0.6"' if style.dim else ""
                vector = drawn_shapes(character, x, y, foreground)
                if vector is not None:
                    shapes.extend(vector)
                else:
                    attributes = 'fill="%s"%s' % (foreground, opacity)
                    if style.bold:
                        attributes += ' font-weight="bold"'
                    if style.italic:
                        attributes += ' font-style="italic"'
                    texts.append('<text x="%s" y="%s" %s>%s</text>' % (
                        number(x), number(y + BASELINE), attributes, escape(character)))
            if style.underline and character != " ":
                shapes.append(rectangle(x, y + CELL_HEIGHT - 3, width * CELL_WIDTH, 1, foreground))
            if style.strike and character != " ":
                shapes.append(rectangle(x, y + CELL_HEIGHT / 2, width * CELL_WIDTH, 1, foreground))
            column += width
        if run:
            backgrounds.append(run)
            run = None
        # Backgrounds were collected per row; draw them now to keep row order.
        for color, start, stop in backgrounds:
            out.append(rectangle(origin_x + start * CELL_WIDTH, y, (stop - start) * CELL_WIDTH, CELL_HEIGHT, color))
        backgrounds = []
    out.append('<g shape-rendering="crispEdges">')
    out.extend(shapes)
    out.append('</g>')
    out.append('<g font-family="%s" font-size="%d" xml:space="preserve">' % (FONT_FAMILY, FONT_SIZE))
    out.extend(texts)
    out.append('</g>')
    out.append('</svg>')
    return "\n".join(out)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("input", help="text with ANSI colors, as printed by tmux capture-pane -e -p")
    parser.add_argument("output", help="SVG file to write")
    parser.add_argument("--title", default="", help="text in the title bar of the window")
    parser.add_argument("--columns", type=int, default=0, help="width of the screen in cells; the widest line by default")
    parser.add_argument("--keep-blank-rows", action="store_true", help="do not drop the blank rows at the end")
    arguments = parser.parse_args()
    with open(arguments.input, encoding="utf-8") as handle:
        lines = handle.read().split("\n")
    if lines and lines[-1] == "":
        lines.pop()
    svg = render(lines, arguments.title, arguments.columns, not arguments.keep_blank_rows)
    with open(arguments.output, "w", encoding="utf-8") as handle:
        handle.write(svg)


if __name__ == "__main__":
    sys.exit(main())
