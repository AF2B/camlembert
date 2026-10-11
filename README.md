# camlembert

Terminal user interfaces for OCaml, ported from Rust's
[Ratatui](https://github.com/ratatui/ratatui).

Camlembert is an immediate-mode library: on every frame you describe the whole
screen by drawing widgets into a buffer. The library compares that buffer with
the previous frame and sends only the cells that changed to the terminal.

## Status

Early development, not yet published to opam.

Available today: layout by constraints, styled text with wrapping and
scrolling, keyboard and resize events, a text cursor, correct column widths for
CJK, emoji and combining marks, a Unix backend and an in-memory backend for
tests, and these widgets: `Block`, `Paragraph`, `Gauge`, `Tabs`, `Item_list`,
`Table`, `Scrollbar`, `Sparkline` and `Clear`.

Not there yet: mouse events, bracketed paste, charts (`BarChart`, `Chart`,
`Canvas`) and Windows support.

## Example

```ocaml
module Block = Camlembert.Block
module Frame = Camlembert.Frame
module Line = Camlembert.Line
module Paragraph = Camlembert.Paragraph
module Terminal = Camlembert.Terminal
module Text = Camlembert.Text

let greeting =
  Paragraph.make
    ~block:(Block.make ~borders:Block.all_sides ~title:(Line.of_string " Hello ") ())
    (Text.of_string "Terminal UIs for OCaml")

let render_frame frame =
  Frame.render_widget frame (Paragraph.widget greeting) ~area:(Frame.area frame)

let () =
  Camlembert_unix.Unix_backend.with_terminal (fun terminal ->
    Terminal.draw terminal render_frame;
    ignore (In_channel.input_char In_channel.stdin))
```

Three larger examples live in [`examples/`](examples):
[`hello.ml`](examples/hello.ml) splits the screen into a menu and a greeting,
[`counter.ml`](examples/counter.ml) reacts to keys and resizes, and
[`dashboard.ml`](examples/dashboard.ml) has three tabs with gauges, a chart, a
table and a scrolling log. Run one with `dune exec examples/dashboard.exe`.
This is `hello.exe`:

```
╭ Menu ──────────────╮╔ Hello ═══════════════════════════════════════╗
│• Widgets           │║                  Camlembert                  ║
│• Layout            │║                                              ║
│• Buffer            │║Terminal UIs for OCaml, ported from Ratatui.  ║
│• Backends          │║Wide characters line up: 漢字かな             ║
│                    │║                                              ║
│                    │║                         Press any key to quit║
│                    │║                                              ║
│                    │║                                              ║
╰────────────────────╯╚══════════════════════════════════════════════╝
```

And the Packages tab of the dashboard, with the fifth row selected:

```
╭──────────────────────────────────────────────────────────────╮
│ Overview │ Packages │ Logs                                   │
╰──────────────────────────────────────────────────────────────╯
╭ Packages ────────────────────────────────────────────────────╮
│  Package                                 Version         Size│
│  alcotest                                1.9.1         1.2 MB│
│  camlembert                              0.1.0         340 KB│
│  dune                                    3.24.2       28.4 MB│
│  ocaml-lsp-server                        1.27.0        9.8 MB│
│▶ ocamlformat                             0.29.0       14.1 MB│
│  odoc                                    3.2.1         6.3 MB│
│  qcheck-core                             0.91          820 KB│
│  uucp                                    18.0.0       21.7 MB│
│  uuseg                                   18.0.0        1.1 MB│
╰──────────────────────────────────────────────────────────────╯
   Up, Down, Home, End move | Tab switches tab | q quits
```

## How it works

```
widgets ──draw──▶ Buffer ──diff──▶ changed cells ──▶ Backend ──▶ terminal
                    ▲
                    └── the previous frame
```

1. A widget is a plain function `Rect.t -> Buffer.t -> unit`. It draws itself
   into the region it is given.
2. `Terminal.draw` hands a fresh frame to your function, then compares the
   finished buffer with the previous one using `Buffer.diff`.
3. Only the changed cells go to the `Backend`, a record of functions. The
   Unix backend turns them into ANSI escape sequences, moving the cursor and
   changing the style only where needed.

Widgets compose as ordinary functions: `Widget.split` draws several widgets in
one region, each in the share its layout constraint asks for, and
`Stateful_widget.with_state` binds the state of widgets such as `Item_list` and
`Table`.

An application reads `Event`s from `Camlembert_unix.Input` (`wait` for the next
one, or `poll` with a timeout so the screen can refresh by itself) and draws a
new frame after each one.

Because the backend is the only part that talks to the outside world, widgets
can be tested without a terminal:

```ocaml
let test_backend = Test_backend.create ~width:20 ~height:3 in
let terminal = Terminal.create (Test_backend.backend test_backend) in
Terminal.draw terminal render_frame;
Test_backend.to_lines test_backend (* one string per row of the screen *)
```

## Modules

| Module | Role |
| --- | --- |
| `Rect`, `Position`, `Color`, `Style`, `Cell`, `Buffer` | The cell grid widgets draw into, and the diff between two frames |
| `Grapheme` | Splitting text into user-perceived characters and measuring terminal columns |
| `Layout`, `Constraint` | Splitting a region with lengths, percentages, ratios, minimums, maximums and fill |
| `Widget`, `Stateful_widget`, `Frame`, `Terminal`, `Backend` | Drawing frames, composing widgets and sending the diff to a backend |
| `Test_backend` | An in-memory terminal to test widgets |
| `Event`, `Input_parser` | Keyboard and resize events, and the parser that turns terminal bytes into them |
| `Alignment`, `Span`, `Line`, `Text`, `Wrap` | Styled text, and breaking long lines into rows |
| `Block`, `Paragraph`, `Gauge`, `Tabs`, `Scrollbar`, `Sparkline`, `Clear` | Widgets that keep no state |
| `Selection`, `Item_list`, `Table` | Widgets with a selected row that scrolls |
| `Camlembert_unix` (`camlembert.unix`) | The Unix backend: `Unix_backend`, `Input`, `Ansi` and `Tty` |

## Repository layout

```
src/core/      cells, buffer, styles, text measuring and layout
src/input/     keyboard and resize events, and the byte parser
src/render/    widget, frame, terminal and backends
src/widgets/   text and every built-in widget
unix/          the Unix backend (ANSI sequences, raw mode, window size, input)
test/          unit and property-based tests
examples/      runnable examples
```

## Development

Install the dependencies and the tools once:

```bash
opam install . --deps-only --with-test
opam install ocamlformat.0.29.0 ocaml-lsp-server odoc
```

Then:

```bash
dune build @all @fmt   # build, with strict warnings, and check the formatting
dune test              # unit tests (Alcotest) and property-based tests (QCheck)
dune fmt               # format the code
```

Branches follow `<type>/<short-description>` and commits follow
[Conventional Commits](https://www.conventionalcommits.org/), in English.
Changes worth knowing about go in [CHANGELOG.md](CHANGELOG.md).

## License

MIT. Camlembert derives from Ratatui, which is also MIT licensed; both
copyright notices are kept in [LICENSE](LICENSE).
