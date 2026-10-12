# camlembert

Terminal user interfaces for OCaml, ported from Rust's
[Ratatui](https://github.com/ratatui/ratatui).

Camlembert is an immediate-mode library: on every frame you describe the whole
screen by drawing widgets into a buffer. The library compares that buffer with
the previous frame and sends only the cells that changed to the terminal.

## Status

Early development, not yet published to opam.

Available today: layout by constraints, styled text with wrapping and
scrolling, keyboard, mouse, paste and resize events, a text cursor, correct
column widths for CJK, emoji and combining marks, a Unix backend and an
in-memory backend for tests, drawing on the whole screen or only in a few rows
below the prompt (an inline viewport), and these widgets: `Block`, `Paragraph`,
`Gauge`, `Line_gauge`, `Tabs`, `Item_list`, `Table`, `Scrollbar`, `Sparkline`,
`Bar_chart`, `Canvas`, `Chart`, `Text_field` and `Clear`.

Not there yet: Windows support.

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

Four larger examples live in [`examples/`](examples):
[`hello.ml`](examples/hello.ml) splits the screen into a menu and a greeting,
[`counter.ml`](examples/counter.ml) reacts to keys and resizes,
[`inline.ml`](examples/inline.ml) shows an installer's progress in a few rows
below the prompt, and [`dashboard.ml`](examples/dashboard.ml) has four tabs with gauges, a line
chart, a radar drawn on a canvas, a filter you can type in, a table, a
scrolling log and a quit dialog; it works with the keyboard and with the
mouse. Run one with
`dune exec examples/dashboard.exe`. This is `hello.exe`:

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

This is what `inline.exe` leaves in the terminal when it ends: the finished
packages were printed above the progress bars while it ran, nothing was
cleared, and the prompt comes back right below the last frame:

```
$ dune exec examples/inline.exe
✔ ocaml-base-compiler 5.5.1
✔ dune 3.24.2
✔ uucp 18.0.0
✔ uuseg 18.0.0
✔ alcotest 1.9.1
✔ qcheck-core 0.91
✔ ocamlformat 0.29.0
✔ odoc 3.2.1
Installing 8 packages
Everything is installed
Total                        ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

$
```

The Overview tab of the dashboard, with its gauges, a line gauge and bars that
move by themselves:

```
╭──────────────────────────────────────────────────────────────╮
│ Overview │ Charts │ Packages │ Logs                          │
╰──────────────────────────────────────────────────────────────╯
╭ CPU ───────────────────────────╮╭ CPU history ───────────────╮
│            ▌ 39%               ││                  ▁▃▄▄▃▁    │
╰────────────────────────────────╯│▂               ▂▆██████▃   │
╭ Memory ────────────────────────╮│█▆▃           ▃▇█████████▇▄ │
│       ▋      24%               ││███▇▄       ▄▇██████████████│
╰────────────────────────────────╯│██████▆▅▄▅▆█████████████████│
╭ Disk ──────────────────────────╮╰────────────────────────────╯
│              72%               │╭ Requests per hour ─────────╮
╰────────────────────────────────╯│                    ▁▁▁     │
╭ Network ───────────────────────╮│    ▄▄▄                     │
│44% ━━━━━━━━━━━━────────────────││                ▅▅▅         │
╰────────────────────────────────╯│▇▇▇         ▁▁▁             │
                                  │                            │
                                  │32  49  66  23  40  57      │
                                  │h0  h1  h2  h3  h4  h5      │
                                  ╰────────────────────────────╯
           Click a tab | Tab switches tab | Esc quits
```

Its Charts tab, where both series and the radar move by themselves:

```
╭──────────────────────────────────────────────────────────────╮
│ Overview │ Charts │ Packages │ Logs                          │
╰──────────────────────────────────────────────────────────────╯
╭ CPU and memory ────────────────────────╮ ╭ Radar ────────────╮
│%                                       │ │                   │
│100│                              CPU   │ │                   │
│   │⠉⠢⡀           ⡰⠉⠢⡀           ⡰Memory│ │    ⢀⣠⠤⠖⠒N⠒⠒⠤⣄⡀    │
│   │  ⢱         ⢀⠎   ⢱          ⡰⠁  ⢣   │ │  ⢠⠖⠉   ⣀⣸⣀   ⠉⠢⣄  │
│   │  ⠈⡆        ⡸ ⢀⡠⠤⠔⠊⢹       ⢰⠁   ⠸⡀  │ │ ⡴⠁ ⢀⡴⠚⠉ ⢸ ⠉⠓⢦⣀⡠⢊⣦ │
│   │   ⢸    ⢀⡠⠤⠔⠊⠉⠁   ⠘⢸      ⢀⠇     ⢣  │ │⣰⠁ ⢠⠎  ⣠⠴⢺⠦⣄⢀⢔⢵⡮⣕⡪⡇│
│ 50│ ⢀⣀⡠⠔⠒⠊⠉⠁  ⡸       ⢸⡀     ⡸      ⠈⢆ │ │⡇  ⡜  ⡸⠁ ⢸⣠⣾⢿⠟⠋⢫  ⢸│
│   │⠊⠁   ⢇    ⡰⠁       ⠘⡄    ⢀⠇       ⠘⡄│ │⡏⠉⠉⢫⠉⠉⢹⡉⠉⢹⠉⢉⡏⠉⠉⡝⠉⠉⢹│
│   │     ⠘⡄  ⢰⠁         ⡇⡄  ⢀⠎      ⣀⠤⠤⠒│ │⠹⡀ ⠘⢆  ⠙⠲⢼⠖⠋  ⡰⠣ ⢀⡇│
│   │      ⠘⣄⡠⠃          ⡇⠘⠤⡠⠊⣀⣀⠤⠤⠒⠉⠉    │ │ ⠳⡀ ⠈⠳⢤⣀ ⢸ ⣀⡤⠞⠁ ⢀⠞ │
│  0│                    ⣇⠤⠒⠒⠉           │ │  ⠘⠦⣀   ⠉⢹⠉   ⣀⠔⠋  │
│   └────────────────────────────────────│ │    ⠈⠙⠒⠦⠤⠼⠤⠤⠒⠋⠁    │
│    -15 s          -7.5 s            now│ │                   │
│                                    time│ │                   │
╰────────────────────────────────────────╯ ╰───────────────────╯
           Click a tab | Tab switches tab | Esc quits
```

And its Packages tab, after clicking it, typing `o` in the filter and
pressing Down:

```
╭──────────────────────────────────────────────────────────────╮
│ Overview │ Charts │ Packages │ Logs                          │
╰──────────────────────────────────────────────────────────────╯
╭ Filter ──────────────────────────────────────────────────────╮
│o                                                             │
╰──────────────────────────────────────────────────────────────╯
╭ Packages ────────────────────────────────────────────────────╮
│  Package                                 Version         Size│
│  alcotest                                1.9.1         1.2 MB│
│▶ ocaml-lsp-server                        1.27.0        9.8 MB│
│  ocamlformat                             0.29.0       14.1 MB│
│  odoc                                    3.2.1         6.3 MB│
│  qcheck-core                             0.91          820 KB│
│  utop                                    2.17.0        3.9 MB│
│                                                              │
│                                                              │
│                                                              │
│                                                              │
╰───────────────────────────────────────────────────── 6 of 10 ╯
      Type to filter | Up, Down or click move | Esc quits
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
new frame after each one. Mouse and paste events are asked for with
`Unix_backend.with_terminal ~features:[ Mouse; Bracketed_paste ]`, and
`Tabs.index_at`, `Item_list.index_at` and `Table.index_at` tell what is under a
click. Text is typed into a `Line_edit`, drawn by a `Text_field`.

A terminal takes the whole screen by default. With
`Unix_backend.with_terminal ~viewport:(Inline rows)` it stays on the normal
screen instead and draws in some rows below the cursor, leaving what is above
untouched, like a progress bar in the middle of a shell session.
`Terminal.insert_before` prints lines above those rows, where they stay as
history, and the last frame is kept when the program ends.

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
| `Widget`, `Stateful_widget`, `Frame`, `Terminal`, `Backend` | Drawing frames on the whole screen or in some rows below the prompt, composing widgets and sending the diff to a backend |
| `Test_backend` | An in-memory terminal to test widgets |
| `Event`, `Input_parser` | Keyboard, mouse, paste and resize events, and the parser that turns terminal bytes into them |
| `Alignment`, `Span`, `Line`, `Text`, `Wrap` | Styled text, and breaking long lines into rows |
| `Block`, `Paragraph`, `Gauge`, `Line_gauge`, `Tabs`, `Scrollbar`, `Sparkline`, `Bar_chart`, `Clear` | Widgets that keep no state |
| `Marker`, `Canvas`, `Chart` | Drawing with Braille patterns, half blocks and blocks: shapes and labels on a canvas, and datasets between two axes |
| `Selection`, `Item_list`, `Table` | Widgets with a selected row that scrolls |
| `Line_edit`, `Text_field` | A line of text being edited, and the box that shows it |
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
dune build @doc        # API docs, in _build/default/_doc/_html
```

The documentation build prints one warning for every `@raise` of a Stdlib or
Unix exception, because those libraries are not part of the generated pages.
Those warnings are harmless; any other one is a broken reference.

Branches follow `<type>/<short-description>` and commits follow
[Conventional Commits](https://www.conventionalcommits.org/), in English.
Changes worth knowing about go in [CHANGELOG.md](CHANGELOG.md).

## License

MIT. Camlembert derives from Ratatui, which is also MIT licensed; both
copyright notices are kept in [LICENSE](LICENSE).
