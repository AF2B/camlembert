#!/usr/bin/env bash
# Takes the screenshots used by the README.
#
# Every scene runs an example in a private tmux server, so nothing of the
# user's own tmux is touched. The screen is captured with its colors, drawn as
# a terminal window by render.py and turned into a PNG by rsvg-convert.
#
# Needs tmux, python3, rsvg-convert, dune and the Adwaita Mono font. The hello
# example and the log tab of the dashboard show CJK text, so they are left out:
# they need a font with those glyphs, which Adwaita Mono does not have.
# Run it from anywhere:  tools/screenshots/capture.sh [scene ...]
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
IMAGES="$ROOT/docs/images"
WORK="$(mktemp -d)"
TMUX_SERVER="camlembert_screenshots"
SESSION="shot"
ZOOM=2
PROMPT='\[\e[1;32m\]$\[\e[0m\] '

trap 'tmux -L "$TMUX_SERVER" kill-server 2>/dev/null || true; rm -rf "$WORK"' EXIT

tmx() { tmux -L "$TMUX_SERVER" "$@"; }

for tool in tmux python3 rsvg-convert dune; do
  command -v "$tool" >/dev/null || { echo "missing tool: $tool" >&2; exit 1; }
done

# start_session COLUMNS ROWS: a clean shell with a short prompt, in the repository.
start_session() {
  tmx kill-server 2>/dev/null || true
  sleep 0.6
  tmx new-session -d -s "$SESSION" -x "$1" -y "$2" -c "$ROOT" 'bash --norc --noprofile'
  sleep 0.4
  tmx send-keys -t "$SESSION" "PS1='$PROMPT'" Enter
  tmx send-keys -t "$SESSION" clear Enter
  sleep 0.3
}

# type_line TEXT: types a line and presses Enter.
type_line() { tmx send-keys -t "$SESSION" -l "$1"; tmx send-keys -t "$SESSION" Enter; }

# press KEY...: presses tmux keys, with a short pause after each one.
press() {
  for key in "$@"; do
    tmx send-keys -t "$SESSION" "$key"
    sleep 0.35
  done
}

# snap NAME TITLE: captures the screen as docs/images/NAME.png.
snap() {
  local name="$1" title="$2"
  tmx capture-pane -t "$SESSION" -e -p > "$WORK/$name.ans"
  python3 "$ROOT/tools/screenshots/render.py" "$WORK/$name.ans" "$WORK/$name.svg" --title "$title"
  mkdir -p "$IMAGES"
  rsvg-convert --zoom "$ZOOM" -o "$IMAGES/$name.png" "$WORK/$name.svg"
  echo "wrote docs/images/$name.png"
}

# dashboard [ROWS]: starts the dashboard in a window 100 columns wide.
dashboard() { start_session 100 "${1:-28}"; type_line "dune exec examples/dashboard.exe"; sleep 2; }

scene_dashboard_overview() { dashboard; snap dashboard-overview "camlembert · examples/dashboard"; }
scene_dashboard_charts()   { dashboard; press Tab; sleep 1; snap dashboard-charts "camlembert · examples/dashboard"; }
scene_dashboard_packages() {
  dashboard 19; press Tab Tab
  tmx send-keys -t "$SESSION" -l "o"; sleep 0.4; press Down Down
  snap dashboard-packages "camlembert · examples/dashboard"
}
scene_dashboard_popup() { dashboard; press Escape; sleep 0.5; snap dashboard-popup "camlembert · examples/dashboard"; }

scene_counter() {
  start_session 64 14
  type_line "dune exec examples/counter.exe"; sleep 1.5
  press Up Up Up Up
  snap counter "camlembert · examples/counter"
}

# The inline installer takes about thirteen seconds: one picture while it
# runs and one when it is done.
scene_inline() {
  start_session 80 18
  type_line "echo 'Building the library...'"
  type_line "echo 'Done.'"
  type_line "dune exec examples/inline.exe"
  sleep 6
  snap inline-running "camlembert · examples/inline"
  sleep 9
  snap inline-done "camlembert · examples/inline"
}

ALL=(dashboard_overview dashboard_charts dashboard_packages dashboard_popup counter inline)
SCENES=("${@:-${ALL[@]}}")
for scene in "${SCENES[@]}"; do
  "scene_${scene//-/_}"
done
