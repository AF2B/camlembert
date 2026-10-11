#include <sys/ioctl.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* What ioctl returns when it fails. */
static const int IOCTL_FAILURE = -1;

/* Layout of the (columns, rows) pair returned to OCaml. */
enum {
  SIZE_FIELD_COUNT = 2,
  COLUMNS_FIELD = 0,
  ROWS_FIELD = 1
};

/* The OCaml Unix library has no binding for ioctl, so the window size of the
   terminal attached to standard output is read here. */
CAMLprim value camlembert_terminal_size(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(size);
  struct winsize window;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &window) == IOCTL_FAILURE) {
    caml_failwith("Tty.size: standard output is not a terminal");
  }

  size = caml_alloc_tuple(SIZE_FIELD_COUNT);
  Store_field(size, COLUMNS_FIELD, Val_int(window.ws_col));
  Store_field(size, ROWS_FIELD, Val_int(window.ws_row));
  CAMLreturn(size);
}
