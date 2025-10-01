`%""%` <- function(lhs, rhs) if (lhs == "") rhs else lhs

`%!||%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

prmask <- function(mask) {
  mask %!||% format_inline(" for data mask {.var {mask}}")
}

r_prmask <- function(arg, mask) {
  x <- "Invalid validation for"
  if (is.na(mask)) {
    x <- c(x, " {.var {arg}}:")
  } else {
    x <- c(x, " {.var {arg}} in data mask {.var {mask}}:")
  }
  format_inline(x)
}

length_or_obj <- function(x) {
  if (length(x) > 1) {
    y <- "length {.var {length(x)}}"
  } else {
    y <- "{.var {x}}"
  }
  format_inline(y)
}

glue_chr <- function(chr, eval_call, calling_fn, error_class, error_call) {
  x <- c()
  try_fetch(
    for (i in seq_along(chr)) {
      x[i] <- glue(chr[i], .envir = eval_call)
    },
    error = function(cnd) {
      parent_error(
        chr[i],
        NULL,
        cnd,
        calling_fn,
        error_class,
        error_call
      )
    }
  )
  x
}

glue_names <- function(obj, eval_call, calling_fn, error_class, error_call) {
  names2(obj) |>
    glue_chr(
      eval_call = eval_call,
      calling_fn = calling_fn,
      error_class = error_class,
      error_call = error_call
    )
}

restrict_list_c <- function(li, names, new_name = "validations") {
  c(
    li[names %in% c("type", "size", "mask", "lossy", "na_rm")],
    set_names(list(li[!names %in% c("type", "size", "mask", "lossy", "na_rm")]), new_name)
  )
}

#' @importFrom utils capture.output
ptype_show <- function(.x) {
  gsub("Prototype: ", "", capture.output(vec_ptype_show(.x)))
}
