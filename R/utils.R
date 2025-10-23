`%""%` <- function(lhs, rhs) if (lhs == "") rhs else lhs

`%!||%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

`%le0%` <- function(lhs, rhs) if (length(lhs) != 0) lhs else rhs

prmask <- function(mask) {
  mask %!||% format_inline(" for data mask {.var {mask}}")
}

r_prmask <- function(arg, mask) {
  format_inline(
    if (is.na(mask)) {
      "Invalid validation for {.var {arg}}:"
    } else {
      "Invalid validation for {.var {arg}} in data mask {.var {mask}}:"
    }
  )
}

length_or_obj <- function(x) {
  format_inline(
    if (length(x) > 1) {
      "length {.var {length(x)}}"
    } else {
      "{.var {x}}"
    }
  )
}

glue_chr <- function(chr, eval_env, error_call, restrictr_fn) {
  x <- c()
  withCallingHandlers(
    for (i in seq_along(chr)) {
      x[i] <- glue(chr[i], .envir = eval_env)
    },
    error = function(cnd) {
      abort_glue(
        cnd,
        expr = chr[i],
        restrictr_fn = restrictr_fn,
        call = error_call
      )
    }
  )
  x
}

glue_names <- function(obj, eval_env, error_call, restrictr_fn) {
  names2(obj) |>
    glue_chr(
      eval_env = eval_env,
      error_call = error_call,
      restrictr_fn = restrictr_fn
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

caller_name <- function(n = 1) {
  x <- caller_call(n)
  paste0(call_ns(x) %!||% paste0(call_ns(x), "::"), call_name(x))
}
