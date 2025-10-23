excl <- function(txt) {
  c("!" = txt)
}

cnd_masked_arg <- function(arg, name, mask) {
  x <- if (is.null(name) || name == "") {
    paste0("`", arg, "`")
  } else {
    paste0("`", name, " = ", arg, "`")
  }

  paste0(
    "In argument: ", x,
    if (!is.null(mask) && !is.na(mask)) {
      paste0(" for data mask ", "`", mask, "`.")
    } else {
      "."
    }
  )
}

cnd_bullets <- function(cnd, restrictr_fn, arg, name = NULL, msg = NULL, mask = NULL) {
  c(
    NULL = format_inline("{.strong Caused by error in {.fn {restrictr_fn}}}:"),
    "i" = cnd_masked_arg(arg, name, mask),
    "!" = msg %||% if (inherits(cnd, "restrictr_error")) {
      cnd_body(cnd)
    } else {
        unname(cnd_header(cnd)) %le0% cnd$message
    },
    NULL = if (inherits(cnd, "restrictr_error")) {
      cnd_footer(cnd)
    } else {
      unname(cnd_body(cnd))
    }
  )
}

#' @export
cnd_header.restrictr_error <- function(cnd, ...) {
  c(NULL = format_inline("{.strong Caused by error in {.fn {cnd$restrictr_fn}}}."))
}

#' @export
cnd_body.restrictr_error <- function(cnd, ...) {
  conditionMessage(cnd) |> excl()
}

#' @export
cnd_body.restrictr_error_env <- function(cnd, ...) {
  "`env` must be an environment." |> excl()
}

#' @export
cnd_body.restrictr_error_global_env <- function(cnd, ...) {
  "`env` must not be the global environment." |> excl()
}

#' @export
cnd_body.restrictr_error_bool <- function(cnd, ...) {
  format_inline("{.var {cnd$bool}} must be {.var TRUE} or {.var FALSE}.") |> excl()
}

#' @export
cnd_body.restrictr_error_chr <- function(cnd, ...) {
  format_inline("{.var {cnd$chr}} must be a character vector.") |> excl()
}

#' @export
cnd_body.restrictr_error_no_args_given <- function(cnd, ...) {
  "No arguments were provided." |> excl()
}

#' @export
cnd_body.restrictr_error_args_unnamed <- function(cnd, ...) {
  format_inline("Arguments are not named with objects to {cnd$action} in {qty(cnd$i)}position{?s}: {.var {cnd$i}}.") |> excl()
}

#' @export
cnd_body.restrictr_error_size_arg <- function(cnd, ...) {
  format_inline("Size argument is not {cnd$size_issue}: {cnd$size_given} given.") |> excl()
}

#' @export
cnd_body.restrictr_error_mask_arg <- function(cnd, ...) {
  format_inline("Mask object {.var {cnd$mask_name}} does not contain named element {.var {cnd$arg}}.") |> excl()
}

#' @export
cnd_body.restrictr_error_args_env <- function(cnd, ...) {
  format_inline("{qty(cnd$not_found)}Object{?s} {.var {cnd$not_found}} {?is/are} not found in the {.var .env} environment specified.") |> excl()
}

#' @export
cnd_body.restrictr_error_size_false <- function(cnd, ...) {
  format_inline("Object {.var {cnd$arg}} is of {.pkg vctrs} size {.var {cnd$actual_size}}, not {.var {cnd$expected_size}}.") |> excl()
}

#' @export
cnd_body.restrictr_error_masked_names_not_present <- function(cnd, ...) {
  format_inline("Named {qty(cnd$not_found)}element{?s} {.var {cnd$not_found}} not found in data mask {.var {cnd$mask}}.") |> excl()
}

#' @export
cnd_body.restrictr_error_restrict_args <- function(cnd, ...) {
  cli_bullets(c(
    i = "With named argument {.var {cnd$arg_name}}.",
    "!" = "calls must be built using the functions: {.fn {cnd$allowed}}, not {.fn {cnd$given}}."
  ))
}

#' @export
cnd_body.restrictr_error_restrict_lossy <- function(cnd, ...) {
  format_inline("{.var lossy} argument can only be given for {.fn coerce} calls, not {.fn {cnd$given_fn}}.") |> excl()
}

#' @export
cnd_body.restrictr_error_wrong_class <- function(cnd, ...) {
  if (is.null(cnd$validate)) {
    format_inline("Returned {.cls {cnd$given_class}}, not {.cls {cnd$expected_class}}.")
  } else {
    format_inline("Validate {.var {cnd$validate}} is of type {.cls {cnd$given_class}}, not {.cls {cnd$expected_class}}.")
  }
}

#' @export
cnd_body.restrictr_error_glue <- function(cnd, ...) {
  c(i = format_inline("In glue expression: {.var {cnd$expr}}."))
}

#' @export
cnd_body.restrictr_error_na_present <- function(cnd, ...) {
  if (is.null(cnd$validate)) {
    "Contains `NA` values and `na_rm` is set to `FALSE`."
  } else {
    format_inline("Validate {.var {cnd$validate}} contains `NA` values and `na_rm` is set to `FALSE`.")
  }
}

#' @export
cnd_body.restrictr_error_empty <- function(cnd, ...) {
  if (is.null(cnd$validate)) {
    "Returned an empty vector."
  } else {
    format_inline("Validate {.var {cnd$validate}} returned an empty vector.")
  }
}

#' @export
cnd_body.restrictr_error_true_false <- function(cnd, ...) {
  format_inline("Returned {.var {if (cnd$check_false) 'FALSE' else 'TRUE'}}.")
}

#' @export
cnd_body.restrictr_error_validate_false <- function(cnd, ...) {
  cnd$vname %""% format_inline("Validate {.var {cnd$validate}} returned {.var FALSE}.")
}
