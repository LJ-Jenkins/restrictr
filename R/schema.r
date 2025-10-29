#' Ensure data.frame/list elements conform to a given schema
#'
#' If any of the data-masked expressions in `...` are not all `TRUE`,
#' [rlang::abort] is called for the first expression which was not
#' (all) `TRUE`. `.names` and `.size` arguments can be used to check for
#' given names and size of the data.frame/list. The checking of size is from
#' the [vctrs](https://vctrs.r-lib.org/) package (using [vctrs::vec_size]) and
#' thus applies vctrs size rules.
#'
#' @param .data a data.frame or list to check the schema of.
#' @param ... any number of R expressions to be evaluated using `.data`
#' as a data-mask, which should each evaluate to (a logical vector of all)
#' `TRUE` for no error to occur.
#' @param .names optional character vector of names which must be
#' present in the data.frame/list.
#' @param .size optional scalar integerish value for the size of that
#' the data.frame/list must have.
#' @param .message single default error message for non-named expressions.
#' @param .na_rm if TRUE, NA values are removed in the logical vectors (default is FALSE)
#' @param .error_call the call environment to use for the error (passed to [rlang::abort]).
#' @details [schema_cast][restrictr::schema_cast] and [schema_recycle][restrictr::schema_recycle]
#' are versions of `schema()` that attempt to coerce the data to the desired schema.
#' @export
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' li <- list(x = 1, y = "hi", z = \(x) x > 1)
#' li |>
#'   schema(x == 1, is.character(y), is.function(z)) # all TRUE
#'
#' li |>
#'   schema(x == 1, is.numeric(y)) |>
#'   try()
#' # => Error: Argument `is.numeric(y)` for data mask `.data` returned `FALSE`.
#'
#' li |>
#'   schema(length(x)) |>
#'   try()
#' # => Error: Expression `length(x)` for object `.data` must evaluate to class
#' # <logical> not <integer>.
#' # even when if(1) "ok" works
#'
#' # The default error message can be overridden to be more informative:
#' df <- data.frame(a = 1L:3L, b = c("x", "y", "z"))
#' df |>
#'   schema("a must be double" = is.double(a)) |>
#'   try()
#' # => Error: a must be double
#'
#' # Alternatively, one error message can be used for all expressions:
#' df |>
#'   schema(
#'     is.integer(a),
#'     !grepl("x", b),
#'     .message = "a must be integer and b cannot contain 'x'."
#'   ) |>
#'   try()
#' # => Error: a must be integer and b cannot contain 'x'.
#'
#' # injection and glue can be used to supply expressions, names, and messages:
#' x <- "my error"
#' schema(df, "{x}" = FALSE) |> try()
#' # => Error: my error
#' y <- FALSE
#' schema(df, {{ x }} := !!y) |> try()
#' # => Error: my error
#' schema(df, !!x := !is.character(b)) |> try()
#' # => Error: my error
#' x <- list("my error" = FALSE)
#' schema(df, !!!x) |> try()
#' # => Error: my error
#' @export
schema <- function(.data, ...) {
  UseMethod("schema", .data)
}

#' @rdname schema
#' @export
schema.data.frame <- function(
    .data,
    ...,
    .names = NULL,
    .size = NULL,
    .message = NULL,
    .na_rm = FALSE,
    .error_call = caller_env()) {
  check_masked_logi_exprs(
    .data,
    ...,
    .names = .names,
    .size = .size,
    .message = .message,
    .na_rm = .na_rm,
    .darg = caller_arg(.data),
    .error_call = .error_call,
    restrictr_fn = "schema"
  )
}

#' @rdname schema
#' @export
schema.list <- function(
    .data,
    ...,
    .names = NULL,
    .size = NULL,
    .message = NULL,
    .na_rm = FALSE,
    .error_call = caller_env()) {
  check_masked_logi_exprs(
    .data,
    ...,
    .names = .names,
    .size = .size,
    .message = .message,
    .na_rm = .na_rm,
    .darg = caller_arg(.data),
    .error_call = .error_call,
    restrictr_fn = "schema"
  )
}

check_masked_logi_exprs <- function(
    .data,
    ...,
    .names,
    .size,
    .message,
    .na_rm,
    .darg,
    .error_call,
    restrictr_fn = NULL) {
  tf <- enquos(...)

  validate_env(
    .error_call,
    allow_global = TRUE,
    call = caller_env(2),
    restrictr_fn = restrictr_fn
  )
  validate_chr(
    .names,
    allow_null = TRUE,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_size_arg(
    .size,
    .darg,
    allow_null = TRUE,
    sname = "`.size`",
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_chr(
    .message,
    allow_null = TRUE,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_bool(
    .na_rm,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )

  if (!is.null(.size)) {
    check_size_true(
      .data,
      .size,
      .darg,
      call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  eval_env <- caller_env(2)

  if (!is.null(.names)) {
    check_names_present(
      .data,
      .names |>
        glue_chr(
          eval_env = eval_env,
          error_call = .error_call,
          restrictr_fn = restrictr_fn
        ),
      .darg,
      call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  if (length(tf) == 0) {
    invisible(NULL)
  }

  nms <- glue_names(
    tf,
    eval_env = eval_env,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )

  if (!is.null(.message)) {
    .message <- glue_chr(
      .message,
      eval_env = eval_env,
      error_call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  check_logi_exprs(
    .data,
    tf,
    nms = nms,
    check_false = TRUE,
    message = .message,
    na_rm = .na_rm,
    error_call = .error_call,
    restrictr_fn = restrictr_fn,
    darg = .darg
  )
}
