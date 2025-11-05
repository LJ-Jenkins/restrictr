#' Ensure the truth of data-masked R expressions
#'
#' If any of the expressions in `...`, evaluated within the data mask
#' `.data` (see [rlang data masking](rlang::args_data_masking)), are
#' not all `TRUE`, [rlang::abort] is called for the first expression
#' which was not ([all]) `TRUE`. The `.names` and `.size` arguments
#' can be used to check for given names and size of the
#' data.frame/list. The checking of size is from the [vctrs]
#' (https://vctrs.r-lib.org/) package (using [vctrs::vec_size]) and
#' thus applies [vctrs size rules]
#' (https://vctrs.r-lib.org/articles/type-size.html).
#'
#' @param .data a data.frame or list to use as the data mask.
#' @param ... any number of R expressions to be evaluated using `.data`
#' as a data mask, which should each evaluate to (a logical vector of [all])
#' `TRUE` for no error to occur. Positive numbers are not `TRUE`, even
#' when they are coerced to `TRUE` inside `if()` or in arithmetic
#' computations in R. If the expressions are named, the names will be
#' used in the error message. Names support [rlang injection]
#' (https://rlang.r-lib.org/reference/topic-inject.html) and [glue]
#' (https://glue.tidyverse.org/) interpreted string literals.
#' @param .na_rm if `TRUE`, NA values are removed in the logical vectors
#' before evaluation.
#' @param .names optional character vector of names which must be
#' present in the `.data` data.frame/list. Can be a glue string.
#' @param .size optional positive scalar integerish value for the size that
#' the `.data` data.frame/list must have.
#' @param .message single default error message for non-named expressions.
#' Can be a glue string.
#' @param .error_call the call environment to use for error messages
#' (passed to [rlang::abort]).
#' @param .darg the argument name of `.data` to use in error messages.
#' @details See [schema_cast](restrictr::schema_cast) and [schema_recycle]
#' (restrictr::schema_recycle) for versions of `schema()` that attempt
#' to coerce named elements of the data to the desired type/size. See
#' [abort_if_not](restrictr::abort_if_not) for a non-data-masked version
#' of this function. [restrict](restrictr::restrict) can also be used for
#' type casting, size recycling, and validation.
#' @return .data is returned with attached class "with_schema" and
#' attribute "schema" containing the schema call to be enforced later.
#' @export
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' li <- list(x = 1, y = "hi", z = \(x) x > 1)
#' li <- schema(li, x == 1, is.character(y), is.function(z)) # all TRUE
#'
#' schema(li, x == 1, is.numeric(y)) |> try()
#' # Error:
#' # Caused by error in `schema()`:
#' # ℹ In argument: `is.numeric(y)` for data mask `li`.
#' # ! Returned `FALSE`.
#'
#' # A custom error message can be given for each expression:
#' schema(li, "y must be numeric, check input" = is.numeric(y)) |> try()
#' # Error:
#' # Caused by error in `schema()`:
#' # ℹ In argument: `is.numeric(y)` for data mask `li`.
#' # ! y must be numeric, check input
#'
#' # Alternatively, one error message can be used for all expressions:
#' schema(
#'   li,
#'   x == 1, is.character(y), is.integer(z),
#'   .message = "li is invalid, check input"
#' ) |> try()
#' # Error:
#' # Caused by error in `schema()`:
#' # ℹ In argument: `is.integer(z)` for data mask `li`.
#' # ! li is invalid, check input
#'
#' # Option to remove NA values before checking:
#' df <- data.frame(x = c(5, NA, 10))
#' df <- schema(df, x > 4, .na_rm = TRUE) # no error
#'
#' # `.names` and `.size` arguments can be used to check that given names
#' # are present and that the data has the desired (vctrs) size:
#' schema(li, .names = c("a", "x", "y", "b")) |> try()
#' # Error:
#' # Caused by error in `schema()`.
#' # ! Named elements `a` and `b` not found in data mask `li`.
#'
#' schema(li, .size = 5) |> try()
#' # Error:
#' # Caused by error in `schema()`.
#' # ! Object `li` is of vctrs size `3`, not `5`.
#'
#' # The `.error_call` argument can be used to specify where the error occurs,
#' # by default this is the caller environment.
#' myfunc <- function(df, ...) schema(df, ...)
#' myfunc(df, x > 4) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `schema()`:
#' # ℹ In argument: `x > 4` for data mask `df`.
#' # ! Contains `NA` values and `.na_rm` is set to `FALSE`.
#'
#' # Injection and glue can be used:
#' y <- "my error"
#' schema(li, "{y}" = x == 2) |> try()
#' schema(li, {{ y }} := x == 2) |> try()
#' schema(li, !!y := x == 2) |> try()
#' schema(li, x == 2, .message = "{y}") |> try()
#' # Error:
#' # Caused by error in `schema()`:
#' # ℹ In argument: `x == 2` for data mask `li`.
#' # ! my error
#' y <- list("my bang-bang-bang error" = rlang::expr(x == 2))
#' schema(li, !!!y) |> try()
#' # Error:
#' # Caused by error in `schema()`:
#' # ℹ In argument: `x == 2` for data mask `li`.
#' # ! my bang-bang-bang error
#' @export
schema <- function(.data, ...) {
  UseMethod("schema", .data)
}

#' @rdname schema
#' @export
schema.data.frame <- function(
    .data,
    ...,
    .na_rm = FALSE,
    .names = NULL,
    .size = NULL,
    .message = NULL,
    .error_call = caller_env(),
    .darg = caller_arg(.data)) {
  check_masked_logi_exprs(
    .data,
    ...,
    .names = .names,
    .size = .size,
    .message = .message,
    .na_rm = .na_rm,
    .darg = .darg,
    .error_call = .error_call,
    restrictr_fn = "schema"
  )
}

#' @rdname schema
#' @export
schema.list <- function(
    .data,
    ...,
    .na_rm = FALSE,
    .names = NULL,
    .size = NULL,
    .message = NULL,
    .error_call = caller_env(),
    .darg = caller_arg(.data)) {
  check_masked_logi_exprs(
    .data,
    ...,
    .names = .names,
    .size = .size,
    .message = .message,
    .na_rm = .na_rm,
    .darg = .darg,
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
  eval_env <- caller_env(2)

  validate_env(
    .error_call,
    allow_global = TRUE,
    call = eval_env,
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

  if (!is.null(.names)) {
    .names <- .names |>
      glue_chr(
        eval_env = eval_env,
        error_call = .error_call,
        restrictr_fn = restrictr_fn
      )
    check_names_present(
      .data,
      .names,
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

  attr(.data, "schema") <- structure(
    list(
      exprs = tf,
      arg_names = nms,
      message = .message,
      na_rm = .na_rm,
      mask_names = .names,
      mask_size = .size
    ),
    class = "restrictr:::schema"
  )
  class(.data) <- c("with_schema", class(.data))

  return(.data)
}
