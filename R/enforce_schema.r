#' Enforce an attached schema
#'
#' For objects with an attached schema (from [schema], [schema_cast], or
#' [schema_recycle]), enforce the schema on the object.
#'
#' @param .data a data.frame or list containing an attached schema (of
#' class `with_schema`).
#' @param ... for future extensions - leave blank.
#' @param .error_call the call environment to use for error messages
#' (passed to [rlang::abort]).
#' @param .darg the argument name of `.data` to use in error messages.
#' @details See [restrictr::schema], [restrictr::schema_cast]
#' and [restrictr::schema_recycle] for creating and attaching a schema.
#' @return Object `.data`, with no change if attached schema is from `schema`,
#' or with named elements cast or recycled to the desired type/size if
#' attached schema is from `schema_cast` or `schema_recycle` respectively.
#' @export
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' li <- list(x = 1, y = "hi")
#' li_with_schema <- schema(li, x == 1, is.character(y))
#'
#' li_with_schema$y <- 1
#' enforce_schema(li_with_schema) |> try()
#' # Error:
#' # Caused by error in `enforce_schema()`:
#' # ℹ In argument: `is.character(y)` for data mask `li_with_schema`.
#' # ! Returned `FALSE`.
#'
#' df <- data.frame(x = 1:2)
#' df_with_schema <- schema_cast(df, x = integer(), .lossy = TRUE)
#'
#' df_with_schema$x <- c(1.5, 2.5)
#' enforce_schema(df_with_schema)$x
#' # 1 2
#'
#' li_with_schema <- schema_recycle(li, x = 2, y = 3)
#' li_with_schema$y <- "hi"
#' enforce_schema(li_with_schema)$y
#' # "hi" "hi" "hi"
#'
#' # The `.error_call` argument can be used to specify where the error occurs,
#' # by default this is the caller environment.
#' myfunc <- function(.x) enforce_schema(.x)
#' li_with_schema$x <- 1:3
#' myfunc(li_with_schema) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `enforce_schema()`:
#' # ℹ In argument: `x = 2` for data mask `li`.
#' # ! Can't recycle `x` (size 3) to size 2.
#' @export
enforce_schema <- function(.data, ...) {
  UseMethod("enforce_schema", .data)
}

#' @export
enforce_schema.default <- function(
    .data,
    ...,
    .error_call = caller_env(),
    .darg = caller_arg(.data)) {
  abort(
    c(
      NULL = "{.strong Caused by error in {.fn enforce_schema}}:",
      "!" = "{.var { .darg}} does not have an attached schema."
    ),
    call = .error_call
  )
}

#' @rdname enforce_schema
#' @export
enforce_schema.with_schema <- function(
    .data,
    ...,
    .error_call = caller_env(),
    .darg = caller_arg(.data)) {
  dispatch_schema(
    attr(.data, "schema"),
    .data,
    .error_call,
    .darg,
    restrictr_fn = "enforce_schema",
    ...
  )
}

dispatch_schema <- function(
    schema,
    .data,
    .error_call,
    .darg,
    restrictr_fn,
    ...) {
  UseMethod("dispatch_schema", schema)
}

#' @export
`dispatch_schema.restrictr:::schema` <- function(
    schema,
    .data,
    .error_call,
    .darg,
    restrictr_fn,
    ...) {
  if (!is.null(schema$mask_size)) {
    check_size_true(
      .data,
      schema$mask_size,
      .darg,
      call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  if (!is.null(schema$mask_names)) {
    check_names_present(
      .data,
      schema$mask_names,
      .darg,
      call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  if (length(schema$exprs) == 0) {
    invisible(.data)
  }

  check_logi_exprs(
    .data,
    schema$exprs,
    nms = schema$arg_names,
    check_false = TRUE,
    message = schema$message,
    na_rm = schema$na_rm,
    error_call = .error_call,
    restrictr_fn = restrictr_fn,
    darg = .darg
  )

  invisible(.data)
}

#' @export
`dispatch_schema.restrictr:::schema_cast` <- function(
    schema,
    .data,
    .error_call,
    .darg,
    restrictr_fn,
    ...) {
  if (!is.null(schema$mask_size)) {
    check_size_true(
      .data,
      schema$mask_size,
      .darg,
      call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  if (!is.null(schema$mask_names)) {
    check_names_present(
      .data,
      schema$mask_names,
      .darg,
      call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  if (length(schema$exprs) == 0) {
    invisible(.data)
  }

  .data <- cast_exprs(
    .data,
    schema$exprs,
    nms = schema$arg_names,
    lossy = schema$lossy,
    eval_env = caller_env(3),
    error_call = .error_call,
    darg = .darg,
    restrictr_fn = restrictr_fn
  )

  invisible(.data)
}

#' @export
`dispatch_schema.restrictr:::schema_recycle` <- function(
    schema,
    .data,
    .error_call,
    .darg,
    restrictr_fn,
    ...) {
  if (!is.null(schema$mask_size)) {
    check_size_true(
      .data,
      schema$mask_size,
      .darg,
      call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  if (!is.null(schema$mask_names)) {
    check_names_present(
      .data,
      schema$mask_names,
      .darg,
      call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  if (length(schema$exprs) == 0) {
    invisible(.data)
  }

  .data <- recycle_exprs(
    .data,
    schema$exprs,
    nms = schema$arg_names,
    eval_env = caller_env(3),
    error_call = .error_call,
    darg = .darg,
    restrictr_fn = restrictr_fn
  )

  invisible(.data)
}
