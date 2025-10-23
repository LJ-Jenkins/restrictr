#' Ensure list elements are of a specific size and recycle them if not
#'
#' If any of the data-masked named expressions in `...` are not the specified size,
#' then the object attempts to be recycled to the size specified in the expression.
#' `.names` and `.size` arguments can be used to check for given names and size of the
#' list. The checking of size and the recycling are from the [vctrs](https://vctrs.r-lib.org/)
#' package (using [vctrs::vec_size] and [vctrs::vec_recycle]) and thus apply the
#' [vctrs recycling rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html).
#'
#' @param .data a list to check the lengths schema of.
#' @param ... any number of [`data-masking`][rlang::args_data_masking] name-value pairs
#' to be evaluated using `.data` as a data-mask. Should follow the format of
#' `name = expected_size`, e.g, `var_x = 10L` or `var_x = var_y`.
#' @param .names optional character vector of names which must be
#' present in the data.frame/list.
#' @param .size optional scalar integerish value for the size that
#' the data.frame/list must have.
#' @param .error_call the call environment to use for the error (passed to [rlang::abort]).
#' @details See also [schema][restrictr::schema] and [schema_recycle][restrictr::schema_recycle],
#' as well as [cast_if_not][restrictr::cast_if_not] for a non-data-masked version of casting.
#' @export
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' li <- list(x = 1, y = "hi", z = 1:2)
#' # li$x and li$y are recycled.
#' li |>
#'   schema_recycle(x = 5, y = 3)
#'
#' li |>
#'   schema_recycle(z = 5) |>
#'   try()
#' # => Error: Can't recycle `z` (size 2) to size 5.
#'
#' li |>
#'   schema_recycle(x = "hi") |>
#'   try()
#' # => Size argument for `z` is not numeric: class <character>.
#'
#' # schema_recycle works sequentially with quosures, so references to objects will
#' # be after they have been evaluated:
#' li |>
#'   schema_recycle(x = vctrs::vec_size(z), y = vctrs::vec_size(x))
#'
#' li |>
#'   schema_recycle(x = 1, .size = 5) |>
#'   try()
#' # => Error: Object `li` must have vctrs size `5`, not `3`.
#'
#' li |>
#'   schema_recycle(x = 1, .names = c("x", "p")) |>
#'   try()
#' # => Error: Names `p` not found in `li`.
#'
#' # injection and glue can be used to supply expressions, names, and messages:
#' li <- list(x = 1, z = 5)
#' x_name <- "x"
#' schema_recycle(li, !!x_name := z)
#' li$x <- 1:2
#' xg_name <- "{x_name}"
#' schema_recycle(li, {{ xg_name }} := 10) |> try()
#' # => Error: Can't recycle `x` (size 2) to size 10.
#' @export
schema_recycle <- function(.data, ...) {
  UseMethod("schema_recycle", .data)
}

#' @rdname schema_recycle
#' @export
schema_recycle.list <- function(
    .data,
    ...,
    .names = NULL,
    .size = NULL,
    .error_call = caller_env()) {
  recycle_masked_exprs(
    .data, ...,
    .names = .names,
    .size = .size,
    .darg = caller_arg(.data),
    .error_call = .error_call,
    restrictr_fn = "schema_recycle"
  )
}

recycle_masked_exprs <- function(
    .data,
    ...,
    .names,
    .size,
    .darg,
    .error_call,
    restrictr_fn = NULL) {
  qs <- enquos(...)

  validate_env(
    .error_call,
    allow_global = TRUE,
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

  if (length(qs) == 0) {
    return(.data)
  }

  nms <- glue_names(
    qs,
    eval_env = eval_env,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )

  validate_args_named(
    nms,
    "recycle",
    call = .error_call,
    restrictr_fn = restrictr_fn
  )

  check_names_present(
    .data,
    nms,
    .darg,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )

  .data <- recycle_exprs(
    .data,
    qs,
    nms,
    eval_env = eval_env,
    error_call = .error_call,
    darg = .darg,
    restrictr_fn = restrictr_fn
  )

  return(.data)
}
