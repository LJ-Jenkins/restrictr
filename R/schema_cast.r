#' Ensure data.frame/list elements are of a specific type and cast them if not
#'
#' If any of the data-masked named expressions in `...` are not of the same type,
#' then the object attempts to be cast to the type specified in the expression.
#' `.names` and `.size` arguments can be used to check for given names and size
#' of the data.frame/list. The checking of type and the type conversion are from the
#' [vctrs](https://vctrs.r-lib.org/) package (using [vctrs::vec_is]
#' and [vctrs::vec_cast]) and thus stick to the [vctrs type conversion rules](https://vctrs.r-lib.org/reference/faq-compatibility-types.html).
#' The checking of size is also from [vctrs](https://vctrs.r-lib.org/)
#' (using [vctrs::vec_size]) and thus applies vctrs size rules.
#'
#' @param .data a data.frame or list to check the types schema of.
#' @param ... any number of [`data-masking`][rlang::args_data_masking]
#' name-value pairs to be evaluated using `.data` as a data-mask.
#' Should follow the format of `name = expected_type()`,
#' e.g, `var_x = integer()` or `var_x = var_y`.
#' @param .lossy logical, if `TRUE` allow [lossy casts][vctrs::allow_lossy_cast].
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
#' li <- list(x = 1.1, y = "hi", z = 1L:2L)
#' # input remains the same if types match
#' li |>
#'   schema_cast(x = double(), y = character(), z = integer()) |>
#'   lapply(class)
#'
#' li |>
#'   schema_cast(y = numeric()) |>
#'   try()
#' # => Error: Can't convert `y` <character> to <double>.
#'
#' li |>
#'   schema_cast(x = z) |>
#'   try()
#' # => Error: Can't convert from `x` <double> to <integer> due to loss of precision.
#'
#' # with lossy casting
#' li |>
#'   schema_cast(x = z, .lossy = TRUE) |>
#'   lapply(class)
#'
#' # schema_cast works sequentially with quosures, so references to objects will be
#' # after they have been evaluated:
#' li$a <- 1L
#' li |>
#'   schema_cast(z = x, a = z) |>
#'   lapply(class)
#'
#' li |>
#'   schema_cast(x = numeric(), .size = 5) |>
#'   try()
#' # => Error: Object `li` must have vctrs size `5`, not `4`.
#'
#' li |>
#'   schema_cast(x = numeric(), .names = c("x", "p")) |>
#'   try()
#' # => Error: Names `p` not found in `li`.
#'
#' # injection and glue can be used to supply expressions, names, and messages:
#' li <- list(x = 1L, z = 5.5)
#' x_name <- "x"
#' schema_cast(li, !!x_name := z) |>
#'   lapply(class)
#' xg_name <- "{x_name}"
#' schema_cast(li, {{ xg_name }} := character()) |> try()
#' # => Error: Can't convert `x` <integer> to <character>.
#' @export
schema_cast <- function(.data, ...) {
  UseMethod("schema_cast", .data)
}

#' @rdname schema_cast
#' @export
schema_cast.data.frame <- function(
    .data,
    ...,
    .lossy = FALSE,
    .names = NULL,
    .size = NULL,
    .error_call = caller_env()) {
  cast_masked_exprs(
    .data,
    ...,
    .lossy = .lossy,
    .names = .names,
    .size = .size,
    .darg = caller_arg(.data),
    .error_call = .error_call,
    restrictr_fn = "schema_cast"
  )
}

#' @rdname schema_cast
#' @export
schema_cast.list <- function(
    .data,
    ...,
    .lossy = FALSE,
    .names = NULL,
    .size = NULL,
    .error_call = caller_env()) {
  cast_masked_exprs(
    .data,
    ...,
    .lossy = .lossy,
    .names = .names,
    .size = .size,
    .darg = caller_arg(.data),
    .error_call = .error_call,
    restrictr_fn = "schema_cast"
  )
}

cast_masked_exprs <- function(
    .data,
    ...,
    .lossy,
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
  validate_bool(
    .lossy,
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
    "cast",
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

  .data <- cast_exprs(
    .data,
    qs,
    nms,
    lossy = .lossy,
    eval_env = eval_env,
    error_call = .error_call,
    darg = .darg,
    restrictr_fn = restrictr_fn
  )

  return(.data)
}
