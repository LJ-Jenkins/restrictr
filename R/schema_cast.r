#' Ensure data.frame/list elements are of a specific type and cast them if not
#'
#' If any of the expressions in `...`, evaluated within the data mask
#' `.data` (see [rlang::args_data_masking]), are
#' not of the same type, then the data element attempts to be cast to
#' the type specified in the expression. The `.names` and `.size`
#' arguments can be used to check for given names and size of the
#' data.frame/list. The checking of type and the type conversion are from the
#' [vctrs](https://vctrs.r-lib.org/) package (using [vctrs::vec_is]
#' and [vctrs::vec_cast]) and thus stick to the [vctrs type conversion rules](https://vctrs.r-lib.org/reference/faq-compatibility-types.html).
#' The checking of size is also from [vctrs](https://vctrs.r-lib.org/)
#' (using [vctrs::vec_size]) and thus applies [vctrs size rules](https://vctrs.r-lib.org/articles/type-size.html).
#'
#' @param .data a data.frame or list to use as the data mask.
#' @param ... any number of R expressions to be evaluated using `.data`
#' as a data mask. Should follow the format of `named_element = expected_type`,
#' e.g, `var_x = integer()` or `var_x = var_y`.
#' @param .lossy if `TRUE`, lossy casting is undertaken.
#' @param .names optional character vector of names which must be
#' present in the `.data` data.frame/list. Can be a glue string.
#' @param .size optional positive scalar integerish value for the size that
#' the `.data` data.frame/list must have.
#' @param .error_call the call environment to use for error messages
#' (passed to [rlang::abort]).
#' @param .darg the argument name of `.data` to use in error messages.
#' @details See [restrictr::schema] and [restrictr::schema_recycle]
#' for validation and recycling, as well as [restrictr::cast_if_not] for
#' a non-data-masked version of casting. [restrictr::restrict] can also
#' be used for type casting, size recycling, and validation.
#' @return Object `.data`, with named elements cast to the desired type. Also
#' attaches class `with_schema` and attribute `schema` containing the
#' schema_cast call to be enforced later.
#' @export
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' li <- list(x = 1.1, y = "hi", z = 1L:2L)
#' # Input remains the same if types match
#' li <- schema_cast(li, x = double(), y = character(), z = integer())
#'
#' # By default, lossy casting is not allowed:
#' schema_cast(li, x = integer()) |> try()
#' # Error:
#' # Caused by error in `schema_cast()`:
#' # ℹ In argument: `x = integer()` for data mask `li`.
#' # ! Can't convert from `x` <double> to <integer> due to loss of precision.
#' # • Locations: 1
#'
#' # Lossy casting can be enabled with the `.lossy` argument:
#' schema_cast(li, x = integer(), .lossy = TRUE)$x
#'
#' # Other objects can be used as the type to cast to, e.g.:
#' schema_cast(li, z = x)$z |> class()
#'
#' # schema_cast() works sequentially, so references to objects will be
#' # after they have been evaluated:
#' li$a <- 1L
#' schema_cast(li, z = double(), a = z)$a |> class()
#'
#' # `.names` and `.size` arguments can be used to check that given names
#' # are present and that the data has the desired (vctrs) size:
#' schema_cast(li, .names = c("a", "x", "y", "b")) |> try()
#' # Error:
#' # Caused by error in `schema_cast()`.
#' # ! Named elements `a` and `b` not found in data mask `li`.
#'
#' schema_cast(li, .size = 5) |> try()
#' # Error:
#' # Caused by error in `schema_cast()`.
#' # ! Object `li` is of vctrs size `3`, not `5`.
#'
#' # The `.error_call` argument can be used to specify where the error occurs,
#' # by default this is the caller environment.
#' myfunc <- function(li, ...) schema_cast(li, ...)
#' myfunc(li, x = character()) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `schema_cast()`:
#' # ℹ In argument: `x = character()` for data mask `li`.
#' # ! Can't convert `x` <double> to <character>.
#'
#' # Injection and glue can be used:
#' li <- list(x = 1L)
#' x_name <- "x"
#' schema_cast(li, "{x_name}" = double())
#' schema_cast(li, !!x_name := double())
#' schema_cast(li, {{ x_name }} := double())
#' x_list <- list(x = double())
#' schema_cast(li, !!!x_list)
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
    .error_call = caller_env(),
    .darg = caller_arg(.data)) {
  cast_masked_exprs(
    .data,
    ...,
    .lossy = .lossy,
    .names = .names,
    .size = .size,
    .darg = .darg,
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
    .error_call = caller_env(),
    .darg = caller_arg(.data)) {
  cast_masked_exprs(
    .data,
    ...,
    .lossy = .lossy,
    .names = .names,
    .size = .size,
    .darg = .darg,
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

  attr(.data, "schema") <- structure(
    list(
      exprs = qs,
      arg_names = nms,
      lossy = .lossy,
      mask_names = .names,
      mask_size = .size
    ),
    class = "restrictr:::schema_cast"
  )
  class(.data) <- c("with_schema", class(.data))

  return(.data)
}
