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
#' @param .class class to assign to the error (passed to [rlang::abort]).
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
    .class = NULL,
    .error_call = caller_env()) {
  check_masked_logi_exprs(
    .data,
    ...,
    .names = .names,
    .size = .size,
    .message = .message,
    .darg = caller_arg(.data),
    .class = .class, .call =
      .error_call,
    .calling_fn = "schema"
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
    .class = NULL,
    .error_call = caller_env()) {
  check_masked_logi_exprs(
    .data,
    ...,
    .names = .names,
    .size = .size,
    .message = .message,
    .darg = caller_arg(.data),
    .class = .class, .call =
      .error_call,
    .calling_fn = "schema"
  )
}

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
#' @param .class class to assign to the error (passed to [rlang::abort]).
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
    .class = NULL,
    .error_call = caller_env()) {
  cast_masked_exprs(
    .data,
    ...,
    .lossy = .lossy,
    .names = .names,
    .size = .size,
    .darg = caller_arg(.data),
    .class = .class,
    .call = .error_call,
    .calling_fn = "schema_cast"
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
    .class = NULL,
    .error_call = caller_env()) {
  cast_masked_exprs(
    .data,
    ...,
    .lossy = .lossy,
    .names = .names,
    .size = .size,
    .darg = caller_arg(.data),
    .class = .class,
    .call = .error_call,
    .calling_fn = "schema_cast"
  )
}

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
#' @param .class class to assign to the error (passed to [rlang::abort]).
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
    .class = NULL,
    .error_call = caller_env()) {
  recycle_masked_exprs(
    .data, ...,
    .names = .names,
    .size = .size,
    .darg = caller_arg(.data),
    .class = .class,
    .call = .error_call,
    .calling_fn = "schema_recycle"
  )
}
