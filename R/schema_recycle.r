#' Ensure list elements are of a specific size and recycle them if not
#'
#' If any of the expressions in `...`, evaluated within the data mask
#' `.data` (see [rlang::args_data_masking]), are
#' not of the same size, then the data element attempts to be recycled to
#' the size specified in the expression. The `.names` and `.size`
#' arguments can be used to check for given names and size of the
#' list. The checking of size and the recycling are from the [vctrs]
#' (https://vctrs.r-lib.org/) package (using [vctrs::vec_size] and
#' [vctrs::vec_recycle]) and thus apply the [vctrs size rules](https://vctrs.r-lib.org/articles/type-size.html)
#' and [vctrs recycling rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html).
#'
#' @param .data a list to use as the data mask.
#' @param ... any number of R expressions to be evaluated using `.data`
#' as a data mask. Should follow the format of `named_element = expected_size`,
#' e.g, `var_x = 10` or `var_x = vctrs::vec_size(var_y)`.
#' @param .names optional character vector of names which must be
#' present in the `.data` data.frame/list. Can be a glue string.
#' @param .size optional positive scalar integerish value for the size that
#' the `.data` data.frame/list must have.
#' @param .error_call the call environment to use for error messages
#' (passed to [rlang::abort]).
#' @param .darg the argument name of `.data` to use in error messages.
#' @details See [restrictr::schema] and [restrictr::schema_cast]
#' for validation and casting, as well as [restrictr::recycle_if_not]
#' for a non-data-masked version of recycling. [restrictr::restrict]
#' can also be used for type casting, size recycling, and validation.
#' @return Object `.data`, with named elements recycled to the desired size.
#' Also attaches class "with_schema" and attribute "schema" containing the
#' schema_recycle call to be enforced later.
#' @export
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' li <- list(x = 1, y = "hi", z = 1:2)
#' schema_recycle(li, x = 5, y = 3) |> lengths()
#'
#' # schema_recycle() follows `vctrs` recycling rules:
#' schema_recycle(li, z = 6) |> try()
#' # Error:
#' # Caused by error in `schema_recycle()`:
#' # ℹ In argument: `z = 6` for data mask `li`.
#' # ! Can't recycle `z` (size 2) to size 6.
#'
#' # Other objects' lengths can be used as the size to
#' # recycle to, e.g.:
#' schema_recycle(li, x = vctrs::vec_size(z))$x |> length()
#'
#' # schema_recycle() works sequentially, so references to objects will be
#' # after they have been evaluated:
#' li$a <- 1.25
#' schema_recycle(
#'   li,
#'   x = vctrs::vec_size(z),
#'   a = vctrs::vec_size(x)
#' )$a |> length()
#'
#' # `.names` and `.size` arguments can be used to check that given names
#' # are present and that the data has the desired (vctrs) size:
#' schema_recycle(li, .names = c("a", "x", "y", "b")) |> try()
#' # Error:
#' # Caused by error in `schema_recycle()`.
#' # ! Named elements `a` and `b` not found in data mask `li`.
#'
#' schema_recycle(li, .size = 5) |> try()
#' # Error:
#' # Caused by error in `schema_recycle()`.
#' # ! Object `li` is of vctrs size `3`, not `5`.
#'
#'
#' # The `.error_call` argument can be used to specify where the error occurs,
#' # by default this is the caller environment.
#' myfunc <- function(li, ...) schema_recycle(li, ...)
#' myfunc(li, x = -5) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `schema_recycle()`:
#' # ℹ In argument: `x = -5` for data mask `li`.
#' # ! Size argument is `-5`, needs to be positive scalar integerish.
#'
#' # Injection and glue can be used:
#' li <- list(x = 1L)
#' x_name <- "x"
#' schema_recycle(li, "{x_name}" = 2)
#' schema_recycle(li, !!x_name := 2)
#' schema_recycle(li, {{ x_name }} := 2)
#' x_list <- list(x = 2)
#' schema_recycle(li, !!!x_list)
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
    .error_call = caller_env(),
    .darg = caller_arg(.data)) {
  recycle_masked_exprs(
    .data, ...,
    .names = .names,
    .size = .size,
    .darg = .darg,
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

  attr(.data, "schema") <- structure(
    list(
      exprs = qs,
      arg_names = nms,
      mask_names = .names,
      mask_size = .size
    ),
    class = "restrictr:::schema_recycle"
  )
  class(.data) <- c("with_schema", class(.data))

  return(.data)
}
