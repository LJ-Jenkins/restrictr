#' Check if objects are of a specific size and recycle them if not
#'
#' If any of the named expressions in `...` are not the specified size,
#' then the object attempts to be recycled to the size specified in the expression.
#' Expressions are evaluated and variables are assigned into the environment
#' specified by the `.env` argument. The checking of size and the recycling
#' are from the [vctrs](https://vctrs.r-lib.org/) package (using [vctrs::vec_size]
#' and [vctrs::vec_recycle]) and thus stick to the [vctrs recycling rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html).
#'
#' @param ... any number of named R expressions, in the form of:
#' `name_of_obj_to_cast = size_to_cast_to`
#' @param .env the environment to use for the evaluation of the recycling
#' expressions & the assignment of the recycled variables. Cannot be the global environment.
#' @param .error_call the call environment to use for the error (passed to [rlang::abort]).
#' @export
#' @examples
#' # Will not alter the global environment so most examples here are wrapped with local().
#' x <- 1
#' recycle_if_not(x = 1) |> try()
#' # => Error: Argument `call` cannot be the global environment.
#'
#' local({
#'   x <- 1
#'   recycle_if_not(x = 3)
#'   x
#' })
#'
#' local({
#'   x <- rep(1, 4)
#'   recycle_if_not(x = 1) |> try()
#' })
#' # => Error : Can't recycle `x` (size 4) to size 1.
#'
#' local({
#'   x <- 1L
#'   y <- 2.3
#'   recycle_if_not(x = 3, y = 2)
#'   cat(x, y, sep = ", ")
#' })
#'
#' # beware when using other objects as the size argument, e.g.:
#' local({
#'   x <- 1L
#'   y <- c(1, 1, 1)
#'   recycle_if_not(x = y) |> try()
#' })
#' # => Error : Size argument for `x` is not a scalar integerish value:
#' # object length `3` of class <numeric>.
#'
#' # when using other objects, call vctrs::vec_size() on them first:
#' local({
#'   x <- 1L
#'   y <- c(1, 1, 1)
#'   recycle_if_not(x = vctrs::vec_size(y))
#'   x
#' })
#'
#' # recycle_if_not works sequentially, so references to objects will
#' # be after they have been evaluated:
#' local({
#'   x <- y <- 1
#'   recycle_if_not(x = 3, y = vctrs::vec_size(x))
#'   cat(length(x), length(y), sep = ", ")
#' })
#'
#' myfunc <- \(x) {
#'   recycle_if_not(x = 3)
#'   x
#' }
#' x <- 1L
#' myfunc(x) # x is recycled to length 3 within the function
#' x # x is still scalar outside the function
#'
#' local({
#'   x <- 1
#'   y <- 2
#'   z <- 3
#'   recycle_if_not(x = 2, y = 3, z = 4)
#'   cat(x, y, z, sep = ", ")
#' })
#'
#' # the `.env` argument determines the expression and assignment environment:
#' local({
#'   x <- 1
#'   e <- new.env()
#'   e$x <- 1
#'   recycle_if_not(x = 3, .env = e)
#'   cat(length(e$x), length(x), sep = ", ")
#' })
#'
#' # names (lhs) are checked to be in the `.env` environment,
#' # throwing an error if not found:
#' local({
#'   x <- 1
#'   e <- new.env()
#'   recycle_if_not(x = 3, .env = e) |> try()
#' })
#' # Error: Objects `x` are not found in the `.env` environment specified.
#'
#' # for expressions (rhs), the `.env` argument is preferentially chosen,
#' # but if not found then the normal R scoping rules apply:
#' local({
#'   x <- 3
#'   e <- new.env()
#'   e$z <- 1
#'   recycle_if_not(z = x, .env = e)
#'   cat(e$z)
#' })
recycle_if_not <- function(
    ...,
    .env = caller_env(),
    .error_call = caller_env()) {
  qs <- enexprs(...)
  restrictr_fn <- "recycle_if_not"

  validate_env(
    .error_call,
    allow_global = TRUE,
    restrictr_fn = restrictr_fn
  )
  validate_env(
    .env,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_args_given(
    qs,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )

  nms <- glue_names(
    qs,
    eval_env = .env,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )

  validate_args_named(
    nms,
    "recycle",
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_objs_exist(
    nms,
    .env,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )

  recycle_exprs(
    NULL,
    qs,
    nms,
    eval_env = .env,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )

  invisible(NULL)
}

recycle_exprs <- function(
    .data,
    qs,
    nms,
    eval_env,
    error_call,
    restrictr_fn,
    darg = NULL,
    no_assignment = FALSE) {
  withCallingHandlers(
    for (i in seq_along(qs)) {
      .data <- recycle_expr(
        .data,
        qs[[i]],
        nms[[i]],
        eval_env,
        darg,
        no_assignment
      )
    },
    error = function(cnd) {
      message <- cnd_bullets(
        cnd,
        restrictr_fn = restrictr_fn,
        arg = as_label(qs[[i]]),
        name = nms[[i]],
        mask = darg
      )
      abort(
        message = message,
        call = error_call
      )
    }
  )

  return(.data)
}

recycle_expr <- function(
    .data,
    qs,
    nm,
    eval_env,
    darg = NULL,
    no_assignment = FALSE) {
  sz <- eval_tidy(qs, data = .data, env = eval_env)

  validate_size_arg(sz, nm, allow_null = FALSE)

  tf <- eval_tidy(
    call2(vec_size, sym(nm)),
    data = .data,
    env = eval_env
  ) == sz # faster than vec_is(x, size = y)

  if (no_assignment) {
    if (tf) {
      return(.data)
    } else {
      abort_size_false(
        arg = nm,
        actual_size = eval_tidy(
          call2(vec_size, sym(nm)),
          data = .data,
          env = eval_env
        ),
        expected_size = sz
      )
    }
  }

  if (!tf) {
    if (!is.null(.data)) {
      .data[[nm]] <- vec_recycle(
        .data[[nm]],
        sz,
        x_arg = nm
      )
    } else {
      out <- eval_tidy(
        call2(
          vec_recycle,
          sym(nm),
          sz,
          x_arg = nm
        ),
        env = eval_env
      )
      assign(nm, out, pos = eval_env)
    }
  }

  return(.data)
}
