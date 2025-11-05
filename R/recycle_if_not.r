#' Check if objects are of a specific size and recycle them if not
#'
#' If any of the named expressions in `...` are not the specified size,
#' then the object specified by the name attempts to be recycled to the
#' size specified in the expression. Expressions are evaluated and
#' objects are assigned into the environment specified by the `.env`
#' argument. The checking of size and the recycling are from the
#' [vctrs](https://vctrs.r-lib.org/) package (using [vctrs::vec_size]
#' and [vctrs::vec_recycle]) and thus stick to the [vctrs recycling rules]
#' (https://vctrs.r-lib.org/reference/theory-faq-recycling.html).
#'
#' @param ... any number of named R expressions, in the form of:
#' `name_of_object_to_recycle = size_to_recycle_to`
#' @param .env the environment to use for the evaluation of the recycling
#' expressions and the assignment of the recycled objects. Cannot be
#' the global environment.
#' @param .error_call the call environment to use for error messages
#' (passed to [rlang::abort]).
#' @return NULL, but objects named in `...` will be changed in the
#' `.env` environment specified.
#' @export
#' @examples
#' # NB: Will not alter the global environment so examples
#' #     here are wrapped with local(). Some of these examples
#' #     are also expected to produce an error so are piped to try().
#'
#' x <- 1
#' recycle_if_not(x = 1) |> try()
#' # Error:
#' # Caused by error in `recycle_if_not()`.
#' # ! `env` must not be the global environment.
#'
#' local({
#'   x <- 1
#'   recycle_if_not(x = 3)
#'   length(x)
#' })
#' # 3
#'
#' # recycle_if_not() follows `vctrs` recycling rules:
#' local({
#'   x <- c(1, 1)
#'   recycle_if_not(x = 6) |> try()
#' })
#' # Error:
#' # Caused by error in `recycle_if_not()`:
#' # ℹ In argument: `x = 6`.
#' # ! Can't recycle `x` (size 2) to size 6.
#'
#' local({
#'   x <- 1L
#'   y <- 2.3
#'   recycle_if_not(x = 3, y = 2)
#'   cat(length(x), length(y), sep = ", ")
#' })
#' # 3, 2
#'
#' # Beware when using other objects as the size argument, e.g.:
#' local({
#'   x <- 1L
#'   y <- c(1, 1, 1)
#'   recycle_if_not(x = y) |> try()
#' })
#' # Error:
#' # Caused by error in `recycle_if_not()`:
#' # ℹ In argument: `x = y`.
#' # ! Size argument is length `3`, needs to be scalar integerish.
#'
#' # When using other objects, call vctrs::vec_size() on them first:
#' local({
#'   x <- 1L
#'   y <- c(1, 1, 1)
#'   recycle_if_not(x = vctrs::vec_size(y))
#'   length(x)
#' })
#' # 3
#'
#' # recycle_if_not() works sequentially, so references to objects will
#' # be after they have been evaluated:
#' local({
#'   x <- y <- 1
#'   recycle_if_not(x = 3, y = vctrs::vec_size(x))
#'   cat(length(x), length(y), sep = ", ")
#' })
#' # 3, 3
#'
#' myfunc <- \(x) {
#'   recycle_if_not(x = 3)
#'   length(x)
#' }
#' x <- 1L
#' myfunc(x) # x is recycled to length 3 within the function
#' length(x) # x is still scalar outside the function
#'
#' # The `.env` argument determines the expression and assignment environment:
#' local({
#'   x <- 1
#'   e <- new.env()
#'   e$x <- 1
#'   recycle_if_not(x = 3, .env = e)
#'   cat(
#'     "environment 'e'", length(e$x), "local environment", length(x),
#'     sep = ", "
#'   )
#' })
#'
#' # Named objects (lhs) are checked to be in the `.env` environment,
#' # throwing an error if not found:
#' local({
#'   x <- 1
#'   e <- new.env()
#'   recycle_if_not(x = 3, .env = e) |> try()
#' })
#' # Error:
#' # Caused by error in `recycle_if_not()`.
#' # ! Object `x` is not found in the `.env` environment specified.
#'
#' # For expressions (rhs), the `.env` argument is preferentially chosen,
#' # but if not found then the normal R scoping rules apply:
#' local({
#'   x <- 3
#'   e <- new.env()
#'   e$z <- 1
#'   recycle_if_not(z = x, .env = e)
#'   length(e$z)
#' })
#' # 3
#'
#' # The `.error_call` argument can be used to specify where the error occurs,
#' # by default this is the caller environment.
#' myfunc <- function(x) recycle_if_not(x = -5)
#' myfunc(1) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `recycle_if_not()`:
#' # ℹ In argument: `x = -5`.
#' # ! Size argument is `-5`, needs to be positive scalar integerish.
#'
#' #' # Injection and glue can be used:
#' local({
#'   y <- 1L
#'   x <- "y"
#'   recycle_if_not("{x}" = 5)
#'   length(y)
#' })
#' # 5
#' local({
#'   y <- 1L
#'   x <- list(y = 5)
#'   recycle_if_not(!!!x)
#'   length(y)
#' })
#' # 5
recycle_if_not <- function(
    ...,
    .env = caller_env(),
    .error_call = caller_env()) {
  qs <- enexprs(...)
  restrictr_fn <- "recycle_if_not"

  validate_env(
    .error_call,
    allow_global = TRUE,
    call = caller_env(),
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
