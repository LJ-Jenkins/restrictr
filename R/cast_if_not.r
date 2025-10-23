#' Check if objects are of a specific type and cast them if not
#'
#' If any of the named expressions in `...` are not of the same type,
#' then the object attempts to be cast to the type specified in the expression.
#' Expressions are evaluated and variables are assigned into the environment
#' specified by the `.env` argument. The checking of type and the type conversion
#' are from the [vctrs](https://vctrs.r-lib.org/) package (using [vctrs::vec_is]
#' and [vctrs::vec_cast]) and thus stick to the [vctrs type conversion rules](https://vctrs.r-lib.org/reference/faq-compatibility-types.html).
#'
#' @param ... any number of named R expressions, in the form of:
#' `name_of_obj_to_cast = obj_of_type_to_cast_to`
#' @param .lossy whether to allow lossy casting.
#' @param .env the environment to use for the evaluation of the casting expressions
#' & the assignment of the casted variables. Cannot be the global environment.
#' @param .error_call the call environment to use for the error (passed to [rlang::abort]).
#' @export
#' @examples
#' # Will not alter the global environment so most examples here are wrapped with local().
#' x <- 1L
#' cast_if_not(x = integer()) |> try()
#' # => Error: Argument `call` cannot be the global environment.
#'
#' local({
#'   x <- 1L
#'   cast_if_not(x = double())
#'   class(x)
#' })
#'
#' local({
#'   x <- 1.5
#'   cast_if_not(x = integer()) |> try()
#' })
#' # => Error : Can't convert from `x` <double> to <integer> due to loss of precision.
#'
#' local({
#'   x <- 1.5
#'   cast_if_not(x = integer(), .lossy = TRUE)
#'   cat(x, class(x), sep = ", ")
#' })
#'
#' # other objects can be used as the type to cast to, e.g.:
#' local({
#'   x <- 1L
#'   y <- 2.3
#'   cast_if_not(x = y)
#'   class(x)
#' })
#'
#' # cast_if_not works sequentially, so references to objects will be
#' # after they have been evaluated:
#' local({
#'   x <- y <- 1L
#'   cast_if_not(x = double(), y = x)
#'   cat(class(x), class(y), sep = ", ")
#' })
#'
#' myfunc <- \(x) {
#'   cast_if_not(x = double())
#'   class(x)
#' }
#' x <- 1L
#' myfunc(x) # x is cast to double within the function
#' class(x) # x is still an integer outside the function
#'
#' local({
#'   x <- y <- z <- 1L
#'   cast_if_not(x = double(), y = double(), z = double())
#'   cat(class(x), class(y), class(z), sep = ", ")
#' })
#'
#' # the `.env` argument determines the expression and assignment environment:
#' local({
#'   x <- 1L
#'   e <- new.env()
#'   e$x <- 1L
#'   cast_if_not(x = 1.5, .env = e)
#'   cat(class(e$x), class(x), sep = ", ")
#' })
#'
#' # names (lhs) are checked to be in the `.env` environment,
#' # throwing an error if not found:
#' local({
#'   x <- 1L
#'   e <- new.env()
#'   cast_if_not(x = 1.5, .env = e) |> try()
#' })
#' # => Error: Objects `x` are not found in the `.env` environment specified.
#'
#' # for expressions (rhs), the `.env` argument is preferentially chosen, but if not found
#' # then the normal R scoping rules apply:
#' local({
#'   x <- 1.5
#'   e <- new.env()
#'   e$z <- 1L
#'   cast_if_not(z = x, .env = e)
#'   cat(class(e$z))
#' })
cast_if_not <- function(
    ...,
    .lossy = FALSE,
    .env = caller_env(),
    .error_call = caller_env()) {
  qs <- enexprs(...)
  restrictr_fn <- caller_name()

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
  validate_bool(
    .lossy,
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
    "cast",
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_objs_exist(
    nms,
    .env,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )

  cast_exprs(
    NULL,
    qs,
    nms,
    lossy = .lossy,
    eval_env = .env,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )

  invisible(NULL)
}

cast_exprs <- function(
    .data,
    qs,
    nms,
    lossy,
    eval_env,
    error_call,
    restrictr_fn,
    darg = NULL,
    no_assignment = FALSE) {
  withCallingHandlers(
    for (i in seq_along(qs)) {
      .data <- cast_expr(
        .data,
        qs[[i]],
        nms[i],
        lossy,
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

cast_expr <- function(
    .data,
    qs,
    nm,
    lossy,
    eval_env,
    darg = NULL,
    no_assignment = FALSE) {
  qs_type <- eval_tidy(qs, data = .data, env = eval_env)

  tf <- eval_tidy(
    call2(vec_is, sym(nm), ptype = qs_type),
    data = .data,
    env = eval_env
  )

  if (no_assignment) {
    if (tf) {
      return(.data)
    } else {
      abort_wrong_class(
        given_class = eval_tidy(
          call2(ptype_show, sym(nm)),
          data = .data,
          env = eval_env
        ),
        expected_class = ptype_show(qs_type),
      )
    }
  }

  if (!tf) {
    vctrs_call <- call2(
      vec_cast,
      sym(nm),
      qs_type,
      x_arg = nm
    )

    if (lossy) {
      vctrs_call <- call2(allow_lossy_cast, vctrs_call)
    }

    out <- eval_tidy(vctrs_call, data = .data, env = eval_env)

    if (!is.null(.data)) {
      .data[[nm]] <- out
    } else {
      assign(nm, out, pos = eval_env)
    }
  }

  return(.data)
}
