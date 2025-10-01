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
#' @param .class class to assign to the error (passed to [rlang::abort]).
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
    .class = NULL,
    .error_call = caller_env()) {
  qs <- enexprs(...)

  validate_env(
    .env,
    class = .class,
    call = .error_call,
    calling_fn = "cast_if_not"
  )
  validate_args_given(
    qs,
    class = .class,
    call = .error_call,
    calling_fn = "cast_if_not"
  )
  validate_bool(
    .lossy,
    class = .class,
    call = .error_call,
    calling_fn = "cast_if_not"
  )

  nms <- glue_names(
    qs,
    .env,
    "cast_if_not",
    .class,
    .error_call
  )

  validate_args_named(
    nms,
    "cast",
    class = .class,
    call = .error_call,
    calling_fn = "cast_if_not"
  )
  validate_objs_exist(
    nms,
    .env,
    class = .class,
    call = .error_call,
    calling_fn = "cast_if_not"
  )

  cast_exprs(
    NULL,
    qs,
    nms,
    lossy = .lossy,
    class = .class,
    call = .env,
    error_call = .error_call,
    calling_fn = "cast_if_not"
  )

  invisible(NULL)
}

cast_masked_exprs <- function(
    .data,
    ...,
    .lossy,
    .names,
    .size,
    .darg,
    .class,
    .call,
    .calling_fn = NULL) {
  qs <- enquos(...)
  validate_env(
    .call,
    allow_global = TRUE,
    class = .class,
    calling_fn = .calling_fn
  )
  validate_chr(
    .names,
    allow_null = TRUE,
    class = .class,
    call = .call,
    calling_fn = .calling_fn
  )
  validate_size_arg(
    .size,
    .darg,
    allow_null = TRUE,
    class = .class,
    call = .call,
    calling_fn = .calling_fn
  )
  validate_bool(
    .lossy,
    class = .class,
    call = .call,
    calling_fn = .calling_fn
  )

  if (!is.null(.size)) {
    check_size_true(
      .data,
      .size,
      .darg,
      class = .class,
      call = .call,
      calling_fn = .calling_fn
    )
  }

  if (!is.null(.names)) {
    check_names_present(
      .data,
      .names |>
        glue_chr(
          caller_env(2),
          .calling_fn,
          .class,
          .call
        ),
      .darg,
      class = .class,
      call = .call,
      calling_fn = .calling_fn
    )
  }

  if (length(qs) == 0) {
    return(.data)
  }

  nms <- glue_names(
    qs,
    caller_env(2),
    .calling_fn,
    .class,
    .call
  )

  validate_args_named(
    nms,
    "cast",
    mask = .darg,
    class = .class,
    call = .call,
    calling_fn = .calling_fn
  )

  check_names_present(
    .data,
    nms,
    .darg,
    class = .class,
    call = .call,
    calling_fn = .calling_fn
  )

  .data <- cast_exprs(
    .data,
    qs,
    nms,
    lossy = .lossy,
    class = .class,
    call = caller_env(),
    error_call = .call,
    darg = .darg,
    calling_fn = .calling_fn
  )

  return(.data)
}

cast_exprs <- function(
    .data,
    qs,
    nms,
    lossy,
    class,
    call,
    error_call,
    darg = NULL,
    no_assignment = FALSE,
    calling_fn = NULL) {
  for (i in seq_along(qs)) {
    qs_type <- try_fetch(
      eval_tidy(qs[[i]], data = .data, env = call),
      error = function(cnd) {
        expr_error(
          as_label(qs[[i]]),
          darg,
          cnd,
          calling_fn,
          class,
          error_call
        )
      }
    )

    tf <- try_fetch(
      eval_tidy(
        call2(vec_is, sym(nms[i]), ptype = qs_type),
        data = .data,
        env = call
      ),
      error = function(cnd) {
        vctrs_is_error(
          nms[i],
          eval_tidy(
            call2(base::class, sym(nms[i])),
            data = .data,
            env = call
          ),
          eval_tidy(
            call2(base::class, qs_type),
            data = .data,
            env = call
          ),
          darg,
          calling_fn,
          class,
          error_call
        )
      }
    )

    if (no_assignment) {
      if (tf) {
        next
      } else {
        class_error(
          nms[i],
          eval_tidy(
            call2(ptype_show, sym(nms[i])),
            data = .data,
            env = call
          ),
          ptype_show(qs_type),
          darg,
          calling_fn,
          class,
          error_call
        )
      }
    }

    if (!tf) {
      vctrs_call <- call2(
        vec_cast,
        sym(nms[i]),
        qs_type,
        x_arg = nms[i],
        call = error_call
      )

      if (lossy) {
        vctrs_call <- call2(allow_lossy_cast, vctrs_call)
      }

      out <- try_fetch(
        eval_tidy(vctrs_call, data = .data, env = call),
        error = function(cnd) {
          cnd_error(cnd, calling_fn, class, error_call)
        }
      )

      if (!is.null(.data)) {
        .data[[nms[i]]] <- out
      } else {
        assign(nms[i], out, pos = call)
      }
    }
  }

  return(.data)
}
