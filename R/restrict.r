#' Restrict arguments to a specific type/size and test adherence to
#' validation functions/formulas.
#'
#' This function takes any number of named expressions referring to objects
#' in the given environment, checking and possibly coercing them to the specified
#' type and/or size, and also checking them against any number of validation
#' functions/formulas. Using the function keywords of `validate`, `cast`,
#' `lossy_cast`, `recycle`, and `coerce` within the expressions allows for
#' different behaviours:
#' - `validate`: checks that the object is of the specified type/size and
#' adheres to the validations, throwing an error if not.
#' - `cast`: differs from validate by checking that the object is of the
#' specified type, and if not attempts to cast it to that type
#' (throwing an error if not possible).
#' - `lossy_cast`: differs from `cast` by allowing lossy casting
#' (e.g. double to integer).
#' - `recycle`: differs from `validate` by checking that the object is of
#' the specified size, and if not attempts to recycle it to that size
#' (throwing an error if not possible).
#' - `coerce`: differs from `validate` by checking both the type and size,
#' and attempting to cast and/or recycle it to that type/size
#' (throwing an error if not possible). Casting is not lossy by default but
#' can be made lossy by adding `lossy = TRUE` within the `coerce()` call.
#'
#' These functions accept the named arguments `type`, `size` and `mask`
#' (`lossy` is also accepted within `coerce()`):
#' - `type`: an R object of the desired type (e.g. `integer()`, `double()`,
#' `character()`, `list()`). The type checking and casting are done using
#' the [vctrs](https://vctrs.r-lib.org/) package (using [vctrs::vec_is] and
#' [vctrs::vec_cast]) and thus stick to the [vctrs type conversion rules](https://vctrs.r-lib.org/reference/faq-compatibility-types.html).
#' - `size`: a scalar integerish value specifying the desired size. The size
#' checking and recycling are done using the [vctrs](https://vctrs.r-lib.org/)
#' package (using [vctrs::vec_size] and [vctrs::vec_recycle]) and thus stick to
#' the [vctrs recycling rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html).
#' - `mask`: an optional data frame or list to use as a data mask for
#' evaluations. Expressions are evaluated using [rlang::eval_tidy] with
#' the `data` argument set to the mask and the `env` argument set to the
#' environment specified by the `.env` argument to `restrict()`.
#' The mask must be present within the environment.
#' - `na_rm`: if TRUE, NA values are removed in the logical vectors from the validations before evaluating (default is FALSE).
#'
#' All other inputs should be unnamed validations: either expressions or formulas
#' (that evaluate to logical). `restrict` first evaluates type, then size, then the
#' validations. Any change from the prior expression is reflected in subsequent
#' expressions, i.e. if an object is cast to a new type then that new type is
#' used for the size check and validations.
#' If you do not wish to use the [vctrs](https://vctrs.r-lib.org/) type/size checking,
#' then instead give validations functions such as `~ is.integer(.x)`.
#' However, these will only validate, not cast or recycle.
#' `restrict` is designed for the checking of numerous objects, for a smaller number of
#' objects to check see the [restrictr::abort_if_not], [restrictr::cast_if_not],
#' [restrictr::recycle_if_not], [restrictr::schema], [restrictr::schema_cast] and
#' [restrictr::schema_recycle] functions.
#'
#' @param ... any number of named R expressions, with the names referring to objects
#' in the environment specified by the `.env` argument, and the expressions built
#' using the functions: `validate()`, `cast()`, `lossy_cast()`, `recycle()`, and `coerce()`.
#' @param .env the environment to use for the evaluation of the expressions & the
#' (possible) assignment of the variables. Cannot be the global environment.
#' @param .error_call the call environment to use for the error (passed to [rlang::abort]).
#' @export
#' @examples
#' # Will not alter the global environment so most examples here are wrapped with local().
#' x <- 1L
#' restrict(x = validate(type = integer())) |> try()
#' # => Error : Argument `.env` cannot be the global environment.
#'
#' local({
#'   x <- 1L
#'   restrict(x = coerce(type = double(), size = 3))
#'   cat(class(x), length(x), sep = ", ")
#' })
#'
#' local({
#'   x <- 1.5
#'   restrict(x = cast(type = integer())) |> try()
#' })
#' # => Error : Can't convert from `x` <double> to <integer> due to loss of precision.
#'
#' local({
#'   x <- 1.5
#'   restrict(x = lossy_cast(type = integer()))
#'   cat(x, class(x), sep = ", ")
#'
#'   # or
#'
#'   x <- 1.5
#'   restrict(x = coerce(type = integer(), lossy = TRUE))
#'   cat(x, class(x), sep = ", ")
#' })
#'
#' # other objects can be used as the type to cast to or size to recycle to, e.g.:
#' local({
#'   x <- 1L
#'   y <- 2.3
#'   z <- 3L
#'   restrict(x = coerce(type = y, size = z))
#'   cat(class(x), length(x), sep = ", ")
#' })
#'
#' # restrict works sequentially, so references to objects will be
#' # after they have been evaluated:
#' local({
#'   x <- y <- 1L
#'   restrict(
#'     x = cast(type = double()),
#'     y = cast(type = x)
#'   )
#'   cat(class(x), class(y), sep = ", ")
#' })
#'
#' # numerous validations can be given and type and size checking can be done
#' # within if base R checking is preferred:
#' local({
#'   x <- 1L
#'   restrict(
#'     x = validate(
#'       ~ is.integer(.x),
#'       ~ length(.x) == 1,
#'       \(y) all(y > 0),
#'       \(z) !is.character(z)
#'     )
#'   )
#' })
#'
#' # the `.env` argument determines the expression and assignment environment:
#' local({
#'   x <- 1L
#'   e <- new.env()
#'   e$x <- 1L
#'   restrict(x = cast(type = 1.5), .env = e)
#'   cat(class(e$x), class(x), sep = ", ")
#' })
#'
#' # names (lhs) are checked to be in the `.env` environment, throwing an error if not found:
#' local({
#'   x <- 1L
#'   e <- new.env()
#'   restrict(x = cast(type = 1.5), .env = e) |> try()
#' })
#' # => Error: Objects `x` are not found in the `.env` environment specified.
#'
#' # for expressions (rhs), the `.env` argument is preferentially chosen, but if not found
#' # then the normal R scoping rules apply:
#' local({
#'   x <- 1.5
#'   e <- new.env()
#'   e$z <- 1L
#'   restrict(x = cast(type = x), .env = e) |> try()
#'   cat(class(e$z))
#' })
restrict <- function(
    ...,
    .env = caller_env(),
    .error_call = caller_env()) {
  rargs <- enexprs(...)
  restrictr_fn <- "restrict"

  validate_args_given(
    rargs,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
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

  arg_names <- glue_names(
    rargs,
    eval_env = .env,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )

  validate_args_named(
    arg_names,
    restrictr_fn,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )

  rargs <- prep_restrict_args(
    rargs,
    arg_names,
    .env,
    .error_call
  )

  vnames <- lapply(
    rargs,
    function(x) {
      glue_names(
        x$validations,
        eval_env = .env,
        error_call = .error_call,
        restrictr_fn = restrictr_fn
      )
    }
  )

  withCallingHandlers(
    for (i in seq_along(rargs)) {
      arg <- arg_names[i]
      args <- rargs[[i]]

      mask <- NULL
      if (!is.null(args$mask)) {
        mask <- .env[[args$mask]] %||%
          abort_args_env(not_found = args$mask)

        check_names_present(mask, arg, args$mask)
      } else {
        validate_objs_exist(arg, .env)
      }

      mask <- check_coerce_type(args, mask, arg, .env)

      mask <- check_coerce_size(args, mask, arg, .env)

      if (length(args$validations) > 0) {
        check_validates(args, mask, arg, vnames, .env)
      }

      # non-masked objects are evaluated in the .env environment
      # but masked objects are altered within restrict
      # assign mask object back here if exists and not a validate call
      # may switch to altered in its own .env like in old versions of restrictr in future
      if (!inherits(args, "validate") && !is.null(args$mask)) {
        mask %!||% assign(args$mask, mask, pos = .env)
      }
    },
    error = function(cnd) {
      message <- cnd_bullets(
        cnd,
        restrictr_fn = restrictr_fn,
        arg = arg,
        mask = args$mask
      )
      abort(
        message = message,
        call = .error_call
      )
    }
  )
}
