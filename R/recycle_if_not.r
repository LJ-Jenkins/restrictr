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
#' @param .class class to assign to the error (passed to [rlang::abort]).
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
    .class = NULL,
    .error_call = caller_env()) {
  qs <- enexprs(...)

  validate_env(
    .env,
    class = .class,
    call = .error_call,
    calling_fn = "recycle_if_not"
  )
  validate_args_given(
    qs,
    class = .class,
    call = .error_call,
    calling_fn = "recycle_if_not"
  )

  nms <- glue_names(qs, .env)

  validate_args_named(
    nms,
    "recycle",
    class = .class,
    call = .error_call,
    calling_fn = "recycle_if_not"
  )
  validate_objs_exist(
    nms,
    .env,
    class = .class,
    call = .error_call,
    calling_fn = "recycle_if_not"
  )

  recycle_exprs(
    NULL,
    qs,
    nms,
    class = .class,
    call = .env,
    error_call = .error_call,
    calling_fn = "recycle_if_not"
  )

  invisible(NULL)
}

recycle_masked_exprs <- function(
    .data,
    ...,
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
      .names |> glue_chr(caller_env(2)),
      .darg,
      class = .class,
      call = .call,
      calling_fn = .calling_fn
    )
  }

  if (length(qs) == 0) {
    return(.data)
  }

  nms <- glue_names(qs, caller_env(2))

  validate_mask_args_named(
    .darg,
    nms,
    "recycle",
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

  .data <- recycle_exprs(
    .data = .data,
    qs = qs,
    nms = nms,
    class = .class,
    call = caller_env(),
    error_call = .call,
    darg = .darg,
    calling_fn = .calling_fn
  )

  return(.data)
}

recycle_exprs <- function(
    .data,
    qs,
    nms,
    class,
    call,
    error_call,
    darg = NULL,
    no_assignment = FALSE,
    calling_fn = NULL) {
  for (i in seq_along(qs)) {
    sz <- try_fetch(
      eval_tidy(qs[[i]], data = .data, env = call),
      error = function(cnd) {
        abort(
          c(
            "Error in {.fn {calling_fn}}",
            i = format_inline(
              "Error evaluating expression {.var {quo_string(qs[[i]])}} ",
              "{darg %!||% format_inline('for data mask {.var {darg}}')}"
            ),
            x = "{conditionMessage(cnd)}."
          ),
          class = class,
          call = error_call
        )
      }
    )

    validate_size_arg(sz, nms[i], class = class, call = error_call)

    tf <- eval_tidy(
      call2(vec_size, sym(nms[i])),
      data = .data,
      env = call
    ) == sz # faster than vec_is(x, size = y)

    if (no_assignment) {
      if (tf) {
        next
      } else {
        expected <- eval_tidy(
          call2(vec_size, sym(nms[i])),
          data = .data,
          env = call
        )

        abort(
          c(
            "Error in {.fn {calling_fn}}",
            x = format_inline(
              "Object {.var {nms[i]}} ",
              "{darg %!||% format_inline('for data mask {.var {darg}}')} ",
              "is of size {.cls {expected}}, not {.cls {sz}}."
            )
          ),
          class = class,
          call = error_call
        )
      }
    }

    if (!tf) {
      if (!is.null(.data)) {
        .data[[nms[i]]] <- try_fetch(
          vec_recycle(
            .data[[nms[i]]],
            sz,
            x_arg = nms[i],
            call = error_call
          ),
          error = function(cnd) {
            abort(
              c(
                "Error in {.fn {calling_fn}}",
                x = "{conditionMessage(cnd)}"
              ),
              class = class,
              call = error_call
            )
          }
        )
      } else {
        out <- try_fetch(
          eval_tidy(
            call2(
              vec_recycle,
              sym(nms[i]),
              sz,
              x_arg = nms[i],
              call = error_call
            ),
            env = call
          ),
          error = function(cnd) {
            abort(
              c(
                "Error in {.fn {calling_fn}}",
                x = "{conditionMessage(cnd)}"
              ),
              class = class,
              call = error_call
            )
          }
        )
        assign(nms[i], out, pos = call)
      }
    }
  }

  return(.data)
}
