#' Ensure the Truth of R Expressions
#'
#' If any of the expressions in `...` are not all `TRUE`, [rlang::abort] is called for
#' the first expression which was not (all) `TRUE`. The associated error message will
#' either be (in hierachy): from the name of the expression, the message argument,
#' or the expression itself.
#'
#' @param ... any number of R expressions, which should each evaluate to
#' (a logical vector of all) `TRUE` for no error to occur. If the expressions
#' are named, the names will be used in the error message.
#' @param .message single default error message for non-named expressions.
#' @param .na_rm if TRUE, NA values are removed in the logical vectors (default is FALSE)
#' @param .class class to assign to the error (passed to [rlang::abort]).
#' @param .error_call the call environment to use for the error (passed to [rlang::abort]).
#' @details `abort_if()` is the opposite of `abort_if_not()`, i.e. expressions
#' should evaluate to (all) `FALSE` for no error to occur.
#' @export
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' abort_if_not(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE
#'
#' m <- matrix(c(1, 3, 3, 1), 2, 2)
#' abort_if_not(m == t(m), diag(m) == rep(1, 2)) # all TRUE
#'
#' abort_if_not(length(10)) |> try()
#' # => Error: Expression `length(10)` for object `.data` must evaluate to
#' # class <logical> not <integer>.
#' # even when if(1) "ok" works
#'
#' # The default error message can be overridden to be more informative:
#' m[1, 2] <- 12
#' abort_if_not("m must be symmetric" = m == t(m)) |> try()
#' # => Error: m must be symmetric
#'
#' # Alternatively, one error message can be used for all expressions:
#' abort_if_not(
#'   m == t(m),
#'   diag(m) == rep(1, 2),
#'   message = "m must be symmetric and have 1s on the diagonal."
#' ) |> try()
#' # => Error: m must be symmetric and have 1s on the diagonal.
#'
#' abort_if(1 == 1) |> try() # abort_if errors if any argument does not evaluate to (all) FALSE
#'
#' # injection and glue can be used to supply expressions, names, and messages:
#' x <- "my error"
#' abort_if_not("{x}" = FALSE) |> try()
#' # => Error: my error
#' y <- FALSE
#' abort_if_not({{ x }} := !!y) |> try()
#' # => Error: my error
#' abort_if_not(!!x := !!y) |> try()
#' # => Error: my error
#' x <- list("my error" = FALSE)
#' abort_if_not(!!!x) |> try()
#' # => Error: my error
abort_if_not <- function(
    ...,
    .message = NULL,
    .na_rm = FALSE,
    .class = NULL,
    .error_call = caller_env()) {
  tf <- enquos(...)

  validate_args_given(
    tf,
    class = .class,
    call = .error_call,
    calling_fn = "abort_if_not"
  )
  validate_env(
    .error_call,
    allow_global = TRUE,
    class = .class,
    calling_fn = "abort_if_not"
  )
  validate_chr(
    .message,
    allow_null = TRUE,
    class = .class,
    call = .error_call,
    calling_fn = "abort_if_not"
  )

  nms <- glue_names(
    tf,
    caller_env(),
    calling_fn = "abort_if_not",
    error_class = .class,
    error_call = .error_call
  )

  check_logi_exprs(
    NULL,
    tf,
    nms,
    check_false = TRUE,
    message = .message,
    na_rm = .na_rm,
    class = .class,
    call = caller_env(),
    error_call = .error_call,
    calling_fn = "abort_if_not"
  )
}

#' @rdname abort_if_not
#' @usage NULL
#' @export
abortifnot <- abort_if_not

#' @rdname abort_if_not
#' @usage NULL
#' @export
abort_if <- function(
    ...,
    .message = NULL,
    .na_rm = FALSE,
    .class = NULL,
    .error_call = caller_env()) {
  tf <- enquos(...)

  validate_args_given(
    tf,
    class = .class,
    call = .error_call,
    calling_fn = "abort_if"
  )
  validate_env(
    .error_call,
    allow_global = TRUE,
    class = .class,
    calling_fn = "abort_if"
  )
  validate_chr(
    .message,
    allow_null = TRUE,
    class = .class,
    call = .error_call,
    calling_fn = "abort_if"
  )

  nms <- glue_names(
    tf,
    caller_env(),
    calling_fn = "abort_if_not",
    error_class = .class,
    error_call = .error_call
  )

  check_logi_exprs(
    NULL,
    tf,
    nms,
    check_false = FALSE,
    message = .message,
    na_rm = .na_rm,
    class = .class,
    call = caller_env(),
    error_call = .error_call,
    calling_fn = "abort_if"
  )
}

check_masked_logi_exprs <- function(
    .data,
    ...,
    .names,
    .size,
    .message,
    .na_rm,
    .darg,
    .class,
    .call,
    .calling_fn = NULL) {
  tf <- enquos(...)

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
  validate_env(
    .call,
    allow_global = TRUE,
    class = .class,
    calling_fn = .calling_fn
  )
  validate_chr(
    .message,
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

  if (length(tf) == 0) {
    invisible(NULL)
  }

  nms <- glue_names(
    tf,
    caller_env(2),
    .calling_fn,
    .class,
    .call
  )

  check_logi_exprs(
    .data,
    tf,
    nms = nms,
    check_false = TRUE,
    message = .message,
    na_rm = .na_rm,
    class = .class,
    call = caller_env(2),
    error_call = .call,
    darg = .darg,
    calling_fn = .calling_fn
  )
}

check_logi_exprs <- function(
    .data,
    tf,
    nms,
    check_false,
    message,
    na_rm,
    class,
    call,
    error_call = NULL,
    darg = NULL,
    calling_fn = NULL) {
  logi_check <- quote(!all(y, na.rm = na_rm))
  if (!check_false) {
    logi_check <- quote(any(y, na.rm = na_rm))
  }

  for (i in seq_along(tf)) {
    y <- try_fetch(
      eval_tidy(tf[[i]], data = .data),
      error = function(cnd) {
        expr_error(
          as_label(tf[[i]]),
          darg,
          cnd,
          calling_fn,
          class,
          error_call
        )
      }
    )

    if (!is.logical(y)) {
      not_logical_error(
        as_label(tf[[i]]),
        darg,
        NULL,
        class(y),
        calling_fn,
        class,
        error_call
      )
    }

    if (eval_tidy(logi_check)) {
      abort(
        tf_error(
          as_label(tf[[i]]),
          darg,
          NULL,
          nms[i] %""% message %||% NULL,
          check_false,
          calling_fn,
          class,
          error_call
        )
      )
    }
  }

  invisible(NULL)
}
