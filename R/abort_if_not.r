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
    .error_call = caller_env()) {
  tf <- enquos(...)
  restrictr_fn <- "abort_if_not"
  eval_env <- caller_env()

  validate_args_given(
    tf,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_env(
    .error_call,
    allow_global = TRUE,
    call = eval_env,
    restrictr_fn = restrictr_fn
  )
  validate_chr(
    .message,
    allow_null = TRUE,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_bool(
    .na_rm,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )

  nms <- glue_names(
    tf,
    eval_env = eval_env,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )

  if (!is.null(.message)) {
    .message <- glue_chr(
      .message,
      eval_env = eval_env,
      error_call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  check_logi_exprs(
    NULL,
    tf,
    nms,
    check_false = TRUE,
    message = .message,
    na_rm = .na_rm,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )
}

#' @rdname abort_if_not
#' @usage NULL
#' @export
abort_if <- function(
    ...,
    .message = NULL,
    .na_rm = FALSE,
    .error_call = caller_env()) {
  tf <- enquos(...)
  restrictr_fn <- "abort_if"
  eval_env <- caller_env()

  validate_args_given(
    tf,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_env(
    .error_call,
    allow_global = TRUE,
    call = eval_env,
    restrictr_fn = restrictr_fn
  )
  validate_chr(
    .message,
    allow_null = TRUE,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )
  validate_bool(
    .na_rm,
    call = .error_call,
    restrictr_fn = restrictr_fn
  )

  nms <- glue_names(
    tf,
    eval_env = eval_env,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )

  if (!is.null(.message)) {
    .message <- glue_chr(
      .message,
      eval_env = eval_env,
      error_call = .error_call,
      restrictr_fn = restrictr_fn
    )
  }

  check_logi_exprs(
    NULL,
    tf,
    nms,
    check_false = FALSE,
    message = .message,
    na_rm = .na_rm,
    error_call = .error_call,
    restrictr_fn = restrictr_fn
  )
}

check_logi_exprs <- function(
    .data,
    tf,
    nms,
    check_false,
    message,
    na_rm,
    error_call,
    restrictr_fn,
    darg = NULL) {
  logi_check <- quote(!all(logi))
  if (!check_false) {
    logi_check <- quote(any(logi))
  }

  withCallingHandlers(
    for (i in seq_along(tf)) {
      check_logi_expr(
        .data,
        tf[[i]],
        logi_check,
        check_false,
        message,
        na_rm
      )
    },
    error = function(cnd) {
      message <- cnd_bullets(
        cnd,
        restrictr_fn = restrictr_fn,
        arg = as_label(tf[[i]]),
        msg = nms[i] %""% message,
        mask = darg
      )
      abort(
        message = message,
        call = error_call
      )
    }
  )
}

check_logi_expr <- function(
    .data,
    tf,
    logi_check,
    check_false,
    message,
    na_rm) {
  logi <- eval_tidy(tf, data = .data)

  if (!is.logical(logi)) {
    abort_wrong_class(
      given_class = class(logi),
      expected_class = "logical"
    )
  }

  if (isTRUE(na_rm) && anyNA(logi)) {
    logi <- logi[!is.na(logi)]
  } else if (anyNA(logi)) {
    abort_na_present()
  }

  if (length(logi) == 0) {
    abort_empty()
  }

  if (eval_tidy(logi_check)) {
    abort_true_false(check_false = check_false)
  }
}
