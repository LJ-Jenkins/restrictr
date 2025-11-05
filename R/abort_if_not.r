#' Ensure the truth of R expressions
#'
#' If any of the expressions in `...` are not all `TRUE`, [rlang::abort] is
#' called for the first expression which was not ([all]) `TRUE`. A replacement
#' for [stopifnot] that utilises [rlang], [cli], and [glue].
#'
#' @param ... any number of R expressions, which should each evaluate to
#' (a logical vector of [all]) `TRUE` for no error to occur. Positive numbers
#' are not `TRUE`, even when they are coerced to `TRUE` inside `if()` or in
#' arithmetic computations in R. If the expressions are named, the names
#' will be used in the error message. Names support
#' [rlang injection](https://rlang.r-lib.org/reference/topic-inject.html) and
#' [glue](https://glue.tidyverse.org/) interpreted string literals.
#' @param .na_rm if `TRUE`, NA values are removed in the logical vectors
#' before evaluation.
#' @param .message single default error message for non-named expressions.
#' Can be a glue string.
#' @param .error_call the call environment to use for error messages
#' (passed to [rlang::abort]).
#' @details `abort_if()` is the opposite of `abort_if_not()`, i.e. expressions
#' should evaluate to ([all]) `FALSE` for no error to occur.
#' @return NULL, called for side effects only.
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
#' abort_if_not(1) |> try()
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `1`.
#' # ! Returned <integer>, not <logical>.
#'
#' # A custom error message can be given for each expression:
#' m[1, 2] <- 12
#' abort_if_not("m must be symmetric" = m == t(m)) |> try()
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `m == t(m)`.
#' # ! m must be symmetric
#'
#' # Alternatively, one error message can be used for all expressions:
#' abort_if_not(
#'   m == t(m),
#'   diag(m) == rep(1, 2),
#'   .message = "m must be symmetric and have 1s on the diagonal."
#' ) |> try()
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `m == t(m)`.
#' # ! m must be symmetric and have 1s on the diagonal.
#'
#' # Option to remove NA values before checking:
#' abort_if_not(c(TRUE, NA, TRUE), .na_rm = TRUE) # no error
#'
#' # The `.error_call` argument can be used to specify where the error occurs,
#' # by default this is the caller environment.
#' myfunc <- function(x) abort_if_not(x)
#' myfunc(FALSE) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `x`.
#' # ! Returned `FALSE`.
#'
#' # abort_if() errors if any argument does not evaluate to (all) FALSE.
#' abort_if(1 == 1) |> try()
#' # Error:
#' # Caused by error in `abort_if()`:
#' # ℹ In argument: `1 == 1`.
#' # ! Returned `TRUE`.
#'
#' # Injection and glue can be used:
#' x <- "my error"
#' abort_if_not("{x}" = FALSE) |> try()
#' abort_if_not({{ x }} := FALSE) |> try()
#' abort_if_not(!!x := FALSE) |> try()
#' abort_if_not(FALSE, .message = "{x}") |> try()
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `FALSE`.
#' # ! my error
#' x <- list("my bang-bang-bang error" = FALSE)
#' abort_if_not(!!!x) |> try()
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `FALSE`.
#' # ! my bang-bang-bang error
abort_if_not <- function(
    ...,
    .na_rm = FALSE,
    .message = NULL,
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
