#-- type

check_coerce_type <- function(args, mask, arg_name, env, class, error_call) {
  if (!is.null(args$type)) {
    UseMethod("check_coerce_type", args)
  } else {
    mask
  }
}

#' @exportS3Method
check_coerce_type.default <- function(args, mask, arg_name, env, class, error_call) {
  cast_exprs(
    mask,
    args["type"],
    arg_name,
    FALSE,
    class,
    env,
    error_call = error_call,
    darg = args$mask,
    no_assignment = TRUE,
    calling_fn = "restrict"
  )
}

#' @exportS3Method
check_coerce_type.cast <- function(args, mask, arg_name, env, class, error_call) {
  cast_exprs(
    mask,
    args["type"],
    arg_name,
    FALSE,
    class,
    env,
    error_call = error_call,
    darg = args$mask,
    calling_fn = "restrict"
  )
}

#' @exportS3Method
check_coerce_type.lossy_cast <- function(args, mask, arg_name, env, class, error_call) {
  cast_exprs(
    mask,
    args["type"],
    arg_name,
    TRUE,
    class,
    env,
    error_call = error_call,
    darg = args$mask,
    calling_fn = "restrict"
  )
}

#' @exportS3Method
check_coerce_type.coerce <- function(args, mask, arg_name, env, class, error_call) {
  cast_exprs(
    mask,
    args["type"],
    arg_name,
    args$lossy,
    class,
    env,
    error_call = error_call,
    darg = args$mask,
    calling_fn = "restrict"
  )
}

#-- size

check_coerce_size <- function(args, mask, arg_name, env, class, error_call) {
  if (!is.null(args$size)) {
    UseMethod("check_coerce_size", args)
  } else {
    mask
  }
}

#' @exportS3Method
check_coerce_size.default <- function(args, mask, arg_name, env, class, error_call) {
  recycle_exprs(
    mask,
    args["size"],
    arg_name,
    class,
    env,
    error_call = error_call,
    darg = args$mask,
    no_assignment = TRUE,
    calling_fn = "restrict"
  )
}

#' @exportS3Method
check_coerce_size.recycle <- function(args, mask, arg_name, env, class, error_call) {
  recycle_exprs(
    mask,
    args["size"],
    arg_name,
    class,
    env,
    error_call = error_call,
    darg = args$mask,
    calling_fn = "restrict"
  )
}

#' @exportS3Method
check_coerce_size.coerce <- function(args, mask, arg_name, env, class, error_call) {
  recycle_exprs(
    mask,
    args["size"],
    arg_name,
    class,
    env,
    error_call = error_call,
    darg = args$mask,
    calling_fn = "restrict"
  )
}

#-- validates

check_validates <- function(args, mask, arg_name, env, class, error_call) {
  if (length(args$validations) > 0) {
    for (q in args$validations) {
      v <- eval_tidy(q, data = mask, env = env)

      if (!is_formula(v) && !is_function(v)) {
        abort(
          c(
            "Error in {.fn restrict}",
            i = format_inline(
              "Invalid validation for {.var {arg_name}}: {.var {as_label(q)}} ",
              "must be a function or formula, not {.cls {class(v)}}."
            )
          ),
          class = class,
          call = error_call
        )
      }

      if (is_formula(v)) {
        v <- as_function(v)
      }

      if (is_function(v)) {
        v <- eval_tidy(
          call2(v, sym(arg_name)),
          data = mask,
          env = env
        )
      }

      if (!is.logical(v)) {
        abort(
          c(
            "Error in {.fn restrict}",
            i = format_inline(
              "Invalid validation for {.var {arg_name}}: {.var {as_label(q)}} ",
              "returned class {.cls {class(v)}}, not {.cls logical}."
            )
          ),
          class = class,
          call = error_call
        )
      }

      if (!all(v)) {
        abort(
          c(
            "Error in {.fn restrict}",
            i = format_inline(
              "Validation failed for {.var {arg_name}}: {.var {as_label(q)}} ",
              "returned {.var FALSE}."
            )
          ),
          class = class,
          call = error_call
        )
      }
    }
  }
}
