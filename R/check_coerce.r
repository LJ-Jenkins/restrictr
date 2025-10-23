#-- type

check_coerce_type <- function(args, mask, arg_name, env) {
  if (!is.null(args$type)) {
    UseMethod("check_coerce_type", args)
  } else {
    mask
  }
}

#' @exportS3Method
check_coerce_type.default <- function(args, mask, arg_name, env) {
  cast_expr(
    mask,
    args[["type"]],
    arg_name,
    FALSE,
    env,
    darg = args$mask,
    no_assignment = TRUE
  )
}

#' @exportS3Method
check_coerce_type.cast <- function(args, mask, arg_name, env) {
  cast_expr(
    mask,
    args[["type"]],
    arg_name,
    FALSE,
    env,
    darg = args$mask
  )
}

#' @exportS3Method
check_coerce_type.lossy_cast <- function(args, mask, arg_name, env) {
  cast_expr(
    mask,
    args[["type"]],
    arg_name,
    TRUE,
    env,
    darg = args$mask
  )
}

#' @exportS3Method
check_coerce_type.coerce <- function(args, mask, arg_name, env) {
  cast_expr(
    mask,
    args[["type"]],
    arg_name,
    args$lossy,
    env,
    darg = args$mask
  )
}

#-- size

check_coerce_size <- function(args, mask, arg_name, env) {
  if (!is.null(args$size)) {
    UseMethod("check_coerce_size", args)
  } else {
    mask
  }
}

#' @exportS3Method
check_coerce_size.default <- function(args, mask, arg_name, env) {
  recycle_expr(
    mask,
    args[["size"]],
    arg_name,
    env,
    darg = args$mask,
    no_assignment = TRUE
  )
}

#' @exportS3Method
check_coerce_size.recycle <- function(args, mask, arg_name, env) {
  recycle_expr(
    mask,
    args[["size"]],
    arg_name,
    env,
    darg = args$mask
  )
}

#' @exportS3Method
check_coerce_size.coerce <- function(args, mask, arg_name, env) {
  recycle_expr(
    mask,
    args[["size"]],
    arg_name,
    env,
    darg = args$mask
  )
}

#-- validates

check_validates <- function(args, mask, arg_name, vnames, env) {
  for (i in seq_along(args$validations)) {
    v <- eval_tidy(args$validations[[i]], data = mask, env = env)

    if (!is_formula(v) && !is_function(v)) {
      abort_wrong_class(
        validate = as_label(args$validations[[i]]),
        given_class = class(v),
        expected_class = c("function", "formula")
      )
    }

    if (is_formula(v)) {
      v <- as_function(v)
    }

    if (is_function(v)) {
      logi <- eval_tidy(
        call2(v, sym(arg_name)),
        data = mask,
        env = env
      )
    }

    if (!is.logical(logi)) {
      abort_wrong_class(
        validate = as_label(args$validations[[i]]),
        given_class = class(logi),
        expected_class = "logical"
      )
    }

    if (isTRUE(args$na_rm) && anyNA(logi)) {
      logi <- logi[!is.na(logi)]
    } else if (anyNA(logi)) {
      abort_na_present(validate = as_label(args$validations[[i]]))
    }

    if (length(logi) == 0) {
      abort_empty(validate = as_label(args$validations[[i]]))
    }

    if (!all(logi)) {
      abort_validate_false(
        vname = vnames[i],
        validate = as_label(args$validations[[i]])
      )
    }
  }
}
