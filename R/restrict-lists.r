prep_restrict_args <- function(args, arg_names, env, class, error_call) {
  rargs <- list()
  fn_names <- c()

  try_fetch(
    for (i in seq_along(args)) {
      fn_names[i] <- call_name(args[[i]])
    },
    error = function(cnd) {
      r_call_name_error(arg_names[i], args[[i]], class, error_call)
    }
  )

  for (i in seq_along(args)) {
    validate_restrict_args_call(
      fn_names[i],
      arg_names[i],
      class = class,
      call = error_call
    )

    given_args_names <- call_args_names(args[[i]])

    given_args <- call_args(args[[i]])

    given_args$mask <- given_args$mask %!||% as_name(given_args$mask)

    validate_lossy(
      given_args$lossy,
      given_args_names,
      fn_names[i],
      arg_names[i],
      given_args$mask,
      class,
      error_call
    )

    rargs[[i]] <- to_restrict_args(
      structure(
        restrict_list_c(given_args, given_args_names),
        class = fn_names[i]
      )
    )
  }

  return(set_names(rargs, arg_names))
}

to_restrict_args <- function(args) {
  UseMethod("to_restrict_args")
}

#' @exportS3Method
to_restrict_args.validate <- function(args) {
  args$coerce <- args$cast <- args$recycle <- args$lossy <- FALSE
  args
}

#' @exportS3Method
to_restrict_args.cast <- function(args) {
  args$coerce <- args$recycle <- args$lossy <- FALSE
  args$cast <- TRUE
  args
}

#' @exportS3Method
to_restrict_args.lossy_cast <- function(args) {
  args$coerce <- args$recycle <- FALSE
  args$cast <- args$lossy <- TRUE
  args
}

#' @exportS3Method
to_restrict_args.recycle <- function(args) {
  args$coerce <- args$cast <- args$lossy <- FALSE
  args$recycle <- TRUE
  args
}

#' @exportS3Method
to_restrict_args.coerce <- function(args) {
  args$coerce <- args$cast <- args$recycle <- TRUE
  args$lossy <- args$lossy %||% FALSE
  args
}
