prep_restrict_args <- function(args, arg_names, env, class, error_call) {
  rargs <- list()
  for (i in seq_along(args)) {
    fn_name <- try_fetch(
      call_name(args[[i]]),
      error = function(cnd) {
        abort(
          c(
            "Error in {.fn restrict}",
            i = format_inline(
              "Each argument to {.fn restrict} must be a call to ",
              "{.fn validate}, {.fn cast}, {.fn lossy_cast}, {.fn recycle} or {.fn coerce}, ",
              "not {.var {arg_names[i]} = {args[[i]]}}."
            )
          ),
          class = class,
          call = error_call
        )
      }
    )

    validate_restrict_args_call(
      fn_name,
      arg_names[i],
      class = class,
      call = error_call,
      calling_fn = "restrict"
    )

    given_args_names <- call_args_names(args[[i]])

    validate_restrict_args_names(
      given_args_names,
      fn_name,
      arg_names[i],
      class = class,
      call = error_call,
      calling_fn = "restrict"
    )

    given_args <- call_args(args[[i]])

    given_args$mask <- given_args$mask %!||% as_name(given_args$mask)

    rargs[[i]] <- to_restrict_args(
      structure(
        unnamed_combine(given_args, given_args_names),
        class = fn_name
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
