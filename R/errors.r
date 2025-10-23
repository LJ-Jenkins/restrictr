abort_restrictr <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort(
    message,
    class = c(class, "restrictr_error"),
    ...,
    call = call
  )
}

abort_env <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_env"),
    ...,
    call = call
  )
}

abort_global_env <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_global_env"),
    ...,
    call = call
  )
}

abort_bool <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_bool"),
    ...,
    call = call
  )
}

abort_chr <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_chr"),
    ...,
    call = call
  )
}

abort_no_args_given <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_no_args_given"),
    ...,
    call = call
  )
}

abort_args_unnamed <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_args_unnamed"),
    ...,
    call = call
  )
}

abort_size_arg <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_size_arg"),
    ...,
    call = call
  )
}

abort_mask_arg <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_mask_arg"),
    ...,
    call = call
  )
}

abort_args_env <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_args_env"),
    ...,
    call = call
  )
}

abort_size_false <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_size_false"),
    ...,
    call = call
  )
}

abort_masked_names_not_present <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_masked_names_not_present"),
    ...,
    call = call
  )
}

abort_restrict_args <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_restrict_args"),
    ...,
    call = call
  )
}

abort_restrict_lossy <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_restrict_lossy"),
    ...,
    call = call
  )
}

abort_wrong_class <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_wrong_class"),
    ...,
    call = call
  )
}

abort_na_present <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_na_present"),
    ...,
    call = call
  )
}

abort_empty <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_empty"),
    ...,
    call = call
  )
}

abort_true_false <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_true_false"),
    ...,
    call = call
  )
}

abort_validate_false <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_validate_false"),
    ...,
    call = call
  )
}

abort_glue <- function(
    cnd,
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_restrictr(
    message,
    class = c(class, "restrictr_error_glue"),
    ...,
    parent = cnd,
    call = call
  )
}
