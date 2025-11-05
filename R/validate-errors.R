validate_env <- function(
    env,
    allow_global = FALSE,
    call = NULL,
    restrictr_fn = NULL) {
  if (!is_environment(env)) {
    abort_env(call = call, restrictr_fn = restrictr_fn)
  }
  if (!allow_global) {
    if (identical(env, globalenv())) {
      abort_global_env(call = call, restrictr_fn = restrictr_fn)
    }
  }
}

validate_bool <- function(
    bool,
    arg = caller_arg(bool),
    inarg = NULL,
    call = NULL,
    restrictr_fn = NULL) {
  if (!is_bool(bool)) {
    abort_bool(
      bool = arg,
      inarg = inarg,
      call = call,
      restrictr_fn = restrictr_fn
    )
  }
}

validate_chr <- function(
    chr,
    arg = caller_arg(chr),
    allow_null = FALSE,
    call = NULL,
    restrictr_fn = NULL) {
  if (allow_null && is.null(chr)) {
    return(invisible(NULL))
  }
  if (!is_character(chr)) {
    abort_chr(chr = arg, call = call, restrictr_fn = restrictr_fn)
  }
}

validate_args_given <- function(
    args,
    call = NULL,
    restrictr_fn = NULL) {
  if (length(args) == 0) {
    abort_no_args_given(call = call, restrictr_fn = restrictr_fn)
  }
}

validate_args_named <- function(
    nms,
    action,
    call = NULL,
    restrictr_fn = NULL) {
  if (any(nms == "")) {
    abort_args_unnamed(
      action = action,
      i = which(nms == ""),
      call = call,
      restrictr_fn = restrictr_fn
    )
  }
}

validate_size_arg <- function(
    size,
    arg,
    allow_null = FALSE,
    sname = "Size",
    call = NULL,
    restrictr_fn = NULL) {
  if (allow_null && is.null(size)) {
    return(invisible(NULL))
  }

  if (!is.numeric(size)) {
    abort_size_arg(
      size_issue = format_inline("{.cls numeric}"),
      size_given = format_inline("{.cls {class(size)}}"),
      sname = sname,
      call = call,
      restrictr_fn = restrictr_fn
    )
  }

  if (!is_scalar_integerish(size) || size < 1) {
    abort_size_arg(
      size_given = length_or_obj(size),
      sname = sname,
      call = call,
      restrictr_fn = restrictr_fn
    )
  }
}

validate_mask_arg_exists <- function(
    obj,
    mask_name,
    arg,
    call = NULL,
    restrictr_fn = NULL) {
  if (is.null(obj)) {
    abort_mask_arg(
      mask_name = mask_name,
      arg = arg,
      call = call,
      restrictr_fn = restrictr_fn
    )
  }
}

validate_objs_exist <- function(
    obj_names,
    env,
    call = NULL,
    restrictr_fn = NULL) {
  if (any(!obj_names %in% env_names(env))) {
    abort_args_env(
      not_found = obj_names[!obj_names %in% env_names(env)],
      call = call,
      restrictr_fn = restrictr_fn
    )
  }
}

check_size_true <- function(
    .data,
    .size,
    darg_name,
    call = NULL,
    restrictr_fn = NULL) {
  if (vec_size(.data) != .size) {
    abort_size_false(
      arg = darg_name,
      actual_size = vec_size(.data),
      expected_size = .size,
      call = call,
      restrictr_fn = restrictr_fn
    )
  }
}

check_names_present <- function(
    .data,
    .names,
    darg_name,
    call = NULL,
    restrictr_fn = NULL) {
  if (!all(.names %in% names2(.data))) {
    abort_masked_names_not_present(
      not_found = .names[!.names %in% names2(.data)],
      mask = darg_name,
      call = call,
      restrictr_fn = restrictr_fn
    )
  }
}

validate_restrict_args_call <- function(
    fn_name,
    arg_name,
    call = NULL,
    restrictr_fn = NULL) {
  allowed <- c("validate", "coerce", "cast", "lossy_cast", "recycle")
  if (!fn_name %in% allowed) {
    abort_restrict_args(
      arg = arg_name,
      allowed = allowed,
      given = fn_name,
      call = call,
      restrictr_fn = restrictr_fn
    )
  }
}

validate_lossy <- function(
    lossy,
    given_args_names,
    fn_name,
    inarg = NULL,
    call = NULL,
    restrictr_fn = NULL) {
  if ("lossy" %in% given_args_names && fn_name != "coerce") {
    abort_restrict_lossy(
      given_fn = fn_name,
      inarg = inarg,
      call = call,
      restrictr_fn = restrictr_fn
    )
  } else if ("lossy" %in% given_args_names && fn_name == "coerce") {
    validate_bool(
      lossy,
      arg = "lossy",
      inarg = inarg,
      call = call,
      restrictr_fn = restrictr_fn
    )
  }
}
