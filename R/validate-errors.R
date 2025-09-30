validate_env <- function(
    env,
    arg = caller_arg(env),
    allow_global = FALSE,
    class = NULL,
    call = caller_env(2),
    calling_fn = NULL) {
  if (!is_environment(env)) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Argument {.var {arg}} must be an environment."
      ),
      class = class,
      call = call
    )
  }
  if (!allow_global) {
    if (identical(env, globalenv())) {
      abort(
        c(
          "Error in {.fn {calling_fn}}",
          x = "Argument {.var {arg}} cannot be the global environment."
        ),
        class = class,
        call = call
      )
    }
  }
}

validate_bool <- function(
    bool,
    arg = caller_arg(bool),
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (!is_bool(bool)) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Argument {.var {arg}} must be {.var TRUE} or {.var FALSE}."
      ),
      class = class,
      call = call
    )
  }
}

validate_chr <- function(
    chr,
    arg = caller_arg(chr),
    allow_null = FALSE,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (allow_null && is.null(chr)) {
    return(invisible(NULL))
  }
  if (!is_character(chr)) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Argument {.var {arg}} must be a character vector."
      ),
      class = class,
      call = call
    )
  }
}

validate_args_given <- function(
    args,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (length(args) == 0) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "No arguments provided."
      ),
      class = class,
      call = call
    )
  }
}

validate_args_named <- function(
    nms,
    fn,
    mask = NULL,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (any(nms == "")) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Arguments{prmask(mask)} are not named with objects to {fn} in positions: {.var {which(nms == '')}}."
      ),
      class = class,
      call = call
    )
  }
}

validate_size_arg <- function(
    size,
    arg,
    mask = NULL,
    allow_null = FALSE,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (allow_null && is.null(size)) {
    return(invisible(NULL))
  }

  if (!is.numeric(size)) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Size argument for {.var {arg}}{prmask(mask)} is not numeric: class {.cls {class(size)}}."
      ),
      class = class,
      call = call
    )
  }

  if (!is_scalar_integerish(size)) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Size argument for {.var {arg}}{prmask(mask)} is not a scalar integerish value: {length_or_obj(size)}."
      ),
      class = class,
      call = call
    )
  }
}

validate_mask_arg_exists <- function(
    obj,
    mask_name,
    arg,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (is.null(obj)) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Mask object {.var {mask_name}} does not contain named element {.var {arg}}."
      ),
      class = class,
      call = call
    )
  }
}

validate_objs_exist <- function(
    obj_names,
    env,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (any(!obj_names %in% env_names(env))) {
    not_found <- obj_names[!obj_names %in% env_names(env)]
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Objects {.var {not_found}} are not found in the {.var .env} environment specified."
      ),
      class = class,
      call = call
    )
  }
}

check_size_true <- function(
    .data,
    .size,
    darg_name,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (vec_size(.data) != .size) {
    size_error(
      darg_name,
      vec_size(.data),
      .size,
      NULL,
      calling_fn,
      class,
      call
    )
  }
}

check_names_present <- function(
    .data,
    .names,
    darg_name,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (!all(.names %in% names2(.data))) {
    not_found <- .names[!.names %in% names2(.data)]
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Named elements {.var {not_found}} not found in {.var {darg_name}}."
      ),
      class = class,
      call = call
    )
  }
}

validate_restrict_args_call <- function(
    fn_name,
    arg_name,
    class = NULL,
    call = NULL,
    calling_fn = "restrict") {
  allowed <- c("validate", "coerce", "cast", "lossy_cast", "recycle")
  if (!fn_name %in% allowed) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "With named argument {.var {arg_name}}: calls must be built using the  functions: {.fn {allowed}}."
      ),
      class = class,
      call = call
    )
  }
}

validate_restrict_args_names <- function(
    args_names,
    fn_name,
    arg_name,
    class = NULL,
    call = NULL,
    calling_fn = "restrict") {
  args_names <- args_names[args_names != ""]
  chck <- c("type", "size", "mask")
  if (fn_name == "coerce") {
    chck <- c(chck, "lossy")
  }
  i <- !args_names %in% chck
  if (any(i)) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        x = "Invalid argument names for {.fn {fn_name}} for named argument {.var {arg_name}}: {.var {args_names[i]}}.",
        i = "Valid argument names are: {.var {chck}}."
      ),
      class = class,
      call = call
    )
  }
}
