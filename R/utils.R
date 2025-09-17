`%""%` <- function(lhs, rhs) if (lhs == "") rhs else lhs

`%!||%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

length_or_obj <- function(x) {
  if (length(x) > 1) {
    y <- "object length {.var {length(x)}}"
  } else {
    y <- "value {.var {x}}"
  }
  format_inline(y)
}

glue_names <- function(obj, call) {
  names2(obj) |>
    vapply(\(x) glue(x, .envir = call), character(1))
}

glue_chr <- function(chr, call) {
  vapply(chr, \(x) glue(x, .envir = call), character(1))
}

unnamed_combine <- function(li, names, unnamed_name = "validations") {
  c(
    li[names != ""],
    set_names(list(li[names == ""]), unnamed_name)
  )
}

#' @importFrom utils capture.output
ptype_show <- function(.x) {
  gsub("Prototype: ", "", capture.output(vec_ptype_show(.x)))
}

quo_string <- function(.q) {
  if (is_quosure(.q)) {
    .q |>
      quo_get_expr() |>
      as_label()
  } else {
    as_label(.q)
  }
}

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
        i = "Argument {.var {arg}} must be an environment."
      ),
      class = class,
      call = call
    )
  }
  # if (!allow_global) {
  #   if (identical(env, globalenv())) {
  #     abort(
  #       c(
  #         "Error in {.fn {calling_fn}}",
  #         i = "Argument {.var {arg}} cannot be the global environment."
  #       ),
  #       class = class,
  #       call = call
  #     )
  #   }
  # }
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
        i = "Argument {.var {arg}} must be {.var TRUE} or {.var FALSE}."
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
        i = "Argument {.var {arg}} must be a character vector."
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
      c("Error in {.fn {calling_fn}}", i = "No arguments provided."),
      class = class,
      call = call
    )
  }
}

validate_args_named <- function(nms, fn, class = NULL, call = NULL, calling_fn = NULL) {
  if (any(nms == "")) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        i = format_inline(
          "Arguments are not named with objects to ",
          "{fn} in positions: {.var {which(nms == '')}}."
        )
      ),
      class = class,
      call = call
    )
  }
}

validate_mask_args_named <- function(
    mask,
    mask_names,
    fn,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (any(mask_names == "")) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        i = format_inline(
          "Arguments for mask {.var {mask}} are not named with ",
          "objects to {fn} in positions: {.var {which(mask_names == '')}}."
        )
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
    if (is.null(mask)) {
      abort(
        c(
          "Error in {.fn {calling_fn}}",
          i = "Size argument for {.var {arg}} is not numeric: class {.cls {class(size)}}."
        ),
        class = class,
        call = call
      )
    } else {
      abort(
        c(
          "Error in {.fn {calling_fn}}",
          i = format_inline(
            "Size argument for mask object {.var {mask}[[{arg}]]} is not numeric: ",
            "class {.cls {class(size)}}."
          )
        ),
        class = class,
        call = call
      )
    }
  }

  if (!is_scalar_integerish(size)) {
    if (is.null(mask)) {
      abort(
        c(
          "Error in {.fn {calling_fn}}",
          i = format_inline(
            "Size argument for {.var {arg}} is not a scalar integerish value: ",
            "{length_or_obj(size)} of class {.cls {class(size)}}."
          )
        ),
        class = class,
        call = call
      )
    } else {
      abort(
        c(
          "Error in {.fn {calling_fn}}",
          i = format_inline(
            "Size argument for mask object {.var {mask}[[{arg}]]} is not a scalar ",
            "integerish value: {length_or_obj(size)} of class {.cls {class(size)}}."
          )
        ),
        class = class,
        call = call
      )
    }
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
        i = "Mask object {.var {mask_name}} does not contain variable {.var {arg}}."
      ),
      class = class,
      call = call
    )
  }
}

validate_mask_exists <- function(
    obj,
    mask_name,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (is.null(obj)) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        i = format_inline(
          "Mask object {.var {mask_name}} is not found in the ",
          "{.var .env} environment specified."
        )
      ),
      class = class, call = call
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
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        i = format_inline(
          "Objects {.var {obj_names[!obj_names %in% env_names(env)]}} ",
          "are not found in the {.var .env} environment specified."
        )
      ),
      class = class,
      call = call
    )
  }
}

abort_not_logical <- function(
    obj,
    expr,
    expr_name = NULL,
    mask = NULL,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (!is.null(expr_name)) {
    expr <- format_inline(expr_name, " = ", expr)
  }
  msg <- format_inline(
    "Expression {.var {expr}} ",
    "{if (!is.null(mask)) paste0(' for object {.var ', mask, '}')} ",
    "must evaluate to class {.cls logical} not {.cls {class(obj)}}."
  )
  abort(c("Error in {.fn {calling_fn}}", i = msg), class = class, call = call)
}

check_size_true <- function(
    .data,
    .size,
    darg_name,
    class = NULL,
    call = NULL,
    calling_fn = NULL) {
  if (vec_size(.data) != .size) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        i = format_inline(
          "Object {.var {darg_name}} must have {.pkg vctrs} size ",
          "{.var { .size}}, not {.var {vec_size(.data)}}."
        )
      ),
      class = class,
      call = call
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
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        i = format_inline(
          "Objects {.var { .names[!.names %in% names2(.data)]}} ",
          "not found in {.var {darg_name}}."
        )
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
    calling_fn = NULL) {
  if (!fn_name %in% c("validate", "coerce", "cast", "lossy_cast", "recycle")) {
    abort(
      c(
        "Error in {.fn {calling_fn}}",
        i = format_inline(
          "With named argument {.var {arg_name}}: calls to ",
          "{.fn restrict} must be built using the  functions: ",
          "{.fn {c('validate', 'cast', 'lossy_cast', 'recycle', 'coerce')}}."
        )
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
    calling_fn = NULL) {
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
        i = format_inline(
          "Invalid argument names for {.fn {fn_name}} for ",
          "named argument {.var {arg_name}}: {.var {args_names[i]}}. ",
          "Valid argument names are: {.var {chck}}."
        )
      ),
      class = class,
      call = call
    )
  }
}
