# allto be changed when errors are updated to be by class

cnd_error <- function(cnd, calling_fn, error_class, error_call) {
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      x = "{conditionMessage(cnd)}"
    ),
    class = error_class,
    call = error_call
  )
}

parent_error <- function(expr, mask, cnd, calling_fn, error_class, error_call) {
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      i = "Error evaluating expression {.var {expr}}{prmask(mask)}: "
    ),
    parent = cnd,
    class = error_class,
    call = error_call
  )
}

expr_error <- function(expr, mask, cnd, calling_fn, error_class, error_call) {
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      i = "Error evaluating expression {.var {expr}}{prmask(mask)}: ",
      x = "{conditionMessage(cnd)}"
    ),
    class = error_class,
    call = error_call
  )
}

tf_error <- function(
    expr,
    mask,
    r_arg_mask,
    msg,
    check_false,
    calling_fn,
    error_class,
    error_call) {
  cf <- if (is.null(check_false)) {
    "an empty vector"
  } else if (is.na(check_false)) {
    "`NA`"
  } else if (check_false) {
    "`FALSE`"
  } else if (!check_false) {
    "`TRUE`"
  }

  abort(
    c(
      "Error in {.fn {calling_fn}}",
      i = r_arg_mask %!||% r_prmask(r_arg_mask[1], r_arg_mask[2]),
      x = msg %||% "Expression {.var {expr}}{prmask(mask)} returned {cf}."
    ),
    class = error_class,
    call = error_call
  )
}

not_logical_error <- function(
    expr,
    mask,
    r_arg_mask,
    actual_class,
    calling_fn,
    error_class,
    error_call) {
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      i = r_arg_mask %!||% r_prmask(r_arg_mask[1], r_arg_mask[2]),
      x = "Expression {.var {expr}}{prmask(mask)} returned {.cls {actual_class}}, not {.cls logical}."
    ),
    class = error_class,
    call = error_call
  )
}

class_error <- function(
    obj_name,
    actual_class,
    expected_class,
    mask,
    calling_fn,
    error_class,
    error_call) {
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      x = "Object {.var {obj_name}}{prmask(mask)} is of type {.cls {actual_class}}, not {.cls {expected_class}}."
    ),
    class = error_class,
    call = error_call
  )
}

size_error <- function(
    obj_name,
    actual_size,
    expected_size,
    mask,
    calling_fn,
    error_class,
    error_call) {
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      x = "Object {.var {obj_name}}{prmask(mask)} is of {.pkg vctrs} size {.var {actual_size}}, not {.var {expected_size}}."
    ),
    class = error_class,
    call = error_call
  )
}

mask_error <- function(mask_name, calling_fn, error_class, error_call) {
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      x = "Mask object {.var {mask_name}} is not found in the {.var .env} environment specified."
    ),
    class = error_class,
    call = error_call
  )
}

r_call_name_error <- function(arg_name, expr, error_class, error_call) {
  abort(
    c(
      "Error in {.fn restrict}",
      i = "Each validation call must be one of {.fn validate}, {.fn cast}, {.fn lossy_cast}, {.fn recycle} or {.fn coerce}.",
      x = "Not {.var {arg_name} = {expr}}"
    ),
    class = error_class,
    call = error_call
  )
}

r_ff_error <- function(
    expr,
    r_arg_mask,
    actual_class,
    calling_fn,
    error_class,
    error_call) {
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      i = r_arg_mask %!||% r_prmask(r_arg_mask[1], r_arg_mask[2]),
      x = "Expression {.var {expr}} must be a {.cls function} or {.cls formula}, not {.cls {actual_class}}."
    ),
    class = error_class,
    call = error_call
  )
}

r_lossy_error <- function(
    arg,
    mask,
    error_class,
    error_call) {
  abort(
    c(
      "Error in {.fn restrict}",
      x = "Error for {.var {arg}}{prmask(mask)}: {.var lossy} argument can only be given for {.fn coerce} calls."
    ),
    class = error_class,
    call = error_call
  )
}

vctrs_is_error <- function(
    obj_name,
    class1,
    class2,
    mask,
    calling_fn,
    error_class,
    error_call) {
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      x = "Object {.var {obj_name}}{prmask(mask)} of type {.cls {class1}} cannot be compared with {.cls {class2}} in {.pkg vctrs}."
    ),
    class = error_class,
    call = error_call
  )
}
