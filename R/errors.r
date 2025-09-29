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
  abort(
    c(
      "Error in {.fn {calling_fn}}",
      i = r_arg_mask %!||% r_prmask(r_arg_mask[1], r_arg_mask[2]),
      x = msg %||% "Expression {.var {expr}}{prmask(mask)} returned {.var {if (check_false) 'FALSE' else 'TRUE'}}."
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
