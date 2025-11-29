# Ensure the truth of data-masked R expressions

If any of the expressions in `...`, evaluated within the data mask
`.data` (see
[rlang::args_data_masking](https://rlang.r-lib.org/reference/args_data_masking.html)),
are not all `TRUE`,
[rlang::abort](https://rlang.r-lib.org/reference/abort.html) is called
for the first expression which was not
([all](https://rdrr.io/r/base/all.html)) `TRUE`. The `.names` and
`.size` arguments can be used to check for given names and size of the
data.frame/list. The checking of size is from the
[vctrs](https://vctrs.r-lib.org/) package (using
[vctrs::vec_size](https://vctrs.r-lib.org/reference/vec_size.html)) and
thus applies [vctrs size
rules](https://vctrs.r-lib.org/articles/type-size.html).

## Usage

``` r
schema(.data, ...)

# S3 method for class 'data.frame'
schema(
  .data,
  ...,
  .na_rm = FALSE,
  .names = NULL,
  .size = NULL,
  .message = NULL,
  .error_call = caller_env(),
  .darg = caller_arg(.data)
)

# S3 method for class 'list'
schema(
  .data,
  ...,
  .na_rm = FALSE,
  .names = NULL,
  .size = NULL,
  .message = NULL,
  .error_call = caller_env(),
  .darg = caller_arg(.data)
)
```

## Arguments

- .data:

  a data.frame or list to use as the data mask.

- ...:

  any number of R expressions to be evaluated using `.data` as a data
  mask, which should each evaluate to (a logical vector of
  [all](https://rdrr.io/r/base/all.html)) `TRUE` for no error to occur.
  Positive numbers are not `TRUE`, even when they are coerced to `TRUE`
  inside `if()` or in arithmetic computations in R. If the expressions
  are named, the names will be used in the error message. Names support
  [rlang injection](https://rlang.r-lib.org/reference/topic-inject.html)
  and [glue](https://glue.tidyverse.org/) interpreted string literals.

- .na_rm:

  if `TRUE`, NA values are removed in the logical vectors before
  evaluation.

- .names:

  optional character vector of names which must be present in the
  `.data` data.frame/list. Can be a glue string.

- .size:

  optional positive scalar integerish value for the size that the
  `.data` data.frame/list must have.

- .message:

  single default error message for non-named expressions. Can be a glue
  string.

- .error_call:

  the call environment to use for error messages (passed to
  [rlang::abort](https://rlang.r-lib.org/reference/abort.html)).

- .darg:

  the argument name of `.data` to use in error messages.

## Value

.data is returned with attached class `with_schema` and attribute
`schema` containing the schema call to be enforced later.

## Details

See
[schema_cast](https://lj-jenkins.github.io/restrictr_test/reference/schema_cast.md)
and
[schema_recycle](https://lj-jenkins.github.io/restrictr_test/reference/schema_recycle.md)
for versions of `schema()` that attempt to coerce named elements of the
data to the desired type/size. See
[abort_if_not](https://lj-jenkins.github.io/restrictr_test/reference/abort_if_not.md)
for a non-data-masked version of this function.
[restrict](https://lj-jenkins.github.io/restrictr_test/reference/restrict.md)
can also be used for type casting, size recycling, and validation.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

li <- list(x = 1, y = "hi", z = \(x) x > 1)
li <- schema(li, x == 1, is.character(y), is.function(z)) # all TRUE

schema(li, x == 1, is.numeric(y)) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`:
#> ℹ In argument: `is.numeric(y)` for data mask `li`.
#> ! Returned `FALSE`.
# Error:
# Caused by error in `schema()`:
# ℹ In argument: `is.numeric(y)` for data mask `li`.
# ! Returned `FALSE`.

# A custom error message can be given for each expression:
schema(li, "y must be numeric, check input" = is.numeric(y)) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`:
#> ℹ In argument: `is.numeric(y)` for data mask `li`.
#> ! y must be numeric, check input
# Error:
# Caused by error in `schema()`:
# ℹ In argument: `is.numeric(y)` for data mask `li`.
# ! y must be numeric, check input

# Alternatively, one error message can be used for all expressions:
schema(
  li,
  x == 1, is.character(y), is.integer(z),
  .message = "li is invalid, check input"
) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`:
#> ℹ In argument: `is.integer(z)` for data mask `li`.
#> ! li is invalid, check input
# Error:
# Caused by error in `schema()`:
# ℹ In argument: `is.integer(z)` for data mask `li`.
# ! li is invalid, check input

# Option to remove NA values before checking:
df <- data.frame(x = c(5, NA, 10))
df <- schema(df, x > 4, .na_rm = TRUE) # no error

# `.names` and `.size` arguments can be used to check that given names
# are present and that the data has the desired (vctrs) size:
schema(li, .names = c("a", "x", "y", "b")) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`.
#> ! Named elements `a` and `b` not found in data mask `li`.
# Error:
# Caused by error in `schema()`.
# ! Named elements `a` and `b` not found in data mask `li`.

schema(li, .size = 5) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`.
#> ! Object `li` is of vctrs size `3`, not `5`.
# Error:
# Caused by error in `schema()`.
# ! Object `li` is of vctrs size `3`, not `5`.

# The `.error_call` argument can be used to specify where the error occurs,
# by default this is the caller environment.
myfunc <- function(df, ...) schema(df, ...)
myfunc(df, x > 4) |> try()
#> Error in myfunc(df, x > 4) : Caused by error in `schema()`:
#> ℹ In argument: `x > 4` for data mask `df`.
#> ! Contains `NA` values and `.na_rm` is set to `FALSE`.
# Error in `myfunc()`:
# Caused by error in `schema()`:
# ℹ In argument: `x > 4` for data mask `df`.
# ! Contains `NA` values and `.na_rm` is set to `FALSE`.

# Injection and glue can be used:
y <- "my error"
schema(li, "{y}" = x == 2) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`:
#> ℹ In argument: `x == 2` for data mask `li`.
#> ! my error
schema(li, {{ y }} := x == 2) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`:
#> ℹ In argument: `x == 2` for data mask `li`.
#> ! my error
schema(li, !!y := x == 2) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`:
#> ℹ In argument: `x == 2` for data mask `li`.
#> ! my error
schema(li, x == 2, .message = "{y}") |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`:
#> ℹ In argument: `x == 2` for data mask `li`.
#> ! my error
# Error:
# Caused by error in `schema()`:
# ℹ In argument: `x == 2` for data mask `li`.
# ! my error
y <- list("my bang-bang-bang error" = rlang::expr(x == 2))
schema(li, !!!y) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`:
#> ℹ In argument: `x == 2` for data mask `li`.
#> ! my bang-bang-bang error
# Error:
# Caused by error in `schema()`:
# ℹ In argument: `x == 2` for data mask `li`.
# ! my bang-bang-bang error
```
