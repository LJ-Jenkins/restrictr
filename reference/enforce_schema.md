# Enforce an attached schema

For objects with an attached schema (from
[schema](https://lj-jenkins.github.io/restrictr_test/reference/schema.md),
[schema_cast](https://lj-jenkins.github.io/restrictr_test/reference/schema_cast.md),
or
[schema_recycle](https://lj-jenkins.github.io/restrictr_test/reference/schema_recycle.md)),
enforce the schema on the object.

## Usage

``` r
enforce_schema(.data, ...)

# S3 method for class 'with_schema'
enforce_schema(
  .data,
  ...,
  .error_call = caller_env(),
  .darg = caller_arg(.data)
)
```

## Arguments

- .data:

  a data.frame or list containing an attached schema (of class
  `with_schema`).

- ...:

  for future extensions - leave empty.

- .error_call:

  the call environment to use for error messages (passed to
  [rlang::abort](https://rlang.r-lib.org/reference/abort.html)).

- .darg:

  the argument name of `.data` to use in error messages.

## Value

Object `.data`, with no change if attached schema is from `schema`, or
with named elements cast or recycled to the desired type/size if
attached schema is from `schema_cast` or `schema_recycle` respectively.

## Details

See
[schema](https://lj-jenkins.github.io/restrictr_test/reference/schema.md),
[schema_cast](https://lj-jenkins.github.io/restrictr_test/reference/schema_cast.md)
and
[schema_recycle](https://lj-jenkins.github.io/restrictr_test/reference/schema_recycle.md)
for creating and attaching a schema.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

li <- list(x = 1, y = "hi")
li_with_schema <- schema(li, x == 1, is.character(y))

li_with_schema$y <- 1
enforce_schema(li_with_schema) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `enforce_schema()`:
#> ℹ In argument: `is.character(y)` for data mask `li_with_schema`.
#> ! Returned `FALSE`.
# Error:
# Caused by error in `enforce_schema()`:
# ℹ In argument: `is.character(y)` for data mask `li_with_schema`.
# ! Returned `FALSE`.

df <- data.frame(x = 1:2)
df_with_schema <- schema_cast(df, x = integer(), .lossy = TRUE)

df_with_schema$x <- c(1.5, 2.5)
enforce_schema(df_with_schema)$x
#> [1] 1 2
# 1 2

li_with_schema <- schema_recycle(li, x = 2, y = 3)
li_with_schema$y <- "hi"
enforce_schema(li_with_schema)$y
#> [1] "hi" "hi" "hi"
# "hi" "hi" "hi"

# The `.error_call` argument can be used to specify where the error occurs,
# by default this is the caller environment.
myfunc <- function(.x) enforce_schema(.x)
li_with_schema$x <- 1:3
myfunc(li_with_schema) |> try()
#> Error in myfunc(li_with_schema) : 
#>   Caused by error in `enforce_schema()`:
#> ℹ In argument: `x = 2` for data mask `.x`.
#> ! Can't recycle `x` (size 3) to size 2.
# Error in `myfunc()`:
# Caused by error in `enforce_schema()`:
# ℹ In argument: `x = 2` for data mask `li`.
# ! Can't recycle `x` (size 3) to size 2.
```
