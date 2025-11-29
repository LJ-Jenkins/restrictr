# Ensure data.frame/list elements are of a specific type and cast them if not

If any of the expressions in `...`, evaluated within the data mask
`.data` (see
[rlang::args_data_masking](https://rlang.r-lib.org/reference/args_data_masking.html)),
are not of the same type, then the data element attempts to be cast to
the type specified in the expression. The `.names` and `.size` arguments
can be used to check for given names and size of the data.frame/list.
The checking of type and the type conversion are from the
[vctrs](https://vctrs.r-lib.org/) package (using
[vctrs::vec_is](https://vctrs.r-lib.org/reference/vec_assert.html) and
[vctrs::vec_cast](https://vctrs.r-lib.org/reference/vec_cast.html)) and
thus stick to the [vctrs type conversion
rules](https://vctrs.r-lib.org/reference/faq-compatibility-types.html).
The checking of size is also from [vctrs](https://vctrs.r-lib.org/)
(using
[vctrs::vec_size](https://vctrs.r-lib.org/reference/vec_size.html)) and
thus applies [vctrs size
rules](https://vctrs.r-lib.org/articles/type-size.html).

## Usage

``` r
schema_cast(.data, ...)

# S3 method for class 'data.frame'
schema_cast(
  .data,
  ...,
  .lossy = FALSE,
  .names = NULL,
  .size = NULL,
  .error_call = caller_env(),
  .darg = caller_arg(.data)
)

# S3 method for class 'list'
schema_cast(
  .data,
  ...,
  .lossy = FALSE,
  .names = NULL,
  .size = NULL,
  .error_call = caller_env(),
  .darg = caller_arg(.data)
)
```

## Arguments

- .data:

  a data.frame or list to use as the data mask.

- ...:

  any number of R expressions to be evaluated using `.data` as a data
  mask. Should follow the format of `named_element = expected_type`,
  e.g, `var_x = integer()` or `var_x = var_y`.

- .lossy:

  if `TRUE`, lossy casting is undertaken.

- .names:

  optional character vector of names which must be present in the
  `.data` data.frame/list. Can be a glue string.

- .size:

  optional positive scalar integerish value for the size that the
  `.data` data.frame/list must have.

- .error_call:

  the call environment to use for error messages (passed to
  [rlang::abort](https://rlang.r-lib.org/reference/abort.html)).

- .darg:

  the argument name of `.data` to use in error messages.

## Value

Object `.data`, with named elements cast to the desired type. Also
attaches class `with_schema` and attribute `schema` containing the
schema_cast call to be enforced later.

## Details

See
[schema](https://lj-jenkins.github.io/restrictr_test/reference/schema.md)
and
[schema_recycle](https://lj-jenkins.github.io/restrictr_test/reference/schema_recycle.md)
for validation and recycling, as well as
[cast_if_not](https://lj-jenkins.github.io/restrictr_test/reference/cast_if_not.md)
for a non-data-masked version of casting.
[restrict](https://lj-jenkins.github.io/restrictr_test/reference/restrict.md)
can also be used for type casting, size recycling, and validation.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

li <- list(x = 1.1, y = "hi", z = 1L:2L)
# Input remains the same if types match
li <- schema_cast(li, x = double(), y = character(), z = integer())

# By default, lossy casting is not allowed:
schema_cast(li, x = integer()) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `schema_cast()`:
#> ℹ In argument: `x = integer()` for data mask `li`.
#> ! Can't convert from `x` <double> to <integer> due to loss of precision.
#> • Locations: 1
# Error:
# Caused by error in `schema_cast()`:
# ℹ In argument: `x = integer()` for data mask `li`.
# ! Can't convert from `x` <double> to <integer> due to loss of precision.
# • Locations: 1

# Lossy casting can be enabled with the `.lossy` argument:
schema_cast(li, x = integer(), .lossy = TRUE)$x
#> [1] 1

# Other objects can be used as the type to cast to, e.g.:
schema_cast(li, z = x)$z |> class()
#> [1] "numeric"

# schema_cast() works sequentially, so references to objects will be
# after they have been evaluated:
li$a <- 1L
schema_cast(li, z = double(), a = z)$a |> class()
#> [1] "numeric"

# `.names` and `.size` arguments can be used to check that given names
# are present and that the data has the desired (vctrs) size:
schema_cast(li, .names = c("a", "x", "y", "b")) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `schema_cast()`.
#> ! Named element `b` not found in data mask `li`.
# Error:
# Caused by error in `schema_cast()`.
# ! Named elements `a` and `b` not found in data mask `li`.

schema_cast(li, .size = 5) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `schema_cast()`.
#> ! Object `li` is of vctrs size `4`, not `5`.
# Error:
# Caused by error in `schema_cast()`.
# ! Object `li` is of vctrs size `3`, not `5`.

# The `.error_call` argument can be used to specify where the error occurs,
# by default this is the caller environment.
myfunc <- function(li, ...) schema_cast(li, ...)
myfunc(li, x = character()) |> try()
#> Error in myfunc(li, x = character()) : 
#>   Caused by error in `schema_cast()`:
#> ℹ In argument: `x = character()` for data mask `li`.
#> ! Can't convert `x` <double> to <character>.
# Error in `myfunc()`:
# Caused by error in `schema_cast()`:
# ℹ In argument: `x = character()` for data mask `li`.
# ! Can't convert `x` <double> to <character>.

# Injection and glue can be used:
li <- list(x = 1L)
x_name <- "x"
schema_cast(li, "{x_name}" = double())
#> $x
#> [1] 1
#> 
#> attr(,"class")
#> [1] "with_schema" "list"       
schema_cast(li, !!x_name := double())
#> $x
#> [1] 1
#> 
#> attr(,"class")
#> [1] "with_schema" "list"       
schema_cast(li, {{ x_name }} := double())
#> $x
#> [1] 1
#> 
#> attr(,"class")
#> [1] "with_schema" "list"       
x_list <- list(x = double())
schema_cast(li, !!!x_list)
#> $x
#> [1] 1
#> 
#> attr(,"class")
#> [1] "with_schema" "list"       
```
