# Ensure list elements are of a specific size and recycle them if not

If any of the expressions in `...`, evaluated within the data mask
`.data` (see
[rlang::args_data_masking](https://rlang.r-lib.org/reference/args_data_masking.html)),
are not of the same size, then the data element attempts to be recycled
to the size specified in the expression. The `.names` and `.size`
arguments can be used to check for given names and size of the list. The
checking of size and the recycling are from the
[vctrs](https://vctrs.r-lib.org/) package (using
[vctrs::vec_size](https://vctrs.r-lib.org/reference/vec_size.html) and
[vctrs::vec_recycle](https://vctrs.r-lib.org/reference/vec_recycle.html))
and thus apply the [vctrs size
rules](https://vctrs.r-lib.org/articles/type-size.html) and [vctrs
recycling
rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html).

## Usage

``` r
schema_recycle(.data, ...)

# S3 method for class 'list'
schema_recycle(
  .data,
  ...,
  .names = NULL,
  .size = NULL,
  .error_call = caller_env(),
  .darg = caller_arg(.data)
)
```

## Arguments

- .data:

  a list to use as the data mask.

- ...:

  any number of R expressions to be evaluated using `.data` as a data
  mask. Should follow the format of `named_element = expected_size`,
  e.g, `var_x = 10` or `var_x = vctrs::vec_size(var_y)`.

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

Object `.data`, with named elements recycled to the desired size. Also
attaches class `with_schema` and attribute `schema` containing the
schema_recycle call to be enforced later.

## Details

See
[schema](https://lj-jenkins.github.io/restrictr_test/reference/schema.md)
and
[schema_cast](https://lj-jenkins.github.io/restrictr_test/reference/schema_cast.md)
for validation and casting, as well as
[recycle_if_not](https://lj-jenkins.github.io/restrictr_test/reference/recycle_if_not.md)
for a non-data-masked version of recycling.
[restrict](https://lj-jenkins.github.io/restrictr_test/reference/restrict.md)
can also be used for type casting, size recycling, and validation.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

li <- list(x = 1, y = "hi", z = 1:2)
schema_recycle(li, x = 5, y = 3) |> lengths()
#> x y z 
#> 5 3 2 

# schema_recycle() follows `vctrs` recycling rules:
schema_recycle(li, z = 6) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `schema_recycle()`:
#> ℹ In argument: `z = 6` for data mask `li`.
#> ! Can't recycle `z` (size 2) to size 6.
# Error:
# Caused by error in `schema_recycle()`:
# ℹ In argument: `z = 6` for data mask `li`.
# ! Can't recycle `z` (size 2) to size 6.

# Other objects' lengths can be used as the size to
# recycle to, e.g.:
schema_recycle(li, x = vctrs::vec_size(z))$x |> length()
#> [1] 2

# schema_recycle() works sequentially, so references to objects will be
# after they have been evaluated:
li$a <- 1.25
schema_recycle(
  li,
  x = vctrs::vec_size(z),
  a = vctrs::vec_size(x)
)$a |> length()
#> [1] 2

# `.names` and `.size` arguments can be used to check that given names
# are present and that the data has the desired (vctrs) size:
schema_recycle(li, .names = c("a", "x", "y", "b")) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `schema_recycle()`.
#> ! Named element `b` not found in data mask `li`.
# Error:
# Caused by error in `schema_recycle()`.
# ! Named elements `a` and `b` not found in data mask `li`.

schema_recycle(li, .size = 5) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `schema_recycle()`.
#> ! Object `li` is of vctrs size `4`, not `5`.
# Error:
# Caused by error in `schema_recycle()`.
# ! Object `li` is of vctrs size `3`, not `5`.


# The `.error_call` argument can be used to specify where the error occurs,
# by default this is the caller environment.
myfunc <- function(li, ...) schema_recycle(li, ...)
myfunc(li, x = -5) |> try()
#> Error in myfunc(li, x = -5) : 
#>   Caused by error in `schema_recycle()`:
#> ℹ In argument: `x = -5` for data mask `li`.
#> ! Size argument is `-5`, needs to be positive scalar integerish.
# Error in `myfunc()`:
# Caused by error in `schema_recycle()`:
# ℹ In argument: `x = -5` for data mask `li`.
# ! Size argument is `-5`, needs to be positive scalar integerish.

# Injection and glue can be used:
li <- list(x = 1L)
x_name <- "x"
schema_recycle(li, "{x_name}" = 2)
#> $x
#> [1] 1 1
#> 
#> attr(,"class")
#> [1] "with_schema" "list"       
schema_recycle(li, !!x_name := 2)
#> $x
#> [1] 1 1
#> 
#> attr(,"class")
#> [1] "with_schema" "list"       
schema_recycle(li, {{ x_name }} := 2)
#> $x
#> [1] 1 1
#> 
#> attr(,"class")
#> [1] "with_schema" "list"       
x_list <- list(x = 2)
schema_recycle(li, !!!x_list)
#> $x
#> [1] 1 1
#> 
#> attr(,"class")
#> [1] "with_schema" "list"       
```
