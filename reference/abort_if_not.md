# Ensure the truth of R expressions

If any of the expressions in `...` are not all `TRUE`,
[rlang::abort](https://rlang.r-lib.org/reference/abort.html) is called
for the first expression which was not
([all](https://rdrr.io/r/base/all.html)) `TRUE`. A replacement for
[stopifnot](https://rdrr.io/r/base/stopifnot.html) that utilises rlang,
cli, and glue.

## Usage

``` r
abort_if_not(..., .na_rm = FALSE, .message = NULL, .error_call = caller_env())
```

## Arguments

- ...:

  any number of R expressions, which should each evaluate to (a logical
  vector of [all](https://rdrr.io/r/base/all.html)) `TRUE` for no error
  to occur. Positive numbers are not `TRUE`, even when they are coerced
  to `TRUE` inside `if()` or in arithmetic computations in R. If the
  expressions are named, the names will be used in the error message.
  Names support [rlang
  injection](https://rlang.r-lib.org/reference/topic-inject.html) and
  [glue](https://glue.tidyverse.org/) interpreted string literals.

- .na_rm:

  if `TRUE`, NA values are removed in the logical vectors before
  evaluation.

- .message:

  single default error message for non-named expressions. Can be a glue
  string.

- .error_call:

  the call environment to use for error messages (passed to
  [rlang::abort](https://rlang.r-lib.org/reference/abort.html)).

## Value

NULL, called for side effects only.

## Details

`abort_if()` is the opposite of `abort_if_not()`, i.e. expressions
should evaluate to ([all](https://rdrr.io/r/base/all.html)) `FALSE` for
no error to occur.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

abort_if_not(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE

m <- matrix(c(1, 3, 3, 1), 2, 2)
abort_if_not(m == t(m), diag(m) == rep(1, 2)) # all TRUE

abort_if_not(1) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`:
#> ℹ In argument: `1`.
#> ! Returned <numeric>, not <logical>.
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `1`.
# ! Returned <integer>, not <logical>.

# A custom error message can be given for each expression:
m[1, 2] <- 12
abort_if_not("m must be symmetric" = m == t(m)) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`:
#> ℹ In argument: `m == t(m)`.
#> ! m must be symmetric
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `m == t(m)`.
# ! m must be symmetric

# Alternatively, one error message can be used for all expressions:
abort_if_not(
  m == t(m),
  diag(m) == rep(1, 2),
  .message = "m must be symmetric and have 1s on the diagonal."
) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`:
#> ℹ In argument: `m == t(m)`.
#> ! m must be symmetric and have 1s on the diagonal.
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `m == t(m)`.
# ! m must be symmetric and have 1s on the diagonal.

# Option to remove NA values before checking:
abort_if_not(c(TRUE, NA, TRUE), .na_rm = TRUE) # no error

# The `.error_call` argument can be used to specify where the error occurs,
# by default this is the caller environment.
myfunc <- function(x) abort_if_not(x)
myfunc(FALSE) |> try()
#> Error in myfunc(FALSE) : 
#>   Caused by error in `abort_if_not()`:
#> ℹ In argument: `x`.
#> ! Returned `FALSE`.
# Error in `myfunc()`:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `x`.
# ! Returned `FALSE`.

# abort_if() errors if any argument does not evaluate to (all) FALSE.
abort_if(1 == 1) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if()`:
#> ℹ In argument: `1 == 1`.
#> ! Returned `TRUE`.
# Error:
# Caused by error in `abort_if()`:
# ℹ In argument: `1 == 1`.
# ! Returned `TRUE`.

# Injection and glue can be used:
x <- "my error"
abort_if_not("{x}" = FALSE) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`:
#> ℹ In argument: `FALSE`.
#> ! my error
abort_if_not({{ x }} := FALSE) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`:
#> ℹ In argument: `FALSE`.
#> ! my error
abort_if_not(!!x := FALSE) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`:
#> ℹ In argument: `FALSE`.
#> ! my error
abort_if_not(FALSE, .message = "{x}") |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`:
#> ℹ In argument: `FALSE`.
#> ! my error
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `FALSE`.
# ! my error
x <- list("my bang-bang-bang error" = FALSE)
abort_if_not(!!!x) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`:
#> ℹ In argument: `FALSE`.
#> ! my bang-bang-bang error
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `FALSE`.
# ! my bang-bang-bang error
```
