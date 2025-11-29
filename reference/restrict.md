# Restrict arguments to a specific type/size and test adherence to validation functions/formulas.

This function takes any number of named expressions referring to objects
in the environment specified by the `.env` argument, checking and
possibly coercing them to the specified type and/or size, and also
checking them against any number of validation functions/formulas. Using
the function keywords of `validate`, `cast`, `lossy_cast`, `recycle`,
and `coerce` within the expressions allows for different behaviours:

- `validate`: checks that the object is of the specified type/size and
  adheres to the validations, throwing an error if not.

- `cast`: differs from `validate` by attempting to cast the object if it
  is not of the specified type.

- `lossy_cast`: differs from `cast` by allowing lossy casting.

- `recycle`: differs from `validate` by attempting to recycle the object
  if it is not of the specified size.

- `coerce`: differs from `validate` by attempting to cast and/or recycle
  the object if it is not of the specified type and/or size. Casting is
  not lossy by default but can be made lossy by adding `lossy = TRUE`
  within the `coerce()` call.

## Usage

``` r
restrict(..., .env = caller_env(), .error_call = caller_env())
```

## Arguments

- ...:

  any number of named R expressions, with the names referring to objects
  in the environment specified by the `.env` argument, and the
  expressions built using the functions: `validate()`, `cast()`,
  `lossy_cast()`, `recycle()`, and `coerce()`.

- .env:

  the environment to use for the evaluation of the
  casting/recycling/validation expressions and the assignment of altered
  objects. Cannot be the global environment.

- .error_call:

  the call environment to use for error messages (passed to
  [rlang::abort](https://rlang.r-lib.org/reference/abort.html)).

## Value

NULL, but objects named in `...` will be changed in the `.env`
environment specified.

## Details

These functions accept the named arguments `type`, `size` and `mask`
(`lossy` is also accepted within `coerce()`):

- `type`: an R object of the desired type (e.g.
  [`integer()`](https://rdrr.io/r/base/integer.html),
  [`double()`](https://rdrr.io/r/base/double.html),
  [`character()`](https://rdrr.io/r/base/character.html),
  [`list()`](https://rdrr.io/r/base/list.html)). The type checking and
  casting are done using the [vctrs](https://vctrs.r-lib.org/) package
  (using
  [vctrs::vec_is](https://vctrs.r-lib.org/reference/vec_assert.html) and
  [vctrs::vec_cast](https://vctrs.r-lib.org/reference/vec_cast.html))
  and thus stick to the [vctrs type conversion
  rules](https://vctrs.r-lib.org/reference/faq-compatibility-types.html).

- `size`: a positive scalar integerish value specifying the desired
  size. The size checking and recycling are done using the
  [vctrs](https://vctrs.r-lib.org/) package (using
  [vctrs::vec_size](https://vctrs.r-lib.org/reference/vec_size.html) and
  [vctrs::vec_recycle](https://vctrs.r-lib.org/reference/vec_recycle.html))
  and thus stick to the [vctrs recycling
  rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html).

- `mask`: the name of an optional data frame or list (found within the
  `.env` environment) to use as a data mask for evaluations.

- `na_rm`: if `TRUE`, NA values are removed in the logical vectors
  before evaluation.

All other inputs should be validation functions or formulas that
evaluate to logical. `restrict` first evaluates type, then size, then
the validations. Any change from the prior expression is reflected in
subsequent expressions, i.e. if an object is cast to a new type then
that new type is used for the size check and validations. If you do not
wish to use the [vctrs](https://vctrs.r-lib.org/) type/size checking,
then instead only give validations such as `~ is.integer(.x)` and
`~ length(.x) == 1`. However, these will only validate, not cast or
recycle. `restrict` is designed for the checking of numerous objects,
for a smaller number of objects to check see
[abort_if_not](https://lj-jenkins.github.io/restrictr_test/reference/abort_if_not.md),
[cast_if_not](https://lj-jenkins.github.io/restrictr_test/reference/cast_if_not.md),[recycle_if_not](https://lj-jenkins.github.io/restrictr_test/reference/recycle_if_not.md),
[schema](https://lj-jenkins.github.io/restrictr_test/reference/schema.md),
[schema_cast](https://lj-jenkins.github.io/restrictr_test/reference/schema_cast.md)
and
[schema_recycle](https://lj-jenkins.github.io/restrictr_test/reference/schema_recycle.md).

## Examples

``` r
# NB: Will not alter the global environment so examples
#     here are wrapped with local(). Some of these examples
#     are also expected to produce an error so are piped to try().

x <- 1L
restrict(x = validate(type = integer())) |> try()
# Error:
# Caused by error in `recycle_if_not()`.
# ! `env` must not be the global environment.

# Functions used within restrict() determine behaviour:
local({
  x <- 1L
  # validate() for validations only.
  restrict(x = validate(type = integer(), size = 1, ~ .x < 5))

  # cast() for validations and type casting (lossy_cast() for lossy casting).
  restrict(x = cast(type = double(), size = 1, ~ .x < 5))
  class(x) |> print()

  # recycle() for validations and size recycling.
  restrict(x = recycle(type = double(), size = 3, ~ .x < 5))
  length(x) |> print()

  # coerce() for validations, type casting and size recycling.
  x <- 1L
  restrict(x = coerce(type = double(), size = 3, ~ .x < 5))
  cat(class(x), length(x), sep = ", ")
})
#> [1] "numeric"
#> [1] 3
#> numeric, 3
# "numeric"
# 3
# numeric, 3

# By default, lossy casting is not allowed:
local({
  x <- 1.5
  restrict(x = cast(type = integer())) |> try()
})
#> Error in eval(quote({ : Caused by error in `restrict()`:
#> ℹ In argument: `x`.
#> ! Can't convert from `x` <double> to <integer> due to loss of precision.
#> • Locations: 1
# Error:
# Caused by error in `restrict()`:
# ℹ In argument: `x`.
# ! Can't convert from `x` <double> to <integer> due to loss of precision.
# • Locations: 1

# Allow lossy casting using lossy_cast() or coerce() with `lossy = TRUE`:
local({
  x <- 1.5
  restrict(x = lossy_cast(type = integer()))
  cat(class(x), ", ", sep = "")

  # or

  x <- 1.5
  restrict(x = coerce(type = integer(), lossy = TRUE))
  cat(class(x))
})
#> integer, integer
# integer, integer

# Other objects can be used as the type to cast to or size to
# recycle to, e.g.:
local({
  x <- 1L
  y <- 2.3
  z <- 3L
  restrict(x = coerce(type = y, size = z))
  cat(class(x), length(x), sep = ", ")
})
#> numeric, 3
# numeric, 3

# restrict works sequentially, so references to objects will be
# after they have been evaluated:
local({
  x <- y <- 1L
  restrict(
    x = cast(type = double()),
    y = cast(type = x)
  )
  cat(class(x), class(y), sep = ", ")
})
#> numeric, numeric
# numeric, numeric

# Multiple validations can be given and type and size checking can be done
# within if base R checking is preferred (no coercion will occur):
local({
  x <- 1L
  restrict(
    x = validate(
      ~ is.integer(.x),
      ~ length(.x) == 1,
      \(.x) all(.x > 0),
      \(.x) !is.character(.x)
    )
  )
})

# Validations can be named for clearer error messages:
local({
  x <- c(1, -2, 3)
  restrict(
    x = validate(
      "x must be positive" = \(.x) all(.x > 0),
    )
  ) |> try()
})
#> Error in eval(quote({ : Caused by error in `restrict()`:
#> ℹ In argument: `x`.
#> ! x must be positive
# Error:
# Caused by error in `restrict()`:
# ℹ In argument: `x`.
# ! x must be positive

# The `.env` argument determines the expression and assignment environment:
local({
  x <- 1L
  e <- new.env()
  e$x <- 1L
  restrict(x = cast(type = 1.5), .env = e)
  cat(class(e$x), class(x), sep = ", ")
})
#> numeric, integer
# numeric, integer

# Named objects (lhs) are checked to be in the `.env` environment,
# throwing an error if not found:
local({
  x <- 1L
  e <- new.env()
  restrict(x = cast(type = 1.5), .env = e) |> try()
})
#> Error in eval(quote({ : Caused by error in `restrict()`:
#> ℹ In argument: `x`.
#> ! Object `x` is not found in the `.env` environment specified.
# Error:
# Caused by error in `restrict()`:
# ℹ In argument: `x`.
# ! Object `x` is not found in the `.env` environment specified.

# For expressions (rhs), the `.env` argument is preferentially chosen,
# but if not found then the normal R scoping rules apply:
local({
  x <- 1.5
  e <- new.env()
  e$z <- 1L
  restrict(z = cast(type = x), .env = e) |> try()
  class(e$z)
})
#> [1] "numeric"
# "numeric"

# the `mask` argument within the functions can be used to
# restrict objects within a data mask:
local({
  df <- data.frame(a = 1L:3L, b = c("x", "y", "z"))
  restrict(
    a = cast(type = double(), ~ .x > 0, mask = df),
    b = validate(type = character(), ~ nchar(.x) == 1, mask = df)
  )
  class(df$a)
})
#> [1] "numeric"
# "numeric"

# The `.error_call` argument can be used to specify where the error occurs,
# by default this is the caller environment.
myfunc <- function(x) restrict(x = validate(size = -5))
myfunc(1) |> try()
#> Error in myfunc(1) : Caused by error in `restrict()`:
#> ℹ In argument: `x`.
#> ! Size argument is `-5`, needs to be positive scalar integerish.
# Error in `myfunc()`:
# Caused by error in `restrict()`:
# ℹ In argument: `x`.
# ! Size argument is `-5`, needs to be positive scalar integerish.

# Injection and glue can be used:
local({
  x <- 1L
  x_name <- "x"
  x_list <- list(x = rlang::expr(validate(type = integer())))
  restrict(
    "{x_name}" = validate(type = integer()),
    !!x_name := validate(type = integer()),
    {{ x_name }} := validate(type = integer()),
    !!!x_list
  )
})

local({
  df <- data.frame(x = 1L)
  x_txt <- "my glue error message"
  restrict(x = validate("{x_txt}" = ~ .x != 1, mask = df)) |> try()
})
#> Error in eval(quote({ : Caused by error in `restrict()`:
#> ℹ In argument: `x` for data mask `df`.
#> ! my glue error message
# Error:
# Caused by error in `restrict()`:
# ℹ In argument: `x` for data mask `df`.
# ! my glue error message
```
