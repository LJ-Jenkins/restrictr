# base R error

    Code
      x <- 1
      li <- list(x = 1)
      tf <- (function() abort_if_not(1 + ""))
      tf()
    Condition
      Error in `tf()`:
      Caused by error in `abort_if_not()`:
      i In argument: `1 + ""`.
      ! non-numeric argument to binary operator
    Code
      tf <- (function(x) cast_if_not(x = 1 + ""))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `cast_if_not()`:
      i In argument: `x = 1 + ""`.
      ! non-numeric argument to binary operator
    Code
      tf <- (function(x) recycle_if_not(x = 1 + ""))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `recycle_if_not()`:
      i In argument: `x = 1 + ""`.
      ! non-numeric argument to binary operator
    Code
      tf <- (function(li) schema(li, 1 + ""))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `schema()`:
      i In argument: `1 + ""` for data mask `li`.
      ! non-numeric argument to binary operator
    Code
      tf <- (function(li) schema_cast(li, x = 1 + ""))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `schema_cast()`:
      i In argument: `x = 1 + ""` for data mask `li`.
      ! non-numeric argument to binary operator
    Code
      tf <- (function(li) schema_recycle(li, x = 1 + ""))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `schema_recycle()`:
      i In argument: `x = 1 + ""` for data mask `li`.
      ! non-numeric argument to binary operator
    Code
      tf <- (function(x) restrict(x = validate(type = 1 + "")))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x`.
      ! non-numeric argument to binary operator
    Code
      tf <- (function(li) restrict(x = validate(type = 1 + "", mask = li)))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x` for data mask `li`.
      ! non-numeric argument to binary operator

# vctrs cast error

    Code
      x <- 1.5
      li <- list(x = 1.5)
      tf <- (function(x) cast_if_not(x = integer()))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `cast_if_not()`:
      i In argument: `x = integer()`.
      ! Can't convert from `x` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      tf <- (function(li) schema_cast(li, x = integer()))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `schema_cast()`:
      i In argument: `x = integer()` for data mask `li`.
      ! Can't convert from `x` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      tf <- (function(x) restrict(x = cast(type = integer())))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x`.
      ! Can't convert from `x` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      tf <- (function(li) restrict(x = cast(type = integer(), mask = li)))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x` for data mask `li`.
      ! Can't convert from `x` <double> to <integer> due to loss of precision.
      * Locations: 1

# vctrs recycle error

    Code
      x <- 1:5
      li <- list(x = 1:5)
      tf <- (function(x) recycle_if_not(x = 10))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `recycle_if_not()`:
      i In argument: `x = 10`.
      ! Can't recycle `x` (size 5) to size 10.
    Code
      tf <- (function(li) schema_recycle(li, x = 10))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `schema_recycle()`:
      i In argument: `x = 10` for data mask `li`.
      ! Can't recycle `x` (size 5) to size 10.
    Code
      tf <- (function(x) restrict(x = recycle(size = 10)))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x`.
      ! Can't recycle `x` (size 5) to size 10.
    Code
      tf <- (function(li) restrict(x = recycle(size = 10, mask = li)))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x` for data mask `li`.
      ! Can't recycle `x` (size 5) to size 10.

# vctrs not vector error

    Code
      x <- mean
      li <- list(x = mean)
      tf <- (function(x) cast_if_not(x = 10))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `cast_if_not()`:
      i In argument: `x = 10`.
      ! `x` must be a vector, not a function.
    Code
      tf <- (function(x) recycle_if_not(x = 10))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `recycle_if_not()`:
      i In argument: `x = 10`.
      ! `x` must be a vector, not a function.
    Code
      tf <- (function(li) schema_cast(li, x = 10))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `schema_cast()`:
      i In argument: `x = 10` for data mask `li`.
      ! `x` must be a vector, not a function.
    Code
      tf <- (function(li) schema_recycle(li, x = 10))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `schema_recycle()`:
      i In argument: `x = 10` for data mask `li`.
      ! `x` must be a vector, not a function.
    Code
      tf <- (function(x) restrict(x = cast(type = 10)))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x`.
      ! `x` must be a vector, not a function.
    Code
      tf <- (function(x) restrict(x = recycle(size = 10)))
      tf(x)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x`.
      ! `x` must be a vector, not a function.
    Code
      tf <- (function(li) restrict(x = cast(type = 10, mask = li)))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x` for data mask `li`.
      ! `x` must be a vector, not a function.
    Code
      tf <- (function(li) restrict(x = recycle(size = 10, mask = li)))
      tf(li)
    Condition
      Error in `tf()`:
      Caused by error in `restrict()`:
      i In argument: `x` for data mask `li`.
      ! `x` must be a vector, not a function.

