test_that("restrict and validate no error when everything aligns", {
  x <- 1L
  y <- c("hello", "bye")
  expect_no_error(
    restrict(
      x = validate(
        size = 1, type = integer(), is.integer,
        \(.x) is.integer(.x), ~ is.integer(.x)
      ),
      y = validate(
        size = 2, type = character(), is.character,
        \(.x) any(grepl("he", .x)), ~ any(grepl("by", .x))
      )
    )
  )
  df <- data.frame(x = 1L:2L, y = c("hello", "bye"))
  expect_no_error(
    restrict(
      x = validate(
        size = 2, type = integer(), is.integer,
        \(.x) is.integer(.x), ~ is.integer(.x),
        mask = df
      ),
      y = validate(
        size = 2, type = character(), is.character,
        \(.x) any(grepl("he", .x)), ~ any(grepl("by", .x)),
        mask = df
      )
    )
  )
})

test_that("restrict and validate errors when incorrect size", {
  x <- rep(1, 5)
  expect_error(
    restrict(
      x = validate(size = 1)
    )
  )
  df <- data.frame(x = 1L:2L)
  expect_error(
    restrict(
      x = validate(size = 10, mask = df)
    )
  )
})

test_that("restrict and validate errors when incorrect type", {
  x <- 1L
  expect_error(
    restrict(
      x = validate(type = character())
    )
  )
  df <- data.frame(x = 1L:2L)
  expect_error(
    restrict(
      x = validate(type = character(), mask = df)
    )
  )
})

test_that("restrict and validate errors when predicate isn't logical", {
  x <- 1L
  expect_error(
    restrict(
      x = validate(100)
    )
  )
  df <- data.frame(x = 1L:2L)
  expect_error(
    restrict(
      x = validate(100, mask = df)
    )
  )
})

test_that("restrict errors when predicate doesn't return (all) TRUE", {
  x <- 1L
  expect_error(
    restrict(
      x = validate(\(.x) .x == 100)
    )
  )
  df <- data.frame(x = c(1, 2, 100))
  expect_error(
    restrict(
      x = validate(\(.x) .x == 100, mask = df)
    )
  )
})

test_that("restrict works sequentially with repeated variables", {
  x <- 1L
  y <- 1L
  err <- expect_error(
    restrict(
      x = cast(type = double()),
      y = validate(type = x)
    )
  )
  expect_equal(unname(err$message), "Caused by error in `restrict()`:")
  expect_equal(
    err$body,
    c(i = "In argument: `y`.", "!" = "Returned <integer>, not <double>.")
  )
  expect_no_error(
    restrict(
      x = recycle(size = 10),
      y = recycle(size = vctrs::vec_size(x))
    )
  )
  expect_identical(length(x), 10L)
  expect_identical(length(y), 10L)
  li <- list(x = 1L, y = 1L)
  err <- expect_error(
    restrict(
      x = cast(type = double(), mask = li),
      y = validate(type = x, mask = li)
    )
  )
  expect_equal(unname(err$message), "Caused by error in `restrict()`:")
  expect_equal(
    err$body,
    c(i = "In argument: `y` for data mask `li`.", "!" = "Returned <integer>, not <double>.")
  )
  expect_no_error(
    restrict(
      x = recycle(size = 5, mask = li),
      y = recycle(size = vctrs::vec_size(x), mask = li)
    )
  )
  expect_identical(lengths(li), c(x = 5L, y = 5L))
  li <- list(x = 1.5, y = 1L)
  expect_no_error(
    restrict(
      y = coerce(type = .data$x, size = vctrs::vec_size(.env$x), mask = li)
    )
  )
  expect_identical(class(li$y), "numeric")
  expect_identical(lengths(li), c(x = 1L, y = 10L))
})
