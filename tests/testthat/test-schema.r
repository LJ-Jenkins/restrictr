test_that("schema errors if masked arguments don't exist", {
  x <- 1:5
  y <- 1:5
  z <- 10
  df <- data.frame(x, y, q = z)
  li <- list(x = x, y = y, z)
  expect_error(schema(df, x = , y = 10, z = 10))
  expect_error(schema(li, x = , y = 10, z = 10))
  expect_error(schema_cast(df, x = 10, y = 10, z = 10))
  expect_error(schema_cast(li, x = 10, y = 10, z = 10))
  expect_error(schema_recycle(df, x = 10, y = 10, z = 10))
  expect_error(schema_recycle(li, x = 10, y = 10, z = 10))
})

test_that("schema errors with unnamed args", {
  x <- 1:5
  y <- 1:5
  z <- 10
  df <- data.frame(x = 1:5, y = 1:5, z = 10)
  expect_error(schema(df, x = 10, y = 10, 10))
  expect_error(schema_cast(df, x = 10, y = 10, 10))
  expect_error(schema_recycle(df, x = 10, y = 10, 10))
})

test_that("schema checks", {
  a_msg <- "var_a"
  gc <- "y"
  df <- data.frame(x = 1L:2L, y = c("hi", "bye"), z = c(1.5, 2.5))
  li <- list(x = 1:5, y = 1:50)
  qcall <- quote(x > 0)
  qlcall <- list("Error here" = quote(is.character(x)))
  glue_list <- list(x = TRUE, "a_glue_msg_for_{a_msg}" = FALSE)
  expect_error(schema(df, is.integer(var)))
  expect_no_error(schema(df, is.integer(x), is.character(y), is.double(z)))
  expect_no_error(schema(df, .size = 2, .names = c("x", "{gc}")))
  expect_no_error(schema(df, !is.character(z), .size = 2, .names = c("x", "y")))
  expect_error(schema(df, !is.character(z), .size = 3, .names = c("x", "y")))
  expect_error(schema(df, !is.character(z), .size = 2, .names = c("x", "y", "a")))
  expect_no_error(schema(df, !!qcall))
  expect_error(schema(df, !!!qlcall), regexp = "Error here")
  expect_no_error(schema(li, y > 0, .size = 2))
  expect_no_error(schema(li, length(x) == 5, length(y) == 50, .size = 2))
  err <- expect_error(schema(li, !!!glue_list))
  expect_equal(unname(err$message), "Caused by error in `schema()`:")
  expect_equal(
    err$body,
    c(i = "In argument: `FALSE` for data mask `li`.", "!" = "a_glue_msg_for_var_a")
  )
  li <- list(x = 1L, y = 1L)
  y <- 1.5
  expect_no_error(schema(li, x == y))
  err <- expect_error(
    schema(li, "Not same class as env variable" = x == class(.env$y))
  )
  expect_equal(
    err$body,
    c(i = "In argument: `x == class(.env$y)` for data mask `li`.", "!" = "Not same class as env variable")
  )
})

test_that("schema_cast casts correctly", {
  x <- 1L
  y <- 5L
  z <- 1L
  df <- data.frame(x, y, z)
  df <- schema_cast(df, x = double(), y = double(), z = y)
  expect_identical(class(df$x), "numeric")
  expect_identical(class(df$y), "numeric")
  expect_identical(class(df$z), "numeric")
})

test_that("schema_cast checks", {
  a_msg <- "var_a"
  gc <- "y"
  df <- data.frame(x = 1L:2L, y = c("hi", "bye"), z = 1:2)
  li <- list(x = 1:5, y = 1:50)
  qcall <- quos(x = integer())
  qlcall <- quos("Error here" = integer())
  glue_list <- list(x = integer(), "a_glue_msg_for_{a_msg}" = logical())
  expect_error(schema_cast(df, var = integer()))
  expect_no_error(schema_cast(df, x = integer(), y = character(), z = double()))
  expect_no_error(schema_cast(df, .size = 2, .names = c("x", "{gc}")))
  expect_no_error(schema_cast(df, "{gc}" = character()))
  expect_no_error(schema_cast(df, y = character(), .size = 2, .names = c("x", "y")))
  expect_error(schema_cast(df, y = character(), .size = 3, .names = c("x", "y")))
  expect_error(schema_cast(df, y = character(), .size = 2, .names = c("x", "y", "a")))
  expect_no_error(schema_cast(df, !!!qcall))
  expect_error(schema_cast(df, !!!qlcall), regexp = "Named element `Error here` not found in data mask `df`.")
  expect_no_error(schema_cast(li, y = double(), .size = 2))
  expect_error(
    schema_cast(df, !!!glue_list),
    regexp = "Named element `a_glue_msg_for_var_a` not found in data mask `df`."
  )
  li <- list(x = 1L, y = 1L)
  y <- 1.5
  li <- schema_cast(li, x = y)
  expect_identical(class(li$x), "integer")
  li <- schema_cast(li, x = .env$y)
  expect_identical(class(li$x), "numeric")
})

test_that("schema_recycle errors if size arguments aren't scalar integerish", {
  li <- list(y = 1, z = 10.5)
  expect_error(schema_recycle(li, y = 10.1))
  expect_error(schema_recycle(li, y = z))
})

test_that("schema_recycle recycles correctly", {
  x <- 1
  y <- 5
  z <- 1
  li <- list(x = x, y = y, z = z)
  li <- schema_recycle(li, x = 10, y = 5, z = vctrs::vec_size(x))
  expect_identical(lengths(li), c(x = 10L, y = 5L, z = 10L))
})

test_that("schema_recycle checks", {
  li <- list(x = 1, y = "hi", z = 2)
  y <- 10L
  a_msg <- "var_a"
  gc <- "y"
  qcall <- quos(x = 10)
  qlcall <- list("Error here" = 1)
  glue_list <- list(x = 5, "a_glue_msg_for_{a_msg}" = 3)
  expect_error(schema_recycle(li, var = 1))
  expect_no_error(schema_recycle(li, x = 2, y = 2, z = 2))
  expect_no_error(schema_recycle(li, .size = 3, .names = c("x", "{gc}")))
  expect_no_error(schema_recycle(li, "{gc}" = 5))
  expect_no_error(schema_recycle(li, z = 2, .size = 3, .names = c("x", "y")))
  expect_error(schema_recycle(li, z = 2, .size = 10, .names = c("x", "y")))
  expect_error(schema_recycle(li, z = 2, .size = 3, .names = c("x", "y", "a")))
  expect_no_error(schema_recycle(li, !!!qcall))
  expect_error(schema_recycle(li, !!!qlcall), regexp = "Named element `Error here` not found in data mask `li`.")
  expect_error(
    schema_recycle(li, !!!glue_list),
    regexp = "Named element `a_glue_msg_for_var_a` not found in data mask `li`."
  )
  li <- list(x = 1, y = 1)
  y <- 1:10
  li <- schema_recycle(li, x = vctrs::vec_size(y))
  expect_identical(lengths(li), c(x = 1L, y = 1L))
  li <- schema_recycle(li, x = vctrs::vec_size(.env$y))
  expect_identical(lengths(li), c(x = 10L, y = 1L))
})

test_that("enforce_schema checks", {
  txt <- "wowza"
  li <- list(x = 1, y = "hi")
  li2 <- schema(li, "{txt}" = x == 1)
  li2$x <- 2
  expect_error(enforce_schema(li2), regexp = "wowza")

  li2 <- schema_cast(li, x = integer(), .lossy = TRUE)
  li2$x <- 1.5
  li2 <- enforce_schema(li2)
  expect_identical(class(li2$x), "integer")

  li2 <- schema_cast(li, y = character())
  li2$y <- 5
  expect_error(enforce_schema(li2), regexp = "Can't convert `y` <double> to <character>.")

  li2 <- schema_recycle(li, x = 2, y = 3)
  li2$y <- "hi"
  li2 <- enforce_schema(li2)
  expect_identical(lengths(li2), c(x = 2L, y = 3L))
})
