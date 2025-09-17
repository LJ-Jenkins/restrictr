test_that("abort_if_not no error for vector usage that evaluates to TRUE", {
  expect_no_error(abort_if_not(TRUE))
  expect_no_error(abort_if_not(c(TRUE, TRUE)))
  x <- 1:10
  expect_no_error(abort_if_not(is.integer(x)))
  expect_no_error(abort_if_not(is.integer(x), is.numeric(x)))
  expect_no_error(abort_if_not(quote(TRUE)))
})

test_that("abort_if_not error for vector usage that evaluates to FALSE", {
  expect_error(abort_if_not(FALSE))
  expect_error(abort_if_not(c(TRUE, FALSE)))
  x <- 1:10
  expect_error(abort_if_not(!is.integer(x)))
  expect_error(abort_if_not(is.integer(x), !is.numeric(x)))
  expect_error(abort_if_not(!is.integer(x), is.numeric(x)))
  expect_error(abort_if_not(quote(FALSE)))
})

test_that("abort_if_not error when arg isn't logical", {
  expect_error(abort_if_not(10))
  expect_error(abort_if_not(NA))
  expect_error(abort_if_not(NULL))
  expect_error(abort_if_not(list(TRUE)))
  expect_error(abort_if_not(data.frame(x = TRUE)))
  expect_error(abort_if_not(\(x) TRUE))
})

test_that("abort_if_not works with injection", {
  a_var <- TRUE
  a_msg <- "var_a"
  a_var_name <- "a"
  a_var_list <- list(a = TRUE, b = TRUE)
  a_var_list2 <- list(a = TRUE, b = TRUE, c = FALSE)
  glue_list <- list(a = TRUE, "a_glue_msg_for_{a_msg}" = FALSE)
  expect_no_error(abort_if_not(!!a_var))
  expect_no_error(abort_if_not(!!!a_var_list))
  expect_error(abort_if_not(!!!a_var_list2))
  expect_no_error(abort_if_not(!!a_var_name := !!a_var))
  expect_no_error(abort_if_not({{ a_var }}))
  expect_no_error(abort_if_not({{ a_var_name }} := {{ a_var }}))
  expect_no_error(abort_if_not({{ a_var_name }} := !!a_var))
  expect_error(
    abort_if_not("a_glue_msg_for_{a_msg}" = FALSE),
    regexp = "a_glue_msg_for_var_a"
  )
  expect_error(
    abort_if_not(!!!glue_list),
    regexp = "a_glue_msg_for_var_a"
  )
})

test_that("abort_if checks", {
  x <- 1:10
  a_msg <- "var_a"
  a_var <- TRUE
  a_var_name <- "a"
  a_var_list <- list(a = FALSE, b = FALSE)
  a_var_list2 <- list(a = FALSE, b = FALSE, c = TRUE)
  glue_list <- list(a = FALSE, "a_glue_msg_for_{a_msg}" = TRUE)
  expect_error(abort_if(TRUE))
  expect_error(abort_if_not(c(FALSE, TRUE)))
  expect_error(abort_if(is.integer(x)))
  expect_no_error(abort_if(is.character(x)))
  expect_no_error(abort_if(quote(FALSE)))
  expect_error(abort_if({{ a_var_name }} := !!a_var))
  expect_no_error(abort_if(!!!a_var_list))
  expect_error(abort_if(!!!a_var_list2, regexp = "c"))
  expect_error(
    abort_if(!!!glue_list),
    regexp = "a_glue_msg_for_var_a"
  )
})
