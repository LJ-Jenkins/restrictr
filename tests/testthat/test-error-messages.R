test_that("base R error", {
  expect_snapshot(
    error = TRUE,
    {
      x <- 1
      li <- list(x = 1)
      tf <- \() abort_if_not(1 + "")
      tf()
      tf <- \(x) cast_if_not(x = 1 + "")
      tf(x)
      tf <- \(x) recycle_if_not(x = 1 + "")
      tf(x)
      tf <- \(li) schema(li, 1 + "")
      tf(li)
      tf <- \(li) schema_cast(li, x = 1 + "")
      tf(li)
      tf <- \(li) schema_recycle(li, x = 1 + "")
      tf(li)
      tf <- \(x) restrict(x = validate(type = 1 + ""))
      tf(x)
      tf <- \(li) restrict(x = validate(type = 1 + "", mask = li))
      tf(li)
    }
  )
})

test_that("vctrs cast error", {
  expect_snapshot(
    error = TRUE,
    {
      x <- 1.5
      li <- list(x = 1.5)
      tf <- \(x) cast_if_not(x = integer())
      tf(x)
      tf <- \(li) schema_cast(li, x = integer())
      tf(li)
      tf <- \(x) restrict(x = cast(type = integer()))
      tf(x)
      tf <- \(li) restrict(x = cast(type = integer(), mask = li))
      tf(li)
    }
  )
})

test_that("vctrs recycle error", {
  expect_snapshot(
    error = TRUE,
    {
      x <- 1:5
      li <- list(x = 1:5)
      tf <- \(x) recycle_if_not(x = 10)
      tf(x)
      tf <- \(li) schema_recycle(li, x = 10)
      tf(li)
      tf <- \(x) restrict(x = recycle(size = 10))
      tf(x)
      tf <- \(li) restrict(x = recycle(size = 10, mask = li))
      tf(li)
    }
  )
})

test_that("vctrs not vector error", {
  expect_snapshot(
    error = TRUE,
    {
      x <- mean
      li <- list(x = mean)
      tf <- \(x) cast_if_not(x = 10)
      tf(x)
      tf <- \(x) recycle_if_not(x = 10)
      tf(x)
      tf <- \(li) schema_cast(li, x = 10)
      tf(li)
      tf <- \(li) schema_recycle(li, x = 10)
      tf(li)
      tf <- \(x) restrict(x = cast(type = 10))
      tf(x)
      tf <- \(x) restrict(x = recycle(size = 10))
      tf(x)
      tf <- \(li) restrict(x = cast(type = 10, mask = li))
      tf(li)
      tf <- \(li) restrict(x = recycle(size = 10, mask = li))
      tf(li)
    }
  )
})
