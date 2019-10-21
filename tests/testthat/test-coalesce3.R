test_that("coalesce3 does coalesce", {
  x1 <- c(1L, 1L, NA)
  x2 <- c(1L, NA, 1L)
  x3 <- c(NA, 1L, 1L)
  expect_equal(coalesce3i(x1, x2, x3), c(1L, 1L, 1L))
})

test_that("coalesce3 evaluates lazily", {
  expect_equal(coalesce3i(c(1L, NA), c(1L, 1L), stop("this won't happen")),
               c(1L, 1L))
  expect_equal(coalesce3i(0L), 0L)
  expect_equal(coalesce3i(c(0L, NA)), c(0L, NA))
})

test_that("coalesce3 correctly gives attributes", {
  o <- coalesce3i(c(1L, 1L, NA), c(1L, 1L, NA), integer(3))
  expect_false(anyNA3i(o))
  x <- coalesce3i(c(1L, 1L, NA), c(1L, 1L, NA))
  expect_true(anyNA3i(x))
  y <- coalesce3i(c(1L, 1L, NA), c(1L, 1L, NA))
  expect_true(anyNA3i(y))
  y <- coalesce3i(c(1L, 1L, NA), c(1L, 1L, NA), c(NA, NA, NA_integer_))
  expect_true(anyNA3i(y))
})



