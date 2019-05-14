context("test-is_fy")

test_that("is_fy works", {
  expect_true(is_fy("2014 15"))
  expect_false(is_fy("2014 14"))
  expect_true(all(is_fy(c("2014-15", "2015-16", "2015-15")) == c(TRUE, TRUE, FALSE)))
})
