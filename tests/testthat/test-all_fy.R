context("test-all_fy")

test_that("all_fy works", {
  expect_true(all_fy(c("2015-16", "2015-16", "2016-17")))
  expect_false(all_fy(c("2014-16", "2015-16", "2016-17")))
})
