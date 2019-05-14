context("test-yr2fy")

test_that("yr2fy works", {
  expect_equal(yr2fy(2016), "2015-16")
  expect_equal(yr2fy(2016:2017), c("2015-16", "2016-17"))
})

test_that("fy2yr works", {
  expect_error(fy2yr(c("2014-15", "2015-15")),
               regexp = "contains non-FYs",
               fixed = TRUE)
  expect_identical(fy2yr("2014-15"), 2015L)
})
