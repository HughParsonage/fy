context("test-fy-methods")

test_that("methods works", {
  as.fy <- function(x) {class(x) <- "fy"; x}
  expect_equal(as.fy("2015-16") + 2L, "2017-18")
  expect_equal(as.fy("2015-16") + 2, "2017-18")
  expect_equal(as.fy("2015-16") - 3L, "2012-13")
  expect_equal(as.fy("2015-16") - 3, "2012-13")

  x <- as.fy(yr2fy(2014:2016))
  expect_equal(x + 4:6, yr2fy(c(2018, 2020, 2022)))
  expect_equal(x - rep(1, 3), yr2fy(2013:2015))
})

test_that("methods for non-integer doubles", {
  as.fy <- function(x) {class(x) <- "fy"; x}
  expect_warning(zz <- as.fy("2015-16") + 2.5,
                 regexp = "integerish")
  expect_warning(zz <- as.fy("2015-16") - 2.5,
                 regexp = "integerish")
  expect_equal(zz, "2013-14")
  expect_error(as.fy("2015-16") + "2015-16",
               regexp = "non-numeric")
  expect_error(as.fy("2015-16") - "2015-16",
               regexp = "non-numeric")
  expect_error(as.fy(c("2015-16", "2016-17")) + 1:3,
               regexp = "length of")
  expect_error(as.fy(c("2015-16", "2016-17")) - 1:3,
               regexp = "length of")
})

test_that("range/min/max", {
  x <- sample(yr2fy(2005:2010))
  class(x) <- "fy"
  rx <- range(x)
  expect_equal(rx[1], range(x)[1])
  expect_equal(min(x), yr2fy(2005))
  expect_equal(max(x), yr2fy(2010))
})

