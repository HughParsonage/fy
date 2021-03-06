context("validate_fys_permitted")

test_that("Error handling", {
  x <- raw(2)
  expect_error(validate_fys_permitted(x),
               regexp = "was type raw")
  x <- c("2013-13")
  expect_error(validate_fys_permitted(x, permitted_fys = c("2014-15", "2015-16")),
               regexp = "`x` set to '2013-13', was not a valid financial year.",
               fixed = TRUE)
  expect_error(validate_fys_permitted(c("2015-16", "2015-17", "2010-9"), c("2015-16", "2016-17")),
               regexp = "contained invalid FYs.",
               fixed = TRUE)
  zzz <- c("2015-16", "2015-17")
  expect_error(validate_fys_permitted(zzz, c("2015-16", "2016-17")),
               regexp = "`zzz` contained invalid entry 2015-17 at position 2.",
               fixed = TRUE)
  zzz <- c("2015-16", "2016-17", "2017-18")
  yyy <- c("2015-16", "2016-17")
  expect_error(validate_fys_permitted(zzz, yyy),
               regexp = "`zzz = 2017-18` was not within the allowed range: 2015-16 <= fy.year <= 2016-17",
               fixed = TRUE)
  expect_error(validate_fys_permitted(c("2015-16", "2016-17", "2017-18", "2018-19"),
                                    c("2015-16", "2016-17")),
               regexp = "were not within the allowed range: 2015-16 <= fy.year <= 2016-17",
               fixed = TRUE)
})

test_that("min or max years", {
  expect_error(validate_fys_permitted("1980-81", min.yr = 1982L))
  expect_error(validate_fys_permitted("1980-81", max.yr = 1979L))
  expect_equal(validate_fys_permitted("1980-81", max.yr = 1982L), "1980-81", check.attributes = FALSE)
  expect_equal(validate_fys_permitted("1984-85", min.yr = 1982L, max.yr = 1989L), "1984-85", check.attributes = FALSE)
  expect_error(validate_fys_permitted(c("1980-81", "1980-80"), min.yr = 1980L),
               regexp = 'contained "1980-80" which is not a valid financial year.')
  expect_error(validate_fys_permitted(c("2014-15", "201516", "2015-16"), min.yr = 2016L, deparsed = "x"),
               regexp = "earliest permitted financial year")
})

test_that("validation of other types", {
  expect_equal(validate_fys_permitted("1980 81"), "1980-81")
  expect_equal(validate_fys_permitted("198081"), "1980-81")
  v <- c("2015-16", "2015 16", "201516", "201516", "2003-04", "2004 05")
  a <- c("2015-16", "2015-16", "2015-16", "2015-16", "2003-04", "2004-05")
  expect_equal(validate_fys_permitted(v), a)
})

test_that("validate permitted years", {
  v <- c("2015-16", "2015 16", "201516", "201516", "2003-04", "2004 05")
  expect_true(inherits(o <- validate_fys_permitted(v, permitted_fys = yr2fy(1950:2050)),
                       "fy"))
  expect_equal(min(o), "2003-04")
})


test_that("Validation memoization", {
  skip_on_cran()
  y <- c("2014-15", "2015-16")
  x <- validate_fys_permitted(y)
  expect_equal(x, y, check.attributes = FALSE)
  x1 <- validate_fys_permitted(x)
  expect_equal(x, x1, check.attributes = FALSE)
  x2 <- validate_fys_permitted(x, min.yr = 2000L)
  expect_equal(x2, x)
  setattr(x2, "fy_min_yr", NULL)
  x3 <- validate_fys_permitted(x2, min.yr = 2000L)
  expect_equal(x, x3, check.attributes = FALSE)
  setattr(x2, "fy_max_yr", NULL)
  x4 <- validate_fys_permitted(x2, min.yr = 2000L, max.yr = 2020L)
  expect_equal(x, x4, check.attributes = FALSE)

  expect_error(validate_fys_permitted(x2, min.yr = 2020L),
               regexp = '`x2` contained "2014-15" which is earlier than the earliest permitted',
               fixed = TRUE)
  expect_error(validate_fys_permitted(x2, max.yr = 2010L),
               regexp = '`x2` contained "2015-16" which is later than the latest permitte',
               fixed = TRUE)

})

test_that("Validation on UTF-8 endashed fys", {
  x <- c("2014-15", "2015-16", paste0("2014", intToUtf8(8210L), "15"))
  y <- validate_fys_permitted(x)
  expect_true(inherits(y, "fy"))
})

test_that("Validation on ATO endashed fys", {
  # Just cells C3 and C4 in Indiviudals_table1_2015-16.xlsx from
  # the ATO's taxstats collection, should be 1978-79
  skip_if_not(file.exists(taxed.rds <- system.file("extdata/taxstats-tbl1-C3C4.rds", package = "fy")))
  taxstats_dash <- readRDS(taxed.rds)
  x197879 <- names(taxstats_dash)[1]
  v197879 <- validate_fys_permitted(x197879)
  expect_true(inherits(v197879, "fy"))
  expect_equal(fy2yr(x197879), 1979L)
  expect_equal(fy2yr(v197879), 1979L)
  expect_equal(fy2date(x197879), as.Date("1979-06-30"))
  expect_equal(fy2date(v197879), as.Date("1979-06-30"))
})

test_that("Validation fmatches attribute", {
  i <- validate_fys_permitted(c("2014-15", "2017-18"), .retain_fmatches = TRUE)
  expect_identical(attr(i, "fy_fmatches"), c(115L, 118L))
})

test_that("validation on already validated ranges", {
  x <- yr2fy(1995:1999)
  x <- validate_fys_permitted(x, min.yr = 1995L, max.yr = 1999L)
  expect_equal(fy2yr(min(x)), 1995L)
  expect_true(inherits(validate_fys_permitted(x, min.yr = 1994L, max.yr = 2000L), "fy"))
  expect_error(validate_fys_permitted(x, min.yr = 1996L),
               regexp = 'earlier than the earliest permitted financial year: "1995-96"',
               fixed = TRUE)
  expect_error(validate_fys_permitted(x, max.yr = 1996L),
               regexp = 'later than the latest permitted financial year: "1995-96"',
               fixed = TRUE)
  attr(x, "fy_min_yr") <- NULL
  expect_error(validate_fys_permitted(x, min.yr = 1996L),
               regexp = 'earlier than the earliest permitted financial year: "1995-96"',
               fixed = TRUE)
  expect_error(validate_fys_permitted(x, max.yr = 1996L),
               regexp = 'later than the latest permitted financial year: "1995-96"',
               fixed = TRUE)
  attr(x, "fy_max_yr") <- NULL
  expect_error(validate_fys_permitted(x, min.yr = 1996L),
               regexp = 'earlier than the earliest permitted financial year: "1995-96"',
               fixed = TRUE)
  expect_error(validate_fys_permitted(x, max.yr = 1996L),
               regexp = 'later than the latest permitted financial year: "1995-96"',
               fixed = TRUE)
})









