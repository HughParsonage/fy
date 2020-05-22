library(fastmatch)
fys1901 <- fy::yr2fy(1901:2100)
fys1901B <- sub("-", "", fys1901, fixed = TRUE)
fys1901C <- sub("-", " ", fys1901, fixed = TRUE)
fys1901_2012 <- sub("-", "\u2012", fys1901, fixed = TRUE)
fys1901_2011 <- sub("-", "\u2011", fys1901, fixed = TRUE)
fys1901_2013 <- sub("-", "\u2013", fys1901, fixed = TRUE)
fys1901_2014 <- sub("-", "\u2014", fys1901, fixed = TRUE)

fys1901_9 <- sprintf("%d-%d", 1900:2099, 1901:2100)
fys1901B_9 <- sub("-", "", fys1901_9, fixed = TRUE)
fys1901C_9 <- sub("-", " ", fys1901_9, fixed = TRUE)
fys1901_2012_9 <- sub("-", "\u2012", fys1901_9, fixed = TRUE)
fys1901_2011_9 <- sub("-", "\u2011", fys1901_9, fixed = TRUE)
fys1901_2013_9 <- sub("-", "\u2013", fys1901_9, fixed = TRUE)
fys1901_2014_9 <- sub("-", "\u2014", fys1901_9, fixed = TRUE)


# Generate match hash
"1999-00" %fin% fys1901
"1999-00" %fin% fys1901B
"1999-00" %fin% fys1901C
"1999-00" %fin% fys1901_2012
"1999-00" %fin% fys1901_2011
"1999-00" %fin% fys1901_2013
"1999-00" %fin% fys1901_2014

"1999-2000" %fin% fys1901_9
"1999-2000" %fin% fys1901B_9
"1999-2000" %fin% fys1901C_9
"1999-2000" %fin% fys1901_2012_9
"1999-2000" %fin% fys1901_2011_9
"1999-2000" %fin% fys1901_2013_9
"1999-2000" %fin% fys1901_2014_9

fys1901_all <- c(fys1901, fys1901B, fys1901C)
"1999-00" %fin% fys1901_all


usethis::use_data(fys1901,
                  fys1901B,
                  fys1901C,
                  fys1901_2011,
                  fys1901_2012,
                  fys1901_2013,
                  fys1901_2014,

                  fys1901_9,
                  fys1901B_9,
                  fys1901C_9,
                  fys1901_2011_9,
                  fys1901_2012_9,
                  fys1901_2013_9,
                  fys1901_2014_9,

                  fys1901_all,
                  internal = TRUE, overwrite = TRUE)
