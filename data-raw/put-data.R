library(fastmatch)
fys1901 <- grattan::yr2fy(1901:2100)
"1999-00" %fin% fys1901

usethis::use_data(fys1901, internal = TRUE, overwrite = TRUE)
