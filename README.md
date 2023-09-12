  <!-- badges: start -->
   [![Codecov test coverage](https://codecov.io/gh/hughparsonage/fy/branch/master/graph/badge.svg)]( https://app.codecov.io/gh/hughparsonage/fy?branch=master)
  <!-- badges: end -->

# fy
Package for financial years

Examples

```r
library(fy)
is_fy(c("2020-21", "2020-20", "foo", "1999-00", "2002-2003"))
fy2yr("2020-21")
yr2fy(2000:2020)
validate_fys_permitted(c("2020-21", "2020-20")) 
```
