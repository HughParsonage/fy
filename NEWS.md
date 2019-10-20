# fy 0.2.0

* Added a `NEWS.md` file to track changes to the package.

Compatibility issues:
  * Ditch package grattan's error message 'fy.yr contains non-FYs' since it referred
    to the wrong function argument and was not specific.
  * `na_error` argument now becomes `validate`.

New features:
  * `fy` now supports some dashes in the financial years.
