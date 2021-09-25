# aRtsy 0.1.2

**New features**

- Added new artwork `canvas_flow()`.
- Added new artwork `canvas_watercolors()`.

**Major changes**

- Changed the `width` and `height` arguments in all functions to `resolution`. If you want to save a non-square artwork, it is best to set a custom `height` and `width` the the preferred export / save function.

**Minor changes**

- Added `Rcpp::checkUserInterrupt()` to all `C++` functions.

# aRtsy 0.1.1

**New features**

- Added six new artworks to the package: `canvas_stripes()`, `canvas_gemstone()`, `canvas_blacklight()`, `canvas_mosaic()`, `canvas_forest()`, and `canvas_nebula()`.

**Minor changes**

- Artwork of the day is now created with `set.seed(Sys.Date())` instead of `set.seed(Sys.time())`.
- Removed unnecessary `ggpubr` from the Imports field in `DESCRIPTION` for CRAN submission.
- Fixed a memory leak in `canvas_ant()`.

# aRtsy 0.1.0

**New features**

- First implementation of the aRtsy package.
