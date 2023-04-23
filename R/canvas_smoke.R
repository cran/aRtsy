# Copyright (C) 2021-2023 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Draw Rainbow Smoke
#'
#' @description This function implements the rainbow smoke algorithm.
#'
#' @usage canvas_smoke(colors, init = 1, shape = c("bursts", "clouds"),
#'              algorithm = c("minimum", "average"), resolution = 150)
#'
#' @param colors      a string or character vector specifying the color(s) used for the artwork.
#' @param init        an integer larger than zero and lower  than or equal to \code{resolution^2} specifying the initial number of pixels to color on the canvas.
#' @param shape       a character indicating the shape of the smoke. Possible options are \code{burst} and \code{clouds}.
#' @param resolution  resolution of the artwork in pixels per row/column. Increasing the resolution increases the quality of the artwork but also increases the computation time exponentially.
#' @param algorithm   a character specifying how to select a new pixel. The default option \code{minimum} selects the pixel with the smallest color difference in a single neighbor and is relatively fast. The option \code{average} selects the pixel with the smallest average color difference in all the neighbors and is relatively slow.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @references \url{http://rainbowsmoke.hu}
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @keywords artwork canvas
#'
#' @seealso \code{colorPalette}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#'
#' # Simple example
#' canvas_smoke(colors = "all", resolution = 500)
#'
#' # Advanced example
#' reds <- colorRampPalette(c("red", "black"))
#' blues <- colorRampPalette(c("goldenrod", "navyblue"))
#' palette <- c(reds(100), blues(100))
#' canvas_smoke(colors = palette, init = 3, shape = "clouds", resolution = 500)
#' }
#'
#' @export
canvas_smoke <- function(colors, init = 1, shape = c("bursts", "clouds"),
                         algorithm = c("minimum", "average"), resolution = 150) {
  .checkUserInput(resolution = resolution)
  stopifnot("'init' must be > 0 and <= resolution^2" = init > 0 && init <= resolution^2)
  shape <- match.arg(shape)
  algorithm <- match.arg(algorithm)
  all_colors <- length(colors) == 1 && colors[1] == "all"
  color_mat <- .getColorMat(colors, all_colors)
  shape <- switch(shape,
    "bursts" = 0,
    "clouds" = 1
  )
  algorithm <- switch(algorithm,
    "minimum" = 0,
    "average" = 1
  )
  canvas <- array(c(rep(-1, 3 * resolution^2), rep(0, 2 * resolution^2)), c(resolution, resolution, 5))
  coords <- as.matrix(expand.grid(0:(resolution - 1), 0:(resolution - 1)))
  canvas <- draw_smoke(canvas, coords, color_mat, init, algorithm, shape, all_colors)
  full_canvas <- as.data.frame(expand.grid(x = 1:resolution, y = 1:resolution))
  full_canvas[["col"]] <- grDevices::rgb(red = canvas[, , 1], green = canvas[, , 2], blue = canvas[, , 3], maxColorValue = 255)
  artwork <- ggplot2::ggplot(data = full_canvas, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_raster(interpolate = TRUE, fill = full_canvas[["col"]])
  artwork <- theme_canvas(artwork)
  return(artwork)
}

.getColorMat <- function(colors, all_colors) {
  if (all_colors) {
    return(matrix(NA, 1, 1))
  } else {
    return(t(as.matrix(grDevices::col2rgb(colors))))
  }
}
