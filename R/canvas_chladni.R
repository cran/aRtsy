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

#' Draw Chladni Figures
#'
#' @description This function draws Chladni figures on a canvas. Named after
#'   Ernst Chladni, an 18th century physicist who first discovered them, Chladni
#'   figures are patterns that arise from the vibrations of a two-dimensional
#'   plate, typically covered with a thin layer of sand or powder. The Chladni
#'   figures are created by varying the frequency of vibration applied to the
#'   plate. In this implementation, the grid underneath the plate can be
#'   transformed using a domain warping technique. The basic idea behind domain
#'   warping is to apply a series of transformations to the input grid to create
#'   a more complex and interesting output.
#'
#' @usage canvas_chladni(
#'   colors,
#'   waves = 5,
#'   warp = 0,
#'   resolution = 500,
#'   angles = NULL,
#'   distances = NULL,
#'   flatten = FALSE
#' )
#'
#' @param colors      a string or character vector specifying the color(s) used
#'   for the artwork.
#' @param waves       a character specifying the number of randomly sampled
#'   waves, or an integer vector of waves to be summed.
#' @param resolution  resolution of the artwork in pixels per row/column.
#'   Increasing the resolution increases the quality of the artwork but also
#'   increases the computation time exponentially.
#' @param warp        a numeric value specifying the maximum warping distance
#'   for each point. If \code{warp = 0} (the default), no warping is performed.
#' @param angles      optional, a resolution x resolution matrix containing the
#'   angles for the warp, or a character indicating the type of noise to use
#'   (\code{svm}, \code{knn}, \code{rf}, \code{perlin}, \code{cubic},
#'   \code{simplex}, or \code{worley}). If \code{NULL} (the default), the noise
#'   type is chosen randomly.
#' @param distances   optional, a resolution x resolution matrix containing the
#'   distances for the warp, or a character indicating the type of noise to use
#'   (\code{svm}, \code{knn}, \code{rf}, \code{perlin}, \code{cubic},
#'   \code{simplex}, or \code{worley}). If \code{NULL} (the default), the noise
#'   type is chosen randomly.
#' @param flatten     logical, should colors be flattened after being assigned
#'   to a point.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @keywords artwork canvas
#'
#' @seealso \code{colorPalette}
#'
#' @examples
#' \donttest{
#' set.seed(2)
#'
#' # Simple example
#' canvas_chladni(colors = colorPalette("origami"))
#'
#' # Advanced example
#' canvas_chladni(
#'   colors = colorPalette("lava"),
#'   waves = c(1, 2, 3, 9),
#'   warp = 1
#' )
#' }
#'
#' @export

canvas_chladni <- function(colors,
                           waves = 5,
                           warp = 0,
                           resolution = 500,
                           angles = NULL,
                           distances = NULL,
                           flatten = FALSE) {
  .checkUserInput(resolution = resolution)
  if (length(waves) == 1) {
    waves <- sample.int(50, size = waves, replace = TRUE)
  }
  x <- seq(0, 0.5 * pi, length.out = resolution)
  y <- seq(0, 0.5 * pi, length.out = resolution)
  canvas <- expand.grid(x, y)
  if (warp > 0) {
    inputCanvas <- .warp(canvas, warp, resolution, angles, distances)
  } else {
    inputCanvas <- as.matrix(canvas)
  }
  full_canvas <- data.frame(x = canvas[, 1], y = canvas[, 2], z = cpp_chladni(x = inputCanvas[, 1], y = inputCanvas[, 2], waves))
  if (flatten) {
    full_canvas[["z"]] <- round(full_canvas[["z"]], 0)
  }
  artwork <- ggplot2::ggplot(data = full_canvas, mapping = ggplot2::aes(x = x, y = y, fill = z)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::scale_fill_gradientn(colours = colors)
  artwork <- theme_canvas(artwork)
  return(artwork)
}
