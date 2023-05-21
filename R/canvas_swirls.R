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

#' Draw Swirls
#'
#' @description This function draws swirling stripes on a canvas by simulating a
#'   particle system.
#'
#' @usage canvas_swirls(
#'   colors,
#'   background = "#fafafa",
#'   iterations = 250,
#'   n = 250,
#'   curvature = 0.005,
#'   lwd = 0.1,
#'   resolution = 500
#' )
#'
#' @param colors      a character (vector) specifying the color(s) used for the
#'   artwork.
#' @param background  a character specifying the color used for the background.
#' @param iterations  a positive integer specifying the number of iterations of
#'   the algorithm.
#' @param n           a positive integer specifying the number of particles.
#' @param curvature   a positive number specifying the curvature of the lines.
#'   Larger values imply relatively curved lines, while lower values produce
#'   relatively straight lines.
#' @param lwd         expansion factor for the line width.
#' @param resolution  resolution of the artwork in pixels per row/column.
#'   Increasing the resolution increases the quality of the artwork but also
#'   increases the computation time exponentially.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @references \url{https://mattdesl.svbtle.com/generative-art-with-nodejs-and-canvas}
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
#' canvas_swirls(colors = colorPalette("origami"))
#' }
#'
#' @export

canvas_swirls <- function(colors, background = "#fafafa", iterations = 250,
                          n = 250, curvature = 0.005, lwd = 0.1, resolution = 500) {
  .checkUserInput(
    resolution = resolution, background = background, iterations = iterations
  )
  stopifnot("'curvature' must be a single value > 0 and <= 0.01" = curvature > 0 && curvature <= 0.01)
  heightMap <- .noise(dims = c(resolution, resolution), type = "perlin", limits = c(0, 255))
  palette <- c(background, colors)
  canvas <- cpp_swirls(heightMap, iterations, n, resolution, length(palette), lwd, curvature)
  canvas <- as.data.frame(canvas)
  colnames(canvas) <- c("x", "y", "xend", "yend", "color", "z", "width")
  canvas <- canvas[((canvas[["xend"]] > 0 & canvas[["xend"]] < resolution) & (canvas[["yend"]] > 0 & canvas[["yend"]] < resolution)), ]
  palette <- palette[canvas[["color"]]]
  artwork <- ggplot2::ggplot(canvas) +
    ggplot2::geom_segment(mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend, group = z), linewidth = canvas[["width"]], color = palette, lineend = "square") +
    ggplot2::scale_x_continuous(limits = c(0, resolution)) +
    ggplot2::scale_y_continuous(limits = c(0, resolution))
  artwork <- aRtsy::theme_canvas(artwork, background = background)
  return(artwork)
}
