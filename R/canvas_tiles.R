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

#' Draw Portuguese Tiles
#'
#' @description This function uses a reaction diffusion algorithm in an attempt to draw a Portuguese-styled tiling pattern.
#'
#' @usage canvas_tiles(
#'   colors,
#'   background = "#ffffff",
#'   iterations = 1000,
#'   size = 5,
#'   col.line = "#000000",
#'   resolution = 100,
#'   layout = NULL
#' )
#'
#' @param colors      a string or character vector specifying the color(s) used for the artwork, or a list containing a set of colors for each unique tile on the wall.
#' @param background  a character specifying the color of the background.
#' @param size        a positive integer specifying how many tiles should be in each row of the wall.
#' @param col.line    a character specifying the color of the joints between the tiles.
#' @param iterations  a positive integer specifying the number of iterations of the algorithm.
#' @param resolution  resolution of the artwork in pixels per row/column. Increasing the resolution increases the quality of the artwork but also increases the computation time exponentially.
#' @param layout      optional. A matrix containing integers ranging from 1 to the maximum number of unique tiles (i.e., \code{length(colors)}) specifying the placement of the tiles on the wall.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @references \url{https://en.wikipedia.org/wiki/Reaction–diffusion_system}
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @keywords artwork canvas
#'
#' @seealso \code{colorPalette}
#'
#' @examples
#' \donttest{
#' set.seed(3)
#'
#' # Simple example
#' canvas_tiles(colors = colorPalette("azul"), iterations = 5000)
#'
#' # Advanced example
#' canvas_tiles(colors = list(
#'   colorPalette("blossom"),
#'   colorPalette("neon1"),
#'   colorPalette("dark1")
#' ))
#'
#' # Custom layout
#' layout <- matrix(c(
#'   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'   1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1,
#'   1, 1, 2, 2, 2, 1, 2, 2, 2, 1, 1,
#'   1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1,
#'   1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1,
#'   1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1,
#'   1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1,
#'   1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1,
#'   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#' ), nrow = 11, byrow = TRUE)
#' canvas_tiles(
#'   colors = list(colorPalette("azul"), colorPalette("blossom")),
#'   size = nrow(layout), layout = layout
#' )
#'
#' # Another custom layout
#' set.seed(11)
#' layout <- matrix(c(
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 1, 2, 2, 3, 3, 2, 2, 4, 4, 4, 2, 5, 5, 5, 2, 6, 2, 6, 2,
#'   2, 1, 2, 1, 2, 3, 2, 3, 2, 2, 4, 2, 2, 5, 2, 2, 2, 6, 2, 6, 2,
#'   2, 1, 1, 1, 2, 3, 3, 2, 2, 2, 4, 2, 2, 2, 5, 2, 2, 2, 6, 2, 2,
#'   2, 1, 2, 1, 2, 3, 2, 3, 2, 2, 4, 2, 5, 5, 5, 2, 2, 2, 6, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
#' ), nrow = 21, byrow = TRUE)
#' canvas_tiles(
#'   colors = list(
#'     colorPalette("blossom"),
#'     colorPalette("azul"),
#'     colorPalette("neon1"),
#'     colorPalette("mixer4"),
#'     colorPalette("neon2"),
#'     colorPalette("vrolik1"),
#'     colorPalette("blackwhite")
#'   ),
#'   iterations = 2000,
#'   size = nrow(layout), layout = layout
#' )
#' }
#'
#' @export

canvas_tiles <- function(colors,
                         background = "#ffffff",
                         iterations = 1000,
                         size = 5,
                         col.line = "#000000",
                         resolution = 100,
                         layout = NULL) {
  .checkUserInput(
    resolution = resolution, background = background, iterations = iterations
  )
  stopifnot("'size' must be > 0" = size > 0)
  rate_a <- 1
  rate_b <- 0.5
  duplicates <- size - 1
  cex.line <- 7 / duplicates
  ones <- matrix(1, resolution, resolution)
  conv_mat <- matrix(c(
    0.05, 0.2, 0.05,
    0.2, -1, 0.2,
    0.05, 0.2, 0.05
  ), nrow = 3)
  if (is.list(colors)) {
    ntiles <- length(colors)
    colorvalues <- unlist(mapply(c, background, colors, USE.NAMES = FALSE))
  } else {
    ntiles <- 1
    colorvalues <- c(background, colors)
  }
  tiles <- list()
  for (i in 1:ntiles) {
    type <- sample.int(4, size = 1)
    feed_rate <- switch(type,
      "1" = 0.0545,
      "2" = 0.055,
      "3" = 0.029,
      "4" = 0.03
    )
    kill_rate <- switch(type,
      "1" = 0.062,
      "2" = 0.062,
      "3" = 0.057,
      "4" = 0.06
    )
    binary <- matrix(sample(c(0, 1), size = resolution * resolution, replace = TRUE, prob = c(0.99, 0.01)), ncol = resolution)
    tile <- array(c(ones, binary), dim = c(resolution, resolution, 2))
    tile <- cpp_tiles(tile, conv_mat, rate_a, rate_b, feed_rate, kill_rate, iterations)[, , 2]
    tile <- cbind(tile, tile[, rev(seq_len(ncol(tile)))])
    tile <- rbind(tile, tile[rev(seq_len(nrow(tile))), ])
    if (ntiles > 1) {
      tile <- scales::rescale(x = tile, from = range(tile), to = c(0, lengths(colors)[i]))
    }
    if (i > 1) {
      tile <- tile + i + sum(lengths(colors)[seq_len(i - 1)])
    }
    tiles[[i]] <- tile
  }
  if (size == 1) {
    canvas <- tiles[[1]]
  } else {
    if (is.null(layout)) {
      suppressWarnings({
        layout_matrix <- matrix(seq_len(ntiles), nrow = size, ncol = size)
      })
    } else {
      stopifnot("'layout' must be a matrix" = is.matrix(layout))
      stopifnot("'layout' must be a `size` x `size` matrix" = nrow(layout) == size && ncol(layout) == size)
      stopifnot("'layout' must contain integers from 1 to the number of tiles" = all(layout %% 1 == 0) && max(layout) <= ntiles && min(layout) == 1)
      layout_matrix <- layout
      layout_matrix <- layout_matrix[rev(seq_len(nrow(layout_matrix))), ]
    }
    for (i in 1:size) {
      row <- tiles[[layout_matrix[1, i]]]
      for (j in 2:size) {
        row <- cbind(row, tiles[[layout_matrix[j, i]]])
      }
      if (i == 1) {
        canvas <- row
      } else {
        canvas <- rbind(canvas, row)
      }
    }
  }
  rownames(canvas) <- colnames(canvas) <- seq_len(nrow(canvas))
  full_canvas <- .unraster(canvas, names = c("x", "y", "z"))
  if (ntiles > 1) {
    breaks <- NULL
    for (i in 1:ntiles) {
      breaks <- c(breaks, seq(floor(min(tiles[[i]])), max(tiles[[i]]), length.out = lengths(colors)[i] + 1))
    }
  } else {
    breaks <- seq(0, max(full_canvas[["z"]]), length.out = length(colorvalues))
  }
  artwork <- ggplot2::ggplot(data = full_canvas, ggplot2::aes(x = x, y = y, fill = z)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::scale_fill_gradientn(colours = colorvalues, values = scales::rescale(breaks, from = range(breaks), to = c(0, 1))) +
    ggplot2::coord_cartesian(xlim = c(0, (resolution * 2) * (duplicates + 1)), ylim = c(0, (resolution * 2) * (duplicates + 1)))
  if (duplicates > 0 && !is.null(col.line)) {
    lineDataX <- data.frame(x = (resolution * 2) * (0:(duplicates + 1)), xend = (resolution * 2) * (0:(duplicates + 1)), y = rep(0, duplicates + 2), yend = rep((resolution * 2) * (duplicates + 1), duplicates + 2))
    lineDataY <- data.frame(y = (resolution * 2) * (0:(duplicates + 1)), yend = (resolution * 2) * (0:(duplicates + 1)), x = rep(0, duplicates + 2), xend = rep((resolution * 2) * (duplicates + 1), duplicates + 2))
    lineDataX2 <- data.frame(x = resolution * (0:((duplicates * 2) + 1)), xend = resolution * (0:((duplicates * 2) + 1)), y = rep(0, (duplicates * 2) + 2), yend = rep(resolution * ((duplicates * 2) + 2), (duplicates * 2) + 2))
    lineDataY2 <- data.frame(y = resolution * (0:((duplicates * 2) + 1)), yend = resolution * (0:((duplicates * 2) + 1)), x = rep(0, (duplicates * 2) + 2), xend = rep(resolution * ((duplicates * 2) + 2), (duplicates * 2) + 2))
    artwork <- artwork + ggplot2::geom_segment(data = lineDataX2, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend), col = col.line, inherit.aes = FALSE, linewidth = cex.line * 0.1) +
      ggplot2::geom_segment(data = lineDataY2, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend), col = col.line, inherit.aes = FALSE, linewidth = cex.line * 0.1) +
      ggplot2::geom_segment(data = lineDataX, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend), col = col.line, inherit.aes = FALSE, linewidth = cex.line * 0.75) +
      ggplot2::geom_segment(data = lineDataY, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend), col = col.line, inherit.aes = FALSE, linewidth = cex.line * 0.75) +
      ggplot2::geom_segment(data = lineDataX, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend), col = background, inherit.aes = FALSE, linewidth = cex.line * 0.3) +
      ggplot2::geom_segment(data = lineDataY, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend), col = background, inherit.aes = FALSE, linewidth = cex.line * 0.3)
    if (size < 8) {
      for (i in 0:size) {
        for (j in 1:10) {
          pointData <- data.frame(x = rep(i * resolution * 2, 2000) + stats::rnorm(2000, 0, cex.line * 0.4), y = seq(0, resolution * 2 * size, length.out = 2000) + stats::rnorm(2000, 0, cex.line * 0.4))
          artwork <- artwork + ggplot2::geom_point(data = pointData, mapping = ggplot2::aes(x = x, y = y), fill = background, inherit.aes = FALSE, size = 0, alpha = 0.1, shape = 21, col = background) +
            ggplot2::geom_point(data = pointData, mapping = ggplot2::aes(x = y, y = x), fill = background, inherit.aes = FALSE, size = 0, alpha = 0.1, shape = 21, col = background)
        }
      }
    }
  }
  artwork <- aRtsy::theme_canvas(artwork)
  return(artwork)
}
