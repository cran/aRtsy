#' Paint Ribbons on a Canvas
#'
#' @description This function paints ribbons and (optionally) a triangle in the middle.
#'
#' @usage canvas_ribbons(colors, background = '#fdf5e6', triangle = TRUE)
#'
#' @param colors      a character (vector) specifying the colors for the ribbons. Colors determine the number of ribbons.
#' @param background  a character specifying the color of the background.
#' @param triangle    logical. Whether to draw the triangle that breaks the ribbon polygons.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' canvas_ribbons(colors = colorPalette('tuscany1'))
#' }
#' 
#' @keywords artwork canvas
#'
#' @export

canvas_ribbons <- function(colors, background = '#fdf5e6', triangle = TRUE) {
  x <- y <- NULL
  # Create an empty figure
  artwork <- ggplot2::ggplot() +
    ggplot2::xlim(c(0, 100)) +
    ggplot2::ylim(0, 100)
  # Determine points on the triangle
  y_max_top <- 75 - 7
  tpl <- data.frame(x = 16:49, y = seq(from = 16, to = 75, length.out = 34))
  tpl <- tpl[which(tpl$y < y_max_top), ]
  tpr <- data.frame(x = 51:84, y = seq(from = 74, to = 16, length.out = 34))
  tpr <- tpr[which(tpr$y < y_max_top), ]
  for (i in 1:length(colors)) {
    # Determine points on left side of triangle
    bpb <- data.frame(x = 0, y = sample(10:90, size = 1))
    fpb <- tpl[sample(1:nrow(tpl), size = 1), ]
    spb <- tpr[sample(1:nrow(tpr), size = 1), ]
    epb <- data.frame(x = 100, y = sample(10:90, size = 1))
    # Determine points on right side of triangle
    bpt <- data.frame(x = 0, y = bpb$y + 5)
    fpt <- data.frame(x = fpb$x + 2.5, y = fpb$y + 5)
    spt <- data.frame(x = spb$x - 2.5, y = spb$y + 5)
    ept <- data.frame(x = 100, y = epb$y + 5)
    # Combine polygon points
    polygon <- rbind(bpb, fpb, spb, epb, ept, spt, fpt, bpt)
    artwork <- artwork + ggplot2::geom_polygon(data = polygon, mapping = ggplot2::aes(x = x, y = y), 
                                               fill = colors[i], color = NA, 
                                               stat = "identity", alpha = 1)
  }
  # (Optionally) draw the triangle 
  if (triangle)
    artwork <- artwork + ggplot2::geom_polygon(data = data.frame(x = c(15, 50, 85), y = c(15, 75, 15)), mapping = ggplot2::aes(x = x, y = y), 
                                               fill = NA, color = "black", 
                                               stat = "identity", size = 1)
  artwork <- themeCanvas(artwork, background)
  return(artwork)
}