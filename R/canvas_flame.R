# Copyright (C) 2021-2022 Koen Derks

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

#' Draw a Fractal Flame
#'
#' @description This function implements the fractal flame algorithm.
#'
#' @usage canvas_flame(colors, background = "#000000",
#'              iterations = 1000000, zoom = 1, resolution = 1000,
#'              variations = NULL, blend = TRUE,
#'              display = c("colored", "logdensity"),
#'              post = FALSE, final = FALSE, extra = FALSE,
#'              verbose = FALSE)
#'
#' @param colors      a string or character vector specifying the color(s) used for the artwork.
#' @param background  a character specifying the color used for the background.
#' @param iterations  a positive integer specifying the number of iterations of the algorithm.
#' @param zoom        a positive value specifying the amount of zooming.
#' @param resolution  resolution of the artwork in pixels per row/column. Increasing the resolution increases the quality of the artwork but also increases the computation time exponentially.
#' @param variations  an integer (vector) specifying the variations to be included in the flame. The default \code{NULL} includes a random number of variations. See the details section for more information about possible variations.
#' @param blend       logical. Whether to blend the variations (\code{TRUE}) or pick a unique variation in each iteration (\code{FALSE}). \code{blend = FALSE} significantly speeds up the computation time.
#' @param display     a character indicating how to display the flame. \code{colored} (the default) displays colors according to which function they originate from. \code{logdensity} plots a gradient using the log density of the pixel count.
#' @param post        logical. Whether to apply a post transformation in each iteration.
#' @param final       logical. Whether to apply a final transformation in each iteration.
#' @param extra       logical. Whether to apply an additional post transformation after the final transformation. Only has an effect when \code{final = TRUE}.
#' @param verbose     logical. Whether to print information.
#'
#' @details           The \code{variation} argument can be used to include specific variations into the flame. See the appendix in the references for examples of all variations. Possible variations are:
#'
#' \itemize{
#'  \item{\code{0}: Linear}
#'  \item{\code{1}: Sine}
#'  \item{\code{2}: Spherical}
#'  \item{\code{3}: Swirl}
#'  \item{\code{4}: Horsehoe}
#'  \item{\code{5}: Polar}
#'  \item{\code{6}: Handkerchief}
#'  \item{\code{7}: Heart}
#'  \item{\code{8}: Disc}
#'  \item{\code{9}: Spiral}
#'  \item{\code{10}: Hyperbolic}
#'  \item{\code{11}: Diamond}
#'  \item{\code{12}: Ex}
#'  \item{\code{13}: Julia}
#'  \item{\code{14}: Bent}
#'  \item{\code{15}: Waves}
#'  \item{\code{16}: Fisheye}
#'  \item{\code{17}: Popcorn}
#'  \item{\code{18}: Exponential}
#'  \item{\code{19}: Power}
#'  \item{\code{20}: Cosine}
#'  \item{\code{21}: Rings}
#'  \item{\code{22}: Fan}
#'  \item{\code{23}: Blob}
#'  \item{\code{24}: PDJ}
#'  \item{\code{25}: Fan2}
#'  \item{\code{26}: Rings2}
#'  \item{\code{27}: Eyefish}
#'  \item{\code{28}: Bubble}
#'  \item{\code{29}: Cylinder}
#'  \item{\code{30}: Perspective}
#'  \item{\code{31}: Noise}
#'  \item{\code{32}: JuliaN}
#'  \item{\code{33}: JuliaScope}
#'  \item{\code{34}: Blur}
#'  \item{\code{35}: Gaussian}
#'  \item{\code{36}: RadialBlur}
#'  \item{\code{37}: Pie}
#'  \item{\code{38}: Ngon}
#'  \item{\code{39}: Curl}
#'  \item{\code{40}: Rectangles}
#'  \item{\code{41}: Arch}
#'  \item{\code{42}: Tangent}
#'  \item{\code{43}: Square}
#'  \item{\code{44}: Rays}
#'  \item{\code{45}: Blade}
#'  \item{\code{46}: Secant}
#'  \item{\code{47}: Twintrian}
#'  \item{\code{48}: Cross}
#' }
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @references \url{https://flam3.com/flame_draves.pdf}
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
#' canvas_flame(colors = c("dodgerblue", "green"))
#'
#' # Advanced example (no-blend sine and spherical variations)
#' canvas_flame(colors = colorPalette("origami"), variations = c(1, 2), blend = FALSE)
#' }
#'
#' @export

canvas_flame <- function(colors, background = "#000000",
                         iterations = 1000000, zoom = 1, resolution = 1000,
                         variations = NULL, blend = TRUE,
                         display = c("colored", "logdensity"),
                         post = FALSE, final = FALSE, extra = FALSE,
                         verbose = FALSE) {
  display <- match.arg(display)
  .checkUserInput(
    resolution = resolution, background = background
  )
  iterations <- iterations + 20
  varNames <- .getVariationNames()
  noVariations <- length(varNames)
  user <- FALSE
  if (is.null(variations)) {
    user <- TRUE
    v <- 0:(noVariations - 1)
    variations <- sample(x = v[-c(32, 35, 36, 37)], size = sample(2:5, size = 1), replace = FALSE)
  } else if (min(variations) < 0 || max(variations) > (noVariations - 1)) {
    stop("'variations' must be between 0 and ", (noVariations - 1))
  }
  if (verbose) {
    cat("\nVariation:\t", paste(varNames[variations + 1], collapse = " + "), "\n")
    catp <- if (post) "Post transformation" else NULL
    catc <- if (final) "Final transformation" else NULL
    cate <- if (extra) "Additional post transformation" else NULL
    cat("Effect:\t\t", paste(c("Affine transformation", catp, catc, cate), collapse = " + "), "\n")
    catd <- if (display == "logdensity") "Log-Density" else "Colored"
    cat("Rendering:\t", catd, "\n")
  }
  nvariations <- length(variations)
  if (display == "logdensity") {
    nfunc <- sample(x = 3:10, size = 1)
    color_mat <- matrix(stats::runif(nfunc * 3), nrow = nfunc, ncol = 3)
  } else {
    nfunc <- sample(x = 3:max(10, length(colors)), size = 1)
    colors <- sample(x = colors, size = nfunc, replace = TRUE)
    color_mat <- matrix(t(grDevices::col2rgb(colors) / 255), nrow = length(colors), ncol = 3)
  }
  w_i <- stats::runif(nfunc, 0, 1)
  if (user) {
    v_ij <- matrix(1, nrow = nfunc, ncol = nvariations)
  } else {
    v_ij <- matrix(stats::runif(nfunc * nvariations, min = 0, max = 1), nrow = nfunc, ncol = nvariations)
  }
  for (i in 1:nrow(v_ij)) {
    v_ij[i, ] <- v_ij[i, ] / sum(v_ij[i, ])
  }
  df <- iterate_flame(
    iterations = iterations,
    functions = 0:(nfunc - 1),
    variations = variations,
    point = c(stats::runif(2, -1, 1), stats::runif(3, 0, 1)),
    w_i = w_i / sum(w_i),
    mat_coef = matrix(stats::runif(nfunc * 6, min = -1, max = 1), nrow = nfunc, ncol = 6),
    blend_variations = blend,
    v_ij = v_ij,
    v_params = .getVariationParameters(),
    transform_p = post,
    p_coef = matrix(stats::runif(nfunc * 6, min = -1, max = 1), nrow = nfunc, ncol = 6),
    transform_f = final,
    f_coef = stats::runif(6, min = -1, max = 1),
    transform_e = extra,
    e_coef = stats::runif(6, min = -1, max = 1),
    colors = color_mat
  )
  df <- df[!is.infinite(df[["x"]]) & !is.infinite(df[["y"]]), ]
  df <- df[!is.na(df[["x"]]) & !is.na(df[["y"]]), ]
  if (nrow(df) == 0) {
    stop("The algorithm did not converge")
  }
  center <- c(stats::median(df[["x"]]), stats::median(df[["y"]]))
  spanx <- diff(stats::quantile(df[["x"]], probs = c(0.1, 0.9))) * (1 / zoom)
  spany <- diff(stats::quantile(df[["y"]], probs = c(0.1, 0.9))) * (1 / zoom)
  canvas <- color_flame( # 1 = alpha, 2 = red, 3 = green, 5 = blue
    canvas = array(0, dim = c(resolution + 1, resolution + 1, 4)),
    binsx = seq(center[1] - spanx, center[1] + spanx, length.out = resolution + 1),
    binsy = seq(center[2] - spany, center[2] + spany, length.out = resolution + 1),
    x = df[["x"]], y = df[["y"]], c1 = df[["c1"]], c2 = df[["c2"]], c3 = df[["c3"]]
  )
  if (length(which(canvas[, , 1] > 0)) < 1) {
    stop("No points are drawn on the canvas")
  }
  full_canvas <- .unraster(canvas[, , 1], c("x", "y", "z"))
  full_canvas[["z"]][full_canvas[["z"]] != 0] <- log(full_canvas[["z"]][full_canvas[["z"]] != 0], base = 1.2589)
  full_canvas[["z"]][full_canvas[["z"]] == 0] <- NA
  if (display == "logdensity") {
    artwork <- ggplot2::ggplot(data = full_canvas, mapping = ggplot2::aes(x = x, y = y, fill = z)) +
      ggplot2::geom_raster(interpolate = TRUE) +
      ggplot2::scale_fill_gradientn(colors = colors, na.value = background)
  } else {
    canvas <- .scaleColorChannels(canvas)
    maxColorValue <- max(c(c(canvas[, , 2]), c(canvas[, , 3]), c(canvas[, , 4])), na.rm = TRUE)
    if (maxColorValue == 0) {
      stop("No points are drawn on the canvas")
    }
    full_canvas[["col"]] <- grDevices::rgb(red = canvas[, , 2], green = canvas[, , 3], blue = canvas[, , 4], maxColorValue = maxColorValue)
    full_canvas[["col"]][which(is.na(full_canvas[["z"]]))] <- background
    artwork <- ggplot2::ggplot(data = full_canvas, mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_raster(interpolate = TRUE, fill = full_canvas[["col"]])
  }
  artwork <- theme_canvas(artwork, background = background)
  return(artwork)
}

.getVariationNames <- function() {
  return(c(
    "Linear", "Sine", "Spherical", "Swirl", "Horsehoe", "Polar", "Handkerchief",
    "Heart", "Disc", "Spiral", "Hyperbolic", "Diamond", "Ex", "Julia", "Bent",
    "Waves", "Fisheye", "Popcorn", "Exponential", "Power", "Cosine", "Rings",
    "Fan", "Blob", "PDJ", "Fan2", "Rings2", "Eyefish", "Bubble", "Cylinder",
    "Perspective", "Noise", "JuliaN", "JuliaScope", "Blur", "Gaussian",
    "RadialBlur", "Pie", "Ngon", "Curl", "Rectangles", "Arch", "Tangent",
    "Square", "Rays", "Blade", "Secant", "Twintrian", "Cross"
  ))
}

.getVariationParameters <- function() {
  return(c(
    stats::runif(1, 0, 1), stats::runif(1, -1, 0), stats::runif(1, 1, 10), # blob.high, blob.low, blob.waves
    stats::runif(4, 0, 1), # padj.a, pdj.b, pdj.c, pdj.d
    stats::runif(1, 0, 1), # rings2.val
    stats::runif(1, 1, pi), stats::runif(1, 0, 1), # perspective.angle, perspective.dist
    stats::runif(1, 1, 5), stats::runif(1, 0, 10), # juliaN.power, juliaN.dist
    stats::runif(1, 1, 5), stats::runif(1, 0, 10), # juliaScope.power, juliaScope.dist
    stats::runif(1, 1, pi), stats::runif(1, 1, 5), # radialBlur.angle, v_36
    sample(1:10, size = 1), stats::runif(1, 1, pi), stats::runif(1, 1, 5), # pie.slices, pie.rotation, pie.thickness
    stats::runif(1, 1, 4), 2 * pi / sample(3:10, size = 1), sample(2:10, size = 1), stats::runif(1, 0, 1), # ngon.power, ngon.sides, ngon.corners, ngon.circle
    stats::runif(1, 0, 1), stats::runif(1, 0, 1), # curl.c1, curl.c2
    stats::runif(1, 2, 50), stats::runif(1, 2, 50), # rectangles.x, rectangles.y
    stats::runif(1, 0, 100), # v_41
    stats::runif(4, 0, 10) # v_44, # v_45, # v_46, # v_47
  ))
}

.scaleColorChannels <- function(canvas) {
  hits <- which(canvas[, , 1] > 0)
  scaling <- log(canvas[, , 1][hits]) / canvas[, , 1][hits]
  canvas[, , 2][hits] <- canvas[, , 2][hits] * scaling
  canvas[, , 3][hits] <- canvas[, , 3][hits] * scaling
  canvas[, , 4][hits] <- canvas[, , 4][hits] * scaling
  return(canvas)
}
