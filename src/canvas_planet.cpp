// Copyright (C) 2021-2023 Koen Derks

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat cpp_planet(arma::mat& canvas,
                     const int& resolution,
                     const int& radius,
                     const int& xcenter,
                     const int& ycenter,
                     const int& threshold,
                     const int& iterations,
                     const int& ncolors,
                     const int& colorsused,
                     const double& starprob,
                     const double& fade,
                     const bool& lightright) {
  const int nrows = canvas.n_rows, ncols = canvas.n_cols, n = resolution * resolution;
  Rcpp::IntegerVector xcircle(n), ycircle(n);
  int l = 0;
  // Part 1: Draw the planet circle
  for (int row = 0; row < nrows; ++row) {
    if (row % (nrows / 10) == 0) {
      Rcpp::checkUserInterrupt();
    }
    for (int col = 0; col < ncols; ++col) {
      const double dist = sqrt(pow(xcenter - col, 2) + pow(ycenter - row, 2));
      if (dist >= ceil(radius * 1.01) && canvas.at(row, col) == 0) { // The point lies in outer space
        double star = R::runif(0, 1);
        if (star < starprob) {
          canvas.at(row, col) = 2;
        }
      } else if (dist <= radius) { // The point lies on the planet
        xcircle[l] = col;
        ycircle[l] = row;
        canvas.at(row, col) = 3 + colorsused + floor(R::runif(0, ncolors - 3));
        ++l;
      } else if (dist > radius && dist < ceil(radius * 1.01)) { // The point lies on the edge
        canvas.at(row, col) = 0;
      }
    }
  }
  // Part 2: Fill the circle
  arma::mat& ref = canvas;
  for (int i = 0; i < iterations; ++i) {
    if (i % 10 == 0) {
      Rcpp::checkUserInterrupt();
    }
    for (int j = 0; j < l; ++j) {
      const int x = xcircle[j], y = ycircle[j];
      if ((y > 0) && (y < (nrows - 1)) && (x > 0) && (x < (ncols - 1))) {
        const int level = ref.at(y, x); // Get the current level 
        const int newlevel = (level + 1) == (ncolors + colorsused) ? 3 + colorsused : level + 1;
        int higherlevels = 0;
        if (ref.at(y - 1, x) == newlevel)     ++higherlevels;
        if (ref.at(y + 1, x) == newlevel)     ++higherlevels;
        if (ref.at(y - 1, x - 1) == newlevel) ++higherlevels;
        if (ref.at(y, x - 1) == newlevel)     ++higherlevels;
        if (ref.at(y + 1, x - 1) == newlevel) ++higherlevels;
        if (ref.at(y - 1, x + 1) == newlevel) ++higherlevels;
        if (ref.at(y, x + 1) == newlevel)     ++higherlevels;
        if (ref.at(y + 1, x + 1) == newlevel) ++higherlevels;
        canvas.at(y, x) = higherlevels >= threshold ? newlevel : level;
      }
    }
    ref = canvas;
  }
  // Part 3: Apply the shading
  for (int j = 0; j < l; ++j) {
    if (j % (l / 10) == 0) {
      Rcpp::checkUserInterrupt();
    }
    const int& x = xcircle[j], y = ycircle[j];
    const double xdist = abs(xcenter - x);
    if (!lightright) {
      if (x < xcenter) {
        canvas.at(y, x) -= (fade * (xdist / radius));
      } else {
        canvas.at(y, x) += (fade * (xdist / radius));
      }
    } else {
      if (x > xcenter) {
        canvas.at(y, x) -= (fade * (xdist / radius));
      } else {
        canvas.at(y, x) += (fade * (xdist / radius));
      }
    }
  }
  return canvas;
}
