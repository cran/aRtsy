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
Rcpp::DataFrame iterate_flow(arma::mat& canvas,
                             const arma::mat& angles,
                             const int& lines,
                             const int& iters,
                             const int& ncolors,
                             const int& left,
                             const int& right,
                             const int& top,
                             const int& bottom,
                             const double& stepmax) {
  // Constants
  const int nrows = angles.n_rows, ncols = angles.n_cols;
  // Variables
  double x, y, step;
  int col_index, row_index, c;
  // Main loop
  for (int j = 0; j < lines; ++j) {
    // Check for interrupt
    Rcpp::checkUserInterrupt();
    // Initialize variables
    x = ceil(R::runif(left + 1, right - 1));
    y = ceil(R::runif(bottom + 1, top - 1));
    step = R::runif(0, 100 * stepmax);
    c = ceil(R::runif(0, ncolors));
    // Inner loop
    for (int i = 0; i < iters; i++) {
      // Assign values
      canvas.at(j * iters + i, 0) = x;
      canvas.at(j * iters + i, 1) = y;
      canvas.at(j * iters + i, 2) = j + 1;
      canvas.at(j * iters + i, 3) = c;
      // Get position
      col_index = x - left;
      row_index = y - bottom;
      // Check bailout condition
      if ((col_index >= ncols) || (col_index <= 0) || (row_index >= nrows) || (row_index <= 0)) {
        break;
      }
      // Update position
      x += step * cos(angles.at(row_index, col_index));
      y += step * sin(angles.at(row_index, col_index));
    }
  }
  return canvas;
}
