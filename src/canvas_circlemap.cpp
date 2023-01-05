// Copyright (C) 2021-2022 Koen Derks

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
arma::mat draw_circlemap(arma::mat& canvas,
                         const double& left,
                         const double& right,
                         const double& bottom,
                         const double& top,
                         const int& iters) {
  const int nrows = canvas.n_rows, ncols = canvas.n_cols;
  const double twopi = 2 * M_PI, xrange = right - left, yrange = top - bottom;
  double K = right, phi = bottom;
  for (int iter = 0; iter < iters; iter++) {
    Rcpp::checkUserInterrupt();
    for (int row = 0; row < nrows; ++row) {
      for (int col = 0; col < ncols; ++col) {
        canvas.at(row, col) = canvas.at(row, col) + phi + K / twopi * sin(twopi * canvas.at(row, col));
        K = K - xrange / nrows;
      }
      phi = phi + yrange / ncols;
      K = right;
    }
  }
  return canvas;
}
