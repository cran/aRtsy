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
arma::mat draw_circlemap(arma::mat X,
                         double left,
                         double right,
                         double bottom,
                         double top,
                         int iters) {
  int nrows = X.n_rows, ncols = X.n_cols;
  double K = right, phi = bottom;
  for (int iter = 0; iter < iters; iter++) {
    for (int row = 0; row < nrows; row++) {
      for (int col = 0; col < ncols; col++) {
        Rcpp::checkUserInterrupt();
        X(row, col) = X(row, col) + phi + K / (2 * M_PI) * sin(2 * M_PI * X(row, col));
        K = K - (right - left) / nrows;
      }
      phi = phi + (top - bottom) / ncols;
      K = right;
    }
  }
  return X;
}
