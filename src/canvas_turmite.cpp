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
arma::mat draw_turmite(arma::mat& canvas,
                       const int& iters,
                       int& row,
                       int& col,
                       const double& p) {
  const int nrows = canvas.n_rows, ncols = canvas.n_cols;
  int state = 0;
  for (int i = 0; i < iters; ++i) {
    if (i % 1000 == 0) {
      Rcpp::checkUserInterrupt();
    }
    double swap = R::runif(0, 1);
    if (swap < p) {
      state = (state == 0) ? 1 : 0;
    }
    if (canvas.at(row, col) == 1) {
      // Color
      if (state == 0) {
        if (canvas.at(row, col) == 0) {
          state = 0;
        } else {
          state = 1;
          canvas.at(row, col) = 0;
        }
      } else {
        if (canvas.at(row, col) == 0) {
          state = 0;
          canvas.at(row, col) = 0;
        }
      }
    } else {
      canvas.at(row,col) = 1;
    }
    // Turn
    int direction = ceil(R::runif(0, 4));
    if (state == 0) {
      if (direction == 1 && row < (nrows - 1)) {
        ++row; 
      } else if (direction == 2 && row >= 1) {
        --row;
      } else if (direction == 3 && col < (ncols - 1)) {
        ++col;
      } else if (direction == 4 && col >= 1) {
        --col;
      } 
    } else if (state == 1) {
      if (direction == 4 && row < (nrows - 2)) {
        ++row;
      } else if (direction == 1 && row >= 2) {
        --row;
      } else if (direction == 2 && col < (ncols - 2)) {
        ++col;
      } else if (direction == 3 && col >= 2) {
        --col;
      }
    }
  }
  return canvas;
}
