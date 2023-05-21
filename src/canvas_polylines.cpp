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
arma::mat cpp_polylines(arma::mat canvas,
                        const double& ratio,
                        const int& iters,
                        const int& rows,
                        const int& cols) {
  canvas.at(0, 0) = R::runif(0, cols);
  canvas.at(0, 1) = R::runif(0, rows);
  int xradius = cols * ratio, yradius = rows * ratio;
  bool c1, c2;
  for (int i = 1; i < iters; ++i) {
    Rcpp::checkUserInterrupt();
    const double v = R::runif(0, 1);
    const double h = R::runif(0, 1);
    double x1;
    if (h > 0.5) {
      x1 = canvas.at(i-1, 0) + R::rnorm(0, xradius);
      if (x1 < 0) {
        x1 = 0;
      } else if (x1 > cols) {
        x1 = cols;
      }
      c1 = x1 > (canvas.at(0, 0) + (xradius * R::runif(0.5, 2)));
      c2 = x1 < (canvas.at(0, 0) - (xradius * R::runif(0.5, 2)));
      if (c1 || c2) {
        x1 = canvas.at(i-1, 0);
      }
    } else {
      x1 = canvas.at(i-1, 0);
    }
    canvas.at(i, 0) = x1;
    double y1;
    if (v > 0.5) {
      y1 = canvas.at(i-1, 1) + R::rnorm(0, yradius);
      if (y1 < 0) {
        y1 = 0;
      } else if (y1 > rows) {
        y1 = rows;
      }
      c1 = y1 > (canvas.at(0, 1) + (yradius * R::runif(0.5, 2)));
      c2 = y1 < (canvas.at(0, 1) - (yradius * R::runif(0.5, 2)));
      if (c1 || c2) {
        y1 = canvas.at(i-1, 1);
      }
    } else {
      y1 = canvas.at(i-1, 1);
    }
    canvas.at(i, 1) = y1;
  }
  return canvas;
}
