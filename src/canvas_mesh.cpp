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

void shift_right_int(Rcpp::IntegerVector& x) {
  const int x1 = x[0];
  x.erase(0);
  x.push_back(x1);
}

void shift_right_double(Rcpp::DoubleVector& x) {
  const double x1 = x[0];
  x.erase(0);
  x.push_back(x1);
}

// [[Rcpp::export]]
Rcpp::DataFrame cpp_mesh(arma::mat& canvas,
                         const Rcpp::DoubleVector& points,
                         const Rcpp::DoubleVector& centers,
                         const int& iterations,
                         const int& start,
                         Rcpp::IntegerVector& order,
                         Rcpp::DoubleVector& radii,
                         Rcpp::DoubleVector& increase) {
  const int l = order.length();
  for (int i = 0; i < (iterations + 1); ++i) {
    if (i % 100 == 0) {
      Rcpp::checkUserInterrupt();
    }
    Rcpp::DoubleVector newy = start + centers[i] + radii * sin(points);
    const Rcpp::IntegerVector index = Rcpp::Range(i * l, i * l + (l- 1));
    for (int j = 0; j < l; ++j) {
      canvas.at(index[j], 0) = newy[j];
      canvas.at(index[j], 1) = order[j];
    }
    shift_right_int(order);
    radii += increase;
    shift_right_double(radii);
  }
  return canvas;
}
