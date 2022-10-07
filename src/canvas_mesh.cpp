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
#include <iostream>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <iterator>

// [[Rcpp::depends(RcppArmadillo)]]

Rcpp::IntegerVector int_seq(int &first, int &last) {
  Rcpp::IntegerVector y(abs(last - first) + 1);
  if (first < last) {
   std::iota(y.begin(), y.end(), first);
  } else {
   std::iota(y.begin(), y.end(), last);
   std::reverse(y.begin(), y.end());
  }
  return y;
}

Rcpp::IntegerVector shift_right_int(Rcpp::IntegerVector x) {
  int x1 = x[0];
  x.erase(0);
  x.push_back(x1);
  return x;
}

Rcpp::DoubleVector shift_right_double(Rcpp::DoubleVector x) {
  double x1 = x[0];
  x.erase(0);
  x.push_back(x1);
  return x;
}

// [[Rcpp::export]]
Rcpp::DataFrame iterate_mesh(int iterations,
                             int start,
                             Rcpp::IntegerVector order,
                             Rcpp::DoubleVector points,
                             Rcpp::DoubleVector centers,
                             Rcpp::DoubleVector radii,
                             Rcpp::DoubleVector increase) {
  int n = (iterations + 1) * order.length();
  Rcpp::IntegerVector z(n);
  Rcpp::DoubleVector y(n);
  for (int i = 0; i < (iterations + 1); i++) {
    Rcpp::checkUserInterrupt();
    Rcpp::DoubleVector newy = start + centers[i] + radii * sin(points);
    int imin = i * order.length();
    int imax = i * order.length() + (order.length() - 1);
    Rcpp::IntegerVector index = int_seq(imin, imax);
    z[index] = order;
    y[index] = newy;
    order = shift_right_int(order);
    radii = shift_right_double(radii + increase);
  }
  Rcpp::DataFrame mesh = Rcpp::DataFrame::create(Rcpp::Named("y") = y,
                                                 Rcpp::Named("z") = z);
  return mesh;
}
