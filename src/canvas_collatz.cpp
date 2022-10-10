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
Rcpp::IntegerVector get_collatz_sequence(int x) {
  Rcpp::IntegerVector sequence = {1};
  while (x > 1) {
    Rcpp::checkUserInterrupt();
    if (x % 2 == 1) {
      x = 3 * x + 1;
      sequence.push_back(1);
    } else {
      x = x / 2;
      sequence.push_back(0);
    }
  }
  return sequence;
}

// [[Rcpp::export]]
arma::mat draw_collatz(arma::mat empty,
                       Rcpp::IntegerVector series,
                       double even,
                       double odd) {
  int s = series.size();
  double angle = M_PI / 2;
  for (int i = 1; i < s; i++) {
    Rcpp::checkUserInterrupt();
    empty(i, 0) = cos(angle) + empty(i - 1, 0);
    empty(i, 1) = sin(angle) + empty(i - 1, 1);
    if (series[i] == 0) {
      angle = angle - even;
    } else {
      angle = angle + odd;
    }
  }
  return empty;
}
