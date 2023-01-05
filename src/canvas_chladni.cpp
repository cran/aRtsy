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

#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector iterate_chladni(const Rcpp::NumericVector& x,
                                    const Rcpp::NumericVector& y,
                                    const Rcpp::NumericVector& waves) {
  const int n = x.length(), k = waves.length();
  Rcpp::NumericVector z(n);
  for (int i = 0; i < k; i++) {
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < n; j++) {
      z[j] += fabs(sin(waves[i] *  x[j]) * sin(waves[i] * y[j]));
    }
  }
  return z;
}
