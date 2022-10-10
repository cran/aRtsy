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
Rcpp::DataFrame deform(Rcpp::DataFrame canvas,
                       int maxdepth,
                       int resolution) {
  Rcpp::DoubleVector x = canvas["x"];
  Rcpp::DoubleVector y = canvas["y"];
  Rcpp::DoubleVector s = canvas["s"];
  for (int i = 0; i < maxdepth; i++) {
    int times = x.length() - 1;
    Rcpp::IntegerVector indexes (times, 0);
    int isize = indexes.length();
    // Create seq(0, length(x) * 2 - 1, by = 2)
    for (int l = 1; l < isize; l++) {
      indexes[l] = indexes[l - 1] + 2;
    }
    // Perform deformation on each of the lines
    for (int j = 0; j < isize; j++) {
      Rcpp::checkUserInterrupt();
      // For each line A -> C in the polygon, find the midpoint, B.
      double bx = (x[indexes[j]] + x[indexes[j] + 1]) / 2;
      double by = (y[indexes[j]] + y[indexes[j] + 1]) / 2;
      // Determine the variance, angle, and length of break
      double edgevar = (s[indexes[j]] + s[indexes[j] + 1]) / 2;
      double angle = R::rnorm(0, 2);
      // Pick a new point B'.
      double bstarx = bx + edgevar * sin(angle);
      double bstary = by + edgevar * cos(angle);
      // Insert new point into the data
      x.insert(indexes[j] + 1, bstarx);
      y.insert(indexes[j] + 1, bstary);
      s.insert(indexes[j] + 1, edgevar * 0.5);
    }
  }
  // Check bounds
  for (int i = 0; i < x.length(); i++) {
    Rcpp::checkUserInterrupt();
    if (x[i] < 0) {
      x[i] = 0;
    } else if (x[i] > resolution) {
      x[i] = resolution;
    }
    if (y[i] < 0) {
      y[i] = 0;
    } else if (y[i] > resolution) {
      y[i] = resolution;
    }
  }
  Rcpp::DataFrame newdata = Rcpp::DataFrame::create(Rcpp::Named("x") = x, Rcpp::Named("y") = y, Rcpp::Named("s") = s);
  return newdata;
}
