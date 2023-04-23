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

#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::DataFrame deform(Rcpp::DoubleVector& x,
                       Rcpp::DoubleVector& y,
                       Rcpp::DoubleVector& s,
                       const int& maxdepth,
                       const double& resolution) {
  // Variables
  int times, index;
  double bx, by, edgevar, angle;
  // Main loop
  for (int i = 0; i < maxdepth; ++i) {
	// Check for interrupt
    Rcpp::checkUserInterrupt();
    times = x.length() - 1;
	Rcpp::IntegerVector indexes(times, 0);
    // Inner loop
    for (int l = 1; l < times; ++l) {
      indexes[l] = indexes[l - 1] + 2; // Create seq(0, length(x) * 2 - 1, by = 2)
    }
    // Second inner loop
    for (int j = 0; j < times; ++j) {
      index = indexes[j];
      // Find midpoint
      bx = (x[index] + x[index + 1]) / 2;
      by = (y[index] + y[index + 1]) / 2;
      // Determine new point
      edgevar = (s[index] + s[index + 1]) / 2;
      angle = R::rnorm(0, 2);
      // Insert new point
      x.insert(index + 1, bx + edgevar * sin(angle));
      y.insert(index + 1, by + edgevar * cos(angle));
      s.insert(index + 1, edgevar * 0.5);
    }
  }
  // Second main loop
  for (int i = 0; i < x.length(); ++i) {
    Rcpp::checkUserInterrupt();
    // Check bounds
    x[i] = std::max(0.0, std::min(x[i], resolution));
    y[i] = std::max(0.0, std::min(y[i], resolution));
  }
  Rcpp::DataFrame newdata = Rcpp::DataFrame::create(Rcpp::Named("x") = x, Rcpp::Named("y") = y, Rcpp::Named("s") = s);
  return newdata;
}
