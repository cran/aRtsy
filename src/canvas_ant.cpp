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
arma::mat cpp_ant(arma::mat& canvas,
                  const arma::mat& directions,
                  const int& iterations,
                  const int& resolution) {
  const int ncolors = directions.n_rows, s = iterations / ncolors;
  int d = 0, c = 0, t0 = 0, t1 = 0, x = floor(R::runif(resolution * 0.05, resolution * 0.95)), y = floor(R::runif(resolution * 0.05, resolution * 0.95));
  for (int i = 0; i < iterations; ++i) {
    if (i % 1000 == 0) {
      Rcpp::checkUserInterrupt();
    }
    if (i % s == 0) { // Switch color every s iterations
      ++c; // Next color
      if (c > ncolors) {
        c = 1;
      }
      t0 = directions.at(c - 1, 0), t1 = directions.at(c - 1, 1);
    }
    if (canvas.at(x, y) == 0) { // White square
      if (t0 == 0) { // Turn 90 degrees clockwise for R (Langtons Ant)
        ++d;
        if (d == 5) {
          d = 1;
        }
      } else if (t1 == 1) { // Turn 90 degrees counter-clockwise for L
        --d;
        if (d == 0) {
          d = 4;
        }
      }
      // Color the square
      canvas.at(x, y) = c;
    } else { // Colored square
      if (t1 == 0) { // Turn 90 degrees counter-clockwise for L (Langtons ant)
        --d;
        if (d == 0) {
          d = 4;
        }
      } else { // Turn 90 degrees clockwise for R
        ++d;
        if (d == 5) {
          d = 1;
        }
      }
      // Undo the color on the square
      canvas.at(x, y) = 0;
    }
    // Move the ant
    if (d == 1) {
      --x; 
      if (x < 0) {
        x = resolution - 1;
      }
    } else if (d == 2) {
      --y;
      if (y < 0) {
        y = resolution - 1;
      }
    } else if (d == 3) {
      ++x;
      if (x >= (resolution - 1)) {
        x = 0;
      }
    } else if (d == 4) {
      ++y;
      if (y >= (resolution - 1)) {
        y = 0;
      }
    }
  }
  return canvas;
}
