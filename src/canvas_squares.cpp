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

int neighbor(int L, int i) {
  if (i < 0)
    return 0;
  if (i >= L)
    return L - 1;
  return i;
}

// [[Rcpp::export]]
arma::mat draw_squares(arma::mat X,
                       const Rcpp::DataFrame neighbors,
                       const int s,
                       const int cuts,
                       const double ratio) {
  // Constants
  const int m = X.n_rows, n = X.n_cols;
  // Variables
  bool cutfromtop, cutfromleft;
  int cutx, cuty, color, c;
  int row = m, col = n;
  Rcpp::IntegerVector dx = neighbors["x"], dy = neighbors["y"];
  // Main loop
  for (int i = 0; i < cuts; ++i) {
    // Check for interrupt
    Rcpp::checkUserInterrupt();
    cutx = ceil(row / ratio);
    cuty = ceil(col / ratio);
    cutfromtop = R::runif(0, 1) < 0.5;
    cutfromleft = R::runif(0, 1) < 0.5;
    color = ceil(R::runif(0, s)); // Sample color from 1 to s
    if (cutfromtop && cutfromleft) {
      for (int x = 0; x < cutx; ++x) {
        for (int y = 0; y < cuty; ++y) {
          X.at(y, x) = color;
        }
      }
    } else if (cutfromtop && !cutfromleft) {
      for (int x = 0; x < cutx; ++x) {
        for (int y = cuty; y < m; ++y) {
          X.at(y, x) = color;
        }
      }
    } else if (!cutfromtop && cutfromleft) {
      for (int x = cutx; x < n; ++x) {
        for (int y = 0; y < cuty; ++y) {
          X.at(y, x) = color;
        }
      }
    } else if (!cutfromtop && !cutfromleft) {
      for (int x = cutx; x < n; ++x) {
        for (int y = cuty; y < m; ++y) {
          X.at(y, x) = color;
        }
      }
    }
    row = floor(R::runif(0, m));
    col = floor(R::runif(0, n));
  }
  arma::mat X_new = X;
  // Second main loop
  for (int x = 0; x < n; ++x) {
    Rcpp::checkUserInterrupt();
    for (int y = 0; y < m; ++y) {
      Rcpp::IntegerVector colors(dx.size());
      for (int z = 0; z < dx.size(); ++z) {
        colors[z] = X.at(neighbor(m, y + dy[z]), neighbor(n, x + dx[z]));
      }
      std::sort(colors.begin(), colors.end());
      c = std::unique(colors.begin(), colors.end()) - colors.begin();
      if (c > 1) {
        X_new.at(y, x) = 0;
      }
    }
  }
  return X_new;
}
