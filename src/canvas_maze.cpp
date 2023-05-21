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

Rcpp::DataFrame mazeNeighbors(const int& x, 
                              const int& y,
                              const int& m,
                              const int& n) {
  Rcpp::NumericVector nx, ny;
  if (y > 0 && y <= m) {
    if (x < n) {
      nx.push_back(x + 1);
      ny.push_back(y);
    }
    if (x > 1) {
      nx.push_back(x - 1);
      ny.push_back(y);
    }
  }
  if (x > 0 && x <= n) {
    if (y > 1) {
      nx.push_back(x);
      ny.push_back(y - 1);
    }
    if (y < m) {
      nx.push_back(x);
      ny.push_back(y + 1);
    }
  }
  Rcpp::DataFrame neighbors = Rcpp::DataFrame::create(Rcpp::Named("x") = nx,
                                                      Rcpp::Named("y") = ny);
  return neighbors;
}

Rcpp::DataFrame selectNeighbors(const Rcpp::NumericVector& x, 
                                const Rcpp::NumericVector& y,
                                const Rcpp::NumericVector& vx,
                                const Rcpp::NumericVector& vy) {
  Rcpp::NumericVector nx, ny;
  for (int i = 0; i < x.length(); ++i) {
    int contains = 0;
    for (int j = 0; j < vx.length(); ++j) {
      if (x[i] == vx[j] && y[i] == vy[j]) {
        contains = 1;
      }
    }
    if (contains == 0) {
      nx.push_back(x[i]);
      ny.push_back(y[i]);
    }
  }
  Rcpp::DataFrame neighbors = Rcpp::DataFrame::create(Rcpp::Named("x") = nx,
                                                      Rcpp::Named("y") = ny);
  return neighbors;
}

// [[Rcpp::export]]
Rcpp::DataFrame cpp_maze(arma::mat& X,
                         double x, 
                         double y) {
  const int m = X.n_rows, n = X.n_cols, dim = m * n;
  Rcpp::NumericVector tx = {x}, ty = {y}, sx = {x}, sy = {y}, vx = {x}, vy = {y};
  while (vx.length() < dim) {
    Rcpp::checkUserInterrupt();
    Rcpp::DataFrame nn = mazeNeighbors(x, y, m, n);
    Rcpp::DataFrame snn = selectNeighbors(nn["x"], nn["y"], vx, vy);
    int nrows = snn.nrows();
    if (nrows > 0) {
      Rcpp::NumericVector nx = snn["x"];
      Rcpp::NumericVector ny = snn["y"];
      const int index = floor(R::runif(0, nrows));
      x = nx[index];
      y = ny[index];
      sx.insert(0, x);
      sy.insert(0, y);
      vx.insert(0, x);
      vy.insert(0, y);
    } else {
      sx.erase(0);
      sy.erase(0);
      x = sx[0];
      y = sy[0];
    }
    tx.insert(0, x);
    ty.insert(0, y);
  }
  Rcpp::DataFrame canvas = Rcpp::DataFrame::create(Rcpp::Named("x") = tx,
                                                   Rcpp::Named("y") = ty);
  return canvas;
}
