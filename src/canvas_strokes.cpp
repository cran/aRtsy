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

int neighboring_block(const int& L, 
                      const int& i) {
  int x = i;
  if (i < 0) {
    x = (L + i % L) % L;
  } else if (i >= L) {
    x = i % L;
  }
  return x;
}

// [[Rcpp::export]]
arma::mat draw_strokes(arma::mat& canvas,
                       arma::mat& neighbors,
                       const int& s,
                       const double& p) {
  const double backwards = R::runif(0, 1);
  const int nrows = canvas.n_rows, ncols = canvas.n_cols, k = neighbors.n_rows;
  for (int x = 0; x < ncols; ++x) {
    if (x % 100 == 0) {
      Rcpp::checkUserInterrupt();
    }
    if (backwards < 0.5) {
      for (int y = 0; y < nrows; ++y) {
        Rcpp::IntegerVector colors(k);
        int len = 0;
        for (int z = 0; z < k; ++z) {
          int color = canvas.at(neighboring_block(ncols, x + neighbors.at(z, 0)), neighboring_block(nrows, y + neighbors.at(z, 1)));
          if (color > 0) {
            colors[len] = color;
            ++len;
          }
        }
        const double prob = R::runif(0, 1);
        if (len > 0 && prob > p) {
          const int index = floor(R::runif(0, len));
          canvas.at(x, y) = colors[index];
        } else {
          canvas.at(x, y) = ceil(R::runif(0, s));
        }
      }
    } else {
      for (int y = nrows; y --> 0;) {
        Rcpp::IntegerVector colors(k);
        int len = 0;
        for (int z = 0; z < k; ++z) {
          int color = canvas.at(neighboring_block(ncols, x + neighbors.at(z, 0)), neighboring_block(nrows, y + neighbors.at(z, 1)));
          if (color > 0) {
            colors[len] = color;
            ++len;
          }
        }
        const double prob = R::runif(0, 1);
        if (len > 0 && prob > p) {
          const int index = floor(R::runif(0, len));
          canvas.at(x, y) = colors[index];
        } else {
          canvas.at(x, y) = ceil(R::runif(0, s));
        }
      }
    }
  }
  return canvas;
}
