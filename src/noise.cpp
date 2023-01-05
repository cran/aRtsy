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
arma::vec c_noise_knn(const arma::vec& x,
                      const arma::vec& y,
                      const arma::vec& z,
                      const arma::vec& newx,
                      const arma::vec& newy,
                      const int& k) {
  const int n = newx.n_elem;
  arma::vec newz(n);
  for(int i = 0; i < n; ++i) {
    if (i % 1000 == 0) {
      Rcpp::checkUserInterrupt();
    }
    const arma::uvec si = arma::sort_index(sqrt(pow(x - newx[i], 2) + pow(y - newy[i], 2)));
    for (int j = 0; j < k; ++j) {
      newz.at(i) += z.at(si.at(j)) / k;
    }
  }
  return newz;
}
