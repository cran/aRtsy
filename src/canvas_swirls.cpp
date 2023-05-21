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

double gen_simplex(const double& x,
                   const double& y,
                   const double& z,
                   const double& freq,
                   const int& seed) {
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("ambient");
  Rcpp::Function f = pkg["gen_simplex"];
  Rcpp::NumericVector result = f( Rcpp::Named("x", x), Rcpp::Named("y", y), Rcpp::Named("z", z), Rcpp::Named("frequency", freq), Rcpp::Named("seed", seed));
  double out = result[0];
  return out;
}

Rcpp::NumericVector init_swirl_position(const double& scale) {
  const double r = R::runif(0, 1) * 2 * M_PI;
  Rcpp::NumericVector out = Rcpp::NumericVector::create(cos(r) * scale, sin(r) * scale);
  return out;
}

void reset_swirl(arma::mat& particles,
                 const Rcpp::IntegerVector& indices,
                 const int& resolution,
                 const int& ncols) {
  double scale = resolution / 2;
  for (int index : indices) {
    particles.at(index, 0) = arma::max(particles.col(0)) + 1;     // id
    Rcpp::NumericVector pos = init_swirl_position(R::runif(0, scale * 0.5));
    particles.at(index, 1) = pos[0] + scale;                      // x-position
    particles.at(index, 2) = pos[1] + scale;                      // y-position
    particles.at(index, 3) = R::runif(0.01, 10);                  // radius
    particles.at(index, 4) = R::runif(1, 1000);                    // duration
    particles.at(index, 5) = R::runif(0, particles.at(index, 4)); // time
    particles.at(index, 6) = R::runif(-1, 1);                     // x-velocity
    particles.at(index, 7) = R::runif(-1, 1);                     // y-velocity
    particles.at(index, 8) = R::runif(0.5, 2);                    // speed
    particles.at(index, 9) = ceil(R::runif(0, ncols));            // color
  }
}

// [[Rcpp::export]]
arma::mat cpp_swirls(const arma::mat& heightMap,
                     const int& iterations,
                     const int& n,
                     const int& resolution,
                     const int& ncols,
                     const double& lwd,
                     const double& freq) {
  int time = 0;
  const double seed = ceil(R::runif(0, INT_MAX));
  Rcpp::IntegerVector indices = Rcpp::seq(0, n - 1);
  arma::mat particles(n, 10), canvas(iterations * n, 7);
  reset_swirl(particles, indices, resolution, ncols);
  for (int i = 0; i < iterations; ++i) {
    ++time;
    Rcpp::checkUserInterrupt();
    for (int j = 0; j < n; ++j) {
      // Look up texture
      const double x = particles.at(j, 1);
      const double y = particles.at(j, 2);
      const double fx = fmin(fmax(round(x), 0), resolution - 1);
      const double fy = fmin(fmax(round(y), 0), resolution - 1);
      const double heightValue = heightMap.at(fy, fx) / 255;
      // Calculate line width
      const double s2 = R::runif(0.0001, 0.05);
      const double r = particles.at(j, 3) * fabs(gen_simplex(x * s2, y * s2, particles.at(j, 4) + time, freq, seed));
      const double width = r * fmin(fmax(heightValue, 0.01), lwd) * (particles.at(j, 5) / particles.at(j, 4));
      // Calculate angle
      double pS = fmin(fmax(heightValue, 0.00001), 0.0001);
      const double angle = gen_simplex(fx * pS, fy * pS, particles.at(j, 4) + time, freq, seed) * M_PI * 2;
      // Calculate speed
      const double speed = particles.at(j, 8) + fmin(fmax(1 - heightValue, 0), 2);
      // Update particle velocity
      Rcpp::DoubleVector velocity = {particles.at(j, 6) + cos(angle), particles.at(j, 7) + sin(angle)};
      velocity = velocity / sqrt(sum(pow(velocity, 2)));
      particles.at(j, 6) = velocity[0];
      particles.at(j, 7) = velocity[1];
      // Update particle position
      Rcpp::DoubleVector move = {velocity[0] * speed, velocity[1] * speed};
      particles.at(j, 1) = particles.at(j, 1) + move[0];
      particles.at(j, 2) = particles.at(j, 2) + move[1];
      // Record position
      const int index = i * n + j;
      canvas.at(index, 0) = x;                  // x-position
      canvas.at(index, 1) = y;                  // y-position
      canvas.at(index, 2) = particles.at(j, 1); // ending x-position
      canvas.at(index, 3) = particles.at(j, 2); // ending y-position
      canvas.at(index, 4) = particles.at(j, 9); // color
      canvas.at(index, 5) = particles.at(j, 0); // id
      canvas.at(index, 6) = width;              // width
      // Update time
      ++particles.at(j, 5);
      if (particles.at(j, 5) > particles.at(j, 4) || (particles.at(j, 1) < 0 || particles.at(j, 1) > resolution) || (particles.at(j, 2) < 0 || particles.at(j, 2) > resolution)) {
        reset_swirl(particles, Rcpp::IntegerVector::create(j), resolution, ncols);
      }
    }
  }
  return canvas;
}
