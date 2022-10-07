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
#include <iostream>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <iterator>

// [[Rcpp::depends(RcppArmadillo)]]

Rcpp::DoubleVector affine(Rcpp::DoubleVector p,
                         double a,
                         double b,
                         double c,
                         double d,
                         double e,
                         double f) {
  Rcpp::DoubleVector x = p;
  x[0] = a * p[0] + b * p[1] + c;
  x[1] = d * p[0] + e * p[1] + f;
  return x;
}

Rcpp::DoubleVector variation(Rcpp::DoubleVector p,
                             int i,
                             double a,
                             double b,
                             double c,
                             double d,
                             double e,
                             double f,
                             Rcpp::DoubleVector pparams) {;
  Rcpp::DoubleVector x = p;
  double r = sqrt(pow(p[0], 2) + pow(p[1], 2));
  double theta = atan(p[0] / p[1]);
  double phi = atan(p[1] / p[0]);
  if (i == 0) { // Linear
    x[0] = p[0];
    x[1] = p[1];
  } else if (i == 1) { // Sine
    x[0] = sin(p[0]);
    x[1] = sin(p[1]);
  } else if (i == 2) { // Sperical
    x[0] = p[0] / pow(r, 2);
    x[1] = p[1] / pow(r, 2);
  } else if (i == 3) { // Swirl
    x[0] = p[0] * sin(pow(r, 2)) - p[1] * cos(pow(r, 2));
    x[1] = p[0] * cos(pow(r, 2)) + p[1] * sin(pow(r, 2));
  } else if (i == 4) { // Horsehoe
    x[0] = (1 / r) * ((p[0] - p[1]) * (p[0] + p[1]));
    x[1] = (1 / r) * (2 * p[0] * p[1]);
  } else if (i == 5) { // Polar
    x[0] = theta / M_PI;
    x[1] = r - 1;
  } else if (i == 6) { // Handkerchief
    x[0] = r * sin(theta + r);
    x[1] = r * cos(theta - r);
  } else if (i == 7) { // Heart
    x[0] = r * sin(theta * r);
    x[1] = r * (-cos(theta * r));
  } else if (i == 8) { // Disc
    x[0] = theta / M_PI * sin(M_PI * r);
    x[1] = theta / M_PI * cos(M_PI * r);
  } else if (i == 9) { // Spiral
    x[0] = 1 / r * (cos(theta) + sin(r));
    x[1] = 1 / r * (sin(theta) + cos(r));
  } else if (i == 10) { // Hyperbolic
    x[0] = sin(theta) / r;
    x[1] = r * cos(theta);
  } else if (i == 11) { // Diamond
    x[0] = sin(theta) * cos(r);
    x[1] = cos(theta) * sin(r);
  } else if (i == 12) { // Ex
    double p0 = sin(theta + r);
    double p1 = cos(theta - r);
    x[0] = r * (pow(p0, 3) + pow(p1, 3));
    x[1] = r * (pow(p0, 3) - pow(p1, 3));
  } else if (i == 13) { // Julia
    double s = R::runif(0, 1);
    double Omega;
    if (s < 0.5) {
      Omega = 0;
    } else {
      Omega = M_PI;
    }
    x[0] = sqrt(r) * cos(theta / 2 + Omega);
    x[1] = sqrt(r) * sin(theta / 2 + Omega);
  } else if (i == 14) { // Bent
    if ((p[0] >= 0) & (p[1] >= 0)) {
      x[0] = p[0];
      x[1] = p[1];
    } else if ((p[0] < 0) & (p[1] >= 0)) {
      x[0] = 2 * p[0];
      x[1] = p[1];
    } else if ((p[0] >= 0) & (p[1] < 0)) {
      x[0] = p[0];
      x[1] = p[1] / 2;
    } else if ((p[0] < 0) & (p[1] < 0)) {
      x[0] = 2 * p[0];
      x[1] = p[1] / 2;
    }
  } else if (i == 15) { // Waves
    x[0] = p[0] + b * sin(p[1] / pow(c, 2));
    x[1] = p[1] + e * sin(p[0] / pow(f, 2));
  } else if (i == 16) { // Fisheye
    x[0] = (2 / (r + 1)) * p[1];
    x[1] = (2 / (r + 1)) * p[0];
  } else if (i == 17) { // Popcorn
    x[0] = p[0] + c * sin(tan(3 * p[1]));
    x[1] = p[1] + f * sin(tan(3 * p[0]));
  } else if (i == 18) { // Exponential
    x[0] = exp(p[0] - 1) * cos(M_PI * p[1]);
    x[1] = exp(p[0] - 1) * sin(M_PI * p[1]);
  } else if (i == 19) { // Power
    x[0] = pow(r, sin(theta)) * cos(theta);
    x[1] = pow(r, sin(theta)) * sin(theta);
  } else if (i == 20) { // Cosine
    x[0] = cos(M_PI * p[0]) * cosh(p[1]);
    x[1] = -(sin(M_PI * p[0]) * sinh(p[1]));
  } else if (i == 21) { // Rings
    double first = fmod(r + pow(c, 2), 2 * pow(c, 2)) - pow(c, 2) + r * (1 - pow(c, 2));
    x[0] = first * cos(theta);
    x[1] = first * sin(theta);
  } else if (i == 22) { // Fan
    double t = M_PI * pow(c, 2);
    if (fmod(theta + f, t) > (t / 2)) {
      x[0] = r * cos(theta - (t/2));
      x[1] = r * sin(theta - (t/2));
    } else {
      x[0] = r * cos(theta + (t/2));;
      x[1] = r * sin(theta + (t/2));
    }
  } else if (i == 23) { // Blob
    double first = r * (pparams[1] + ((pparams[0] - pparams[1])/ 2) * (sin(pparams[2] * theta) + 1));
    x[0] = first * cos(theta);
    x[1] = first * sin(theta);
  } else if (i == 24) { // PDJ
    x[0] = sin(pparams[3] * p[1]) - cos(pparams[4] * p[0]);
    x[1] = sin(pparams[5] * p[0]) - cos(pparams[6] * p[1]);
  } else if (i == 25) { // Fan2
    Rcpp::DoubleVector fanresult = variation(p, 22, a, b, c, d, e, f, pparams);
    double p1 = M_PI * pow(fanresult[0], 2);
    double p2 = fanresult[1];
    double t = theta + p2 - p1 * trunc((2 * theta * p2) / p1);
    if (t > (p1 / 2)) {
      x[0] = r * sin(theta - (p1 / 2));
      x[1] = r * cos(theta - (p1 / 2));
    } else {
      x[0] = r * sin(theta + (p1 / 2));
      x[1] = r * cos(theta + (p1 / 2));
    }
  } else if (i == 26) { // Rings2
    double pp = pow(pparams[7], 2);
    double t = r - 2 * pp * trunc((r + pp) / (2 * pp)) + r * (1 - pp);
    x[0] = t * sin(theta);
    x[1] = t * cos(theta);
  } else if (i == 27) { // Eyefish
    x[0] = (2 / (r + 1)) * p[0];
    x[1] = (2 / (r + 1)) * p[1];
  } else if (i == 28) { // Bubble
    x[0] = (4 / (pow(r, 2) + 4)) * p[0];
    x[1] = (4 / (pow(r, 2) + 4)) * p[1];	
  } else if (i == 29) { // Cylinder
    x[0] = sin(p[0]);
    x[1] = p[1];
  } else if (i == 30) { // Perspective
    double first = (pparams[9] / (pparams[9] - p[1] * sin(pparams[8])));
    x[0] = first * p[0];
    x[1] = first * (p[1] * cos(pparams[8]));
  } else if (i == 31) { // Noise
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    x[0] = Psi1 * (p[0] * cos(2 * M_PI * Psi2));
    x[1] = Psi1 * (p[1] * sin(2 * M_PI * Psi2));
  } else if (i == 32) { // JuliaN
    double Psi = R::runif(0, 1);
    double p3 = trunc(fabs(pparams[11]) * Psi);
    double t = (phi + 2 * M_PI * p3) / pparams[11];
    x[0] = pow(r, pparams[11] / pparams[10]) * cos(t);
    x[1] = pow(r, pparams[11] / pparams[10]) * sin(t);
  } else if (i == 33) { // JuliaScope
    double Psi = R::runif(0, 1);
    int Lambda = floor(R::runif(0, 2));
    double p3 = trunc(fabs(pparams[12] * Psi));
    double t = (Lambda * phi + 2 * M_PI * p3) / pparams[12];
    x[0] = pow(r, pparams[13] / pparams[12]) * cos(t);
    x[1] = pow(r, pparams[13] / pparams[12]) * sin(t);
  } else if (i == 34) { // Blur
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    x[0] = Psi1 * cos(2 * M_PI * Psi2);
    x[1] = Psi1 * sin(2 * M_PI * Psi2);
  } else if (i == 35) { // Gaussian
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    double Psi3 = R::runif(0, 1);
    double Psi4 = R::runif(0, 1);
    double Psi5 = R::runif(0, 1);
    x[0] = (Psi1 + Psi2 + Psi3 + Psi4 - 2) * cos(2 * M_PI * Psi5);
    x[1] = (Psi1 + Psi2 + Psi3 + Psi4 - 2) * sin(2 * M_PI * Psi5);
  } else if (i == 36) { // RadialBlur
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    double Psi3 = R::runif(0, 1);
    double Psi4 = R::runif(0, 1);
    double p1 = pparams[14] * (M_PI / 2);
    double t1 = pparams[15] * (Psi1 + Psi2 + Psi3 + Psi4 - 2);
    double t2 = phi + t1 * sin(p1);
    double t3 = t1 * cos(p1) - 1;
    x[0] = (1 / pparams[15]) * (r * cos(t2) + t3 * p[0]);
    x[1] = (1 / pparams[15]) * (r * sin(t2) + t3 * p[1]);
  } else if (i == 37) { // Pie
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    double Psi3 = R::runif(0, 1);
    double t1 = trunc(Psi1 * pparams[16] + 0.5);
    double t2 = pparams[17] + ((2 * M_PI) / pparams[16]) * (t1 + Psi2 * pparams[17]);
    x[0] = Psi3 * cos(t2);
    x[1] = Psi3 * sin(t2);
  } else if (i == 38) { // Ngon
    double t3 = phi - pparams[19] * floor(phi / pparams[19]);
    double t4;
    if (t3 > (pparams[19] / 2)) {
      t4 = t3;
    } else {
      t4 = t3 - pparams[19];
    }
    double k = (pparams[20] * ((1 / cos(t4) - 1) + pparams[21]) + pparams[22]) / pow(r, pparams[18]);
    x[0] = k * p[0];
    x[1] = k * p[1];
  } else if (i == 39) { // Curl
    double t1 = 1 + pparams[23] * p[0] + pparams[24] * (pow(p[0], 2) - pow(p[1], 2));
    double t2 = pparams[23] * p[1] + 2 * pparams[24] * p[0] * p[1];
    double first = 1 / (pow(t1, 2) + pow(t2, 2));
    x[0] = first * (p[0] * t1 + p[1] * t2);
    x[1] = first * (p[1] * t1 - p[0] * t2);
  } else if (i == 40) { // Rectangles
    x[0] = (2 * floor(p[0] / pparams[25]) + 1) * pparams[25] - p[0];
    x[1] = (2 * floor(p[1] / pparams[26]) + 1) * pparams[26] - p[1];
  } else if (i == 41) { // Arch
    double Psi = R::runif(0, 1);
    x[0] = sin(Psi * M_PI * pparams[27]);
    x[1] = pow(sin(Psi * M_PI * pparams[27]), 2) / cos(Psi * M_PI * pparams[27]);
  } else if (i == 42) { // Tangent
    x[0] = sin(p[0]) / cos(p[1]);
    x[1] = tan(p[1]);
  } else if (i == 43) { // Square
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    x[0] = Psi1 - 0.5;
    x[1] = Psi2 - 0.5;
  } else if (i == 44) { // Rays
    double Psi = R::runif(0, 1);
    double first = (pparams[28] * tan(Psi * M_PI * pparams[28])) / pow(r, 2);
    x[0] = first * cos(p[0]);
    x[1] = first * sin(p[0]);
  } else if (i == 45) { // Rays
    double Psi = R::runif(0, 1);
    x[0] = p[0] * (cos(Psi * r * pparams[29]) + sin(Psi * r * pparams[29]));
    x[1] = p[0] * (cos(Psi * r * pparams[29]) - sin(Psi * r * pparams[29]));
  } else if (i == 46) { // Secant
    x[0] = p[0];
    x[1] = 1 / (pparams[30] * cos(pparams[30] * r));
  } else if (i == 47) { // Twintrian
    double Psi = R::runif(0, 1);
    double t = log10(pow(sin(Psi * r * pparams[31]), 2)) + cos(Psi * r * pparams[31]);
    x[0] = p[0] * t;
    x[1] = p[0] * (t - M_PI * sin(Psi * r * pparams[31]));
  } else if (i == 48) { // Cross
    double first = sqrt(1 / pow(pow(p[0], 2) - pow(p[1], 2), 2));
    x[0] = first * p[0];
    x[1] = first * p[1];
  }
  return x;
}

Rcpp::DoubleVector posttransform(Rcpp::DoubleVector p,
                         double alpha,
                         double beta,
                         double gamma,
                         double delta,
                         double epsilon,
                         double zeta) {
  Rcpp::DoubleVector x = p;
  x[0] = alpha * p[0] + beta * p[1] + gamma;
  x[1] = delta * p[0] + epsilon * p[1] + zeta;
  return x;
}

Rcpp::NumericVector get_variation_weights(arma::mat v_ij,
                                          Rcpp::NumericVector i) {
  int n = v_ij.n_cols;
  Rcpp::NumericVector v_j(n);
  for (int iter = 0; iter < n; iter++) {
    v_j[iter] = v_ij(i[0], iter);
  }
  return v_j;
}

Rcpp::DoubleVector update_colors(Rcpp::DoubleVector p,
                                  arma::mat colors,
                                  int i) {
  Rcpp::DoubleVector x = p;
  x[2] = (p[2] + colors(i, 0)) / 2;
  x[3] = (p[3] + colors(i, 1)) / 2;
  x[4] = (p[4] + colors(i, 2)) / 2;
  return x;
}

// [[Rcpp::export]]
Rcpp::DataFrame iterate_flame(int iterations,
                              Rcpp::DoubleVector functions,
                              Rcpp::DoubleVector variations,
                              Rcpp::DoubleVector point,
                              Rcpp::DoubleVector w_i,
                              arma::mat mat_coef,
                              bool blend_variations,
                              arma::mat v_ij,
                              Rcpp::DoubleVector v_params,
                              bool transform_p,
                              arma::mat p_coef,
                              bool transform_f,
                              Rcpp::DoubleVector f_coef,
                              bool transform_e,
                              Rcpp::DoubleVector e_coef,
                              arma::mat colors) {
  int nvariations = variations.length();
  int npoints = (iterations - 20);
  Rcpp::DoubleVector x(npoints);
  Rcpp::DoubleVector y(npoints);
  Rcpp::DoubleVector c1(npoints);
  Rcpp::DoubleVector c2(npoints);
  Rcpp::DoubleVector c3(npoints);
  Rcpp::DoubleVector p(5);
  for (int iter = 0; iter < iterations; iter++) {
    Rcpp::checkUserInterrupt();
    // Pick an affine function to use
    Rcpp::NumericVector i = Rcpp::sample(functions, 1, false, w_i);
    Rcpp::DoubleVector newpoint(5);
    // Apply the affine function to the point
    p = affine(point, mat_coef(i[0], 0), mat_coef(i[0], 1), mat_coef(i[0], 2), mat_coef(i[0], 3), mat_coef(i[0], 4), mat_coef(i[0], 5));
    // Apply the variation(s) to the point
    if (blend_variations) {
      for (int j = 0; j < nvariations; j++) {
        if (v_ij(i[0], j) == 0) {
          continue;
        }
        newpoint += v_ij(i[0], j) * variation(p, variations[j], mat_coef(i[0], 0), mat_coef(i[0], 1), mat_coef(i[0], 2), mat_coef(i[0], 3), mat_coef(i[0], 4), mat_coef(i[0], 5), v_params);
      }
    } else {
      Rcpp::NumericVector v_j = get_variation_weights(v_ij, i[0]);
      Rcpp::NumericVector ch = Rcpp::sample(variations, 1, false, v_j);
      newpoint = variation(p, ch[0], mat_coef(i[0], 0), mat_coef(i[0], 1), mat_coef(i[0], 2), mat_coef(i[0], 3), mat_coef(i[0], 4), mat_coef(i[0], 5), v_params);
    }
    point = newpoint;
    // Update the color of the point
    point = update_colors(point, colors, i[0]);
    // Apply a post transformation
    if (transform_p) {
      point = posttransform(point, p_coef(i[0], 0), p_coef(i[0], 1), p_coef(i[0], 2), p_coef(i[0], 3), p_coef(i[0], 4), p_coef(i[0], 5));
    }
    // Apply a final transformation
    if (transform_f) {
      point = affine(point, f_coef[0], f_coef[1], f_coef[2], f_coef[3], f_coef[4], f_coef[5]);
      // Apply an additional post transformation
      if (transform_e) {
        point = affine(point, e_coef[0], e_coef[1], e_coef[2], e_coef[3], e_coef[4], e_coef[5]);
      }
    }
    if (iter > 19) {
      x[iter - 20] = point[0];
      y[iter - 20] = point[1];
      c1[iter - 20] = point[2];
      c2[iter - 20] = point[3];
      c3[iter - 20] = point[4];
    }
  }
  Rcpp::DataFrame flame = Rcpp::DataFrame::create(Rcpp::Named("x") = x,
                                                  Rcpp::Named("y") = y,
                                                  Rcpp::Named("c1") = c1,
                                                  Rcpp::Named("c2") = c2,
                                                  Rcpp::Named("c3") = c3);
  return flame;
}

int cfi(double v, const Rcpp::NumericVector& x) {
  return std::distance(x.begin(), std::upper_bound(x.begin(), x.end(), v));
}

// [[Rcpp::export]]
arma::cube color_flame(arma::cube canvas,
                       Rcpp::DoubleVector binsx,
                       Rcpp::DoubleVector binsy,
                       Rcpp::DoubleVector x,
                       Rcpp::DoubleVector y,
                       Rcpp::DoubleVector c1,
                       Rcpp::DoubleVector c2,
                       Rcpp::DoubleVector c3) {
  for (int i = 0; i < x.length(); i++) {
    Rcpp::checkUserInterrupt();
    int indx = cfi(x[i], binsx);
    if ((indx == 0) | (indx == binsx.length())) {
      continue;
    }
    int indy = cfi(y[i], binsy);
    if ((indy == 0) | (indy == binsy.length())) {
      continue;
    }
    canvas(indy, indx, 0) = canvas(indy, indx, 0) + 1;
    canvas(indy, indx, 1) = canvas(indy, indx, 1) + c1[i];
    canvas(indy, indx, 2) = canvas(indy, indx, 2) + c2[i];
    canvas(indy, indx, 3) = canvas(indy, indx, 3) + c3[i];
  }
  return canvas;
}
