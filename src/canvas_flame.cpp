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

void transform(double &x,
               double &y,
               const double a,
               const double b,
               const double c,
               const double d,
               const double e,
               const double f) {
  double newx = a * x + b * y + c;
  double newy = d * x + e * y + f;
  x = newx;
  y = newy;
}

void variation(double &x,
               double &y,
               const int i,
               const double a,
               const double b,
               const double c,
               const double d,
               const double e,
               const double f,
               const Rcpp::DoubleVector pparams) {;
  double newx, newy;
  if (i == 0) { // Linear
    newx = x;
    newy = y;
  } else if (i == 1) { // Sine
    newx = sin(x);
    newy = sin(y);
  } else if (i == 2) { // Sperical
    double r = sqrt(pow(x, 2) + pow(y, 2));
    newx = x / pow(r, 2);
    newy = y / pow(r, 2);
  } else if (i == 3) { // Swirl
    double r = sqrt(pow(x, 2) + pow(y, 2));
    newx = x * sin(pow(r, 2)) - y * cos(pow(r, 2));
    newy = x * cos(pow(r, 2)) + y * sin(pow(r, 2));
  } else if (i == 4) { // Horsehoe
    double r = sqrt(pow(x, 2) + pow(y, 2));
    newx = (1 / r) * ((x - y) * (x + y));
    newy = (1 / r) * (2 * x * y);
  } else if (i == 5) { // Polar
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    newx = theta / M_PI;
    newy = r - 1;
  } else if (i == 6) { // Handkerchief
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    newx = r * sin(theta + r);
    newy = r * cos(theta - r);
  } else if (i == 7) { // Heart
    double theta = atan(x / y);
    double r = sqrt(pow(x, 2) + pow(y, 2));
    newx = r * sin(theta * r);
    newy = r * (-cos(theta * r));
  } else if (i == 8) { // Disc
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    newx = theta / M_PI * sin(M_PI * r);
    newy = theta / M_PI * cos(M_PI * r);
  } else if (i == 9) { // Spiral
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    newx = 1 / r * (cos(theta) + sin(r));
    newy = 1 / r * (sin(theta) + cos(r));
  } else if (i == 10) { // Hyperbolic
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    newx = sin(theta) / r;
    newy = r * cos(theta);
  } else if (i == 11) { // Diamond
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    newx = sin(theta) * cos(r);
    newy = cos(theta) * sin(r);
  } else if (i == 12) { // Ex
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    double p0 = sin(theta + r);
    double p1 = cos(theta - r);
    newx = r * (pow(p0, 3) + pow(p1, 3));
    newy = r * (pow(p0, 3) - pow(p1, 3));
  } else if (i == 13) { // Julia
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    double s = R::runif(0, 1);
    double Omega;
    if (s < 0.5) {
      Omega = 0;
    } else {
      Omega = M_PI;
    }
    newx = sqrt(r) * cos(theta / 2 + Omega);
    newy = sqrt(r) * sin(theta / 2 + Omega);
  } else if (i == 14) { // Bent
    if ((x >= 0) && (y >= 0)) {
      newx = x;
      newy = y;
    } else if ((x < 0) && (y >= 0)) {
      newx = 2 * x;
      newy = y;
    } else if ((x >= 0) && (y < 0)) {
      newx = x;
      newy = y / 2;
    } else if ((x < 0) && (y < 0)) {
      newx = 2 * x;
      newy = y / 2;
    }
  } else if (i == 15) { // Waves
    newx = x + b * sin(y / pow(c, 2));
    newy = y + e * sin(x / pow(f, 2));
  } else if (i == 16) { // Fisheye
    double r = sqrt(pow(x, 2) + pow(y, 2));
    newx = (2 / (r + 1)) * y;
    newy = (2 / (r + 1)) * x;
  } else if (i == 17) { // Popcorn
    newx = x + c * sin(tan(3 * y));
    newy = y + f * sin(tan(3 * x));
  } else if (i == 18) { // Exponential
    newx = exp(x - 1) * cos(M_PI * y);
    newy = exp(x - 1) * sin(M_PI * y);
  } else if (i == 19) { // Power
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    newx = pow(r, sin(theta)) * cos(theta);
    newy = pow(r, sin(theta)) * sin(theta);
  } else if (i == 20) { // Cosine
    newx = cos(M_PI * x) * cosh(y);
    newy = -(sin(M_PI * x) * sinh(y));
  } else if (i == 21) { // Rings
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    double first = fmod(r + pow(c, 2), 2 * pow(c, 2)) - pow(c, 2) + r * (1 - pow(c, 2));
    newx = first * cos(theta);
    newy = first * sin(theta);
  } else if (i == 22) { // Fan
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    double t = M_PI * pow(c, 2);
    if (fmod(theta + f, t) > (t / 2)) {
      newx = r * cos(theta - (t/2));
      newy = r * sin(theta - (t/2));
    } else {
      newx = r * cos(theta + (t/2));;
      newy = r * sin(theta + (t/2));
    }
  } else if (i == 23) { // Blob
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    double first = r * (pparams[1] + ((pparams[0] - pparams[1])/ 2) * (sin(pparams[2] * theta) + 1));
    newx = first * cos(theta);
    newy = first * sin(theta);
  } else if (i == 24) { // PDJ
    newx = sin(pparams[3] * y) - cos(pparams[4] * x);
    newy = sin(pparams[5] * x) - cos(pparams[6] * y);
  } else if (i == 25) { // Fan2
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    variation(x, y, 22, a, b, c, d, e, f, pparams);
    double p1 = M_PI * pow(x, 2);
    double p2 = y;
    double t = theta + p2 - p1 * trunc((2 * theta * p2) / p1);
    if (t > (p1 / 2)) {
      newx = r * sin(theta - (p1 / 2));
      newy = r * cos(theta - (p1 / 2));
    } else {
      newx = r * sin(theta + (p1 / 2));
      newy = r * cos(theta + (p1 / 2));
    }
  } else if (i == 26) { // Rings2
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double theta = atan(x / y);
    double pp = pow(pparams[7], 2);
    double t = r - 2 * pp * trunc((r + pp) / (2 * pp)) + r * (1 - pp);
    newx = t * sin(theta);
    newy = t * cos(theta);
  } else if (i == 27) { // Eyefish
    double r = sqrt(pow(x, 2) + pow(y, 2));
    newx = (2 / (r + 1)) * x;
    newy = (2 / (r + 1)) * y;
  } else if (i == 28) { // Bubble
    double r = sqrt(pow(x, 2) + pow(y, 2));
    newx = (4 / (pow(r, 2) + 4)) * x;
    newy = (4 / (pow(r, 2) + 4)) * y;	
  } else if (i == 29) { // Cylinder
    newx = sin(x);
    newy = y;
  } else if (i == 30) { // Perspective
    double first = (pparams[9] / (pparams[9] - y * sin(pparams[8])));
    newx = first * x;
    newy = first * (y * cos(pparams[8]));
  } else if (i == 31) { // Noise
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    newx = Psi1 * (x * cos(2 * M_PI * Psi2));
    newy = Psi1 * (y * sin(2 * M_PI * Psi2));
  } else if (i == 32) { // JuliaN
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double phi = atan(y / x);
    double Psi = R::runif(0, 1);
    double p3 = trunc(fabs(pparams[11]) * Psi);
    double t = (phi + 2 * M_PI * p3) / pparams[11];
    newx = pow(r, pparams[11] / pparams[10]) * cos(t);
    newy = pow(r, pparams[11] / pparams[10]) * sin(t);
  } else if (i == 33) { // JuliaScope
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double phi = atan(y / x);
    double Psi = R::runif(0, 1);
    int Lambda = floor(R::runif(0, 2));
    double p3 = trunc(fabs(pparams[12] * Psi));
    double t = (Lambda * phi + 2 * M_PI * p3) / pparams[12];
    newx = pow(r, pparams[13] / pparams[12]) * cos(t);
    newy = pow(r, pparams[13] / pparams[12]) * sin(t);
  } else if (i == 34) { // Blur
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    newx = Psi1 * cos(2 * M_PI * Psi2);
    newy = Psi1 * sin(2 * M_PI * Psi2);
  } else if (i == 35) { // Gaussian
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    double Psi3 = R::runif(0, 1);
    double Psi4 = R::runif(0, 1);
    double Psi5 = R::runif(0, 1);
    newx = (Psi1 + Psi2 + Psi3 + Psi4 - 2) * cos(2 * M_PI * Psi5);
    newy = (Psi1 + Psi2 + Psi3 + Psi4 - 2) * sin(2 * M_PI * Psi5);
  } else if (i == 36) { // RadialBlur
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double phi = atan(y / x);
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    double Psi3 = R::runif(0, 1);
    double Psi4 = R::runif(0, 1);
    double p1 = pparams[14] * (M_PI / 2);
    double t1 = pparams[15] * (Psi1 + Psi2 + Psi3 + Psi4 - 2);
    double t2 = phi + t1 * sin(p1);
    double t3 = t1 * cos(p1) - 1;
    newx = (1 / pparams[15]) * (r * cos(t2) + t3 * x);
    newy = (1 / pparams[15]) * (r * sin(t2) + t3 * y);
  } else if (i == 37) { // Pie
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    double Psi3 = R::runif(0, 1);
    double t1 = trunc(Psi1 * pparams[16] + 0.5);
    double t2 = pparams[17] + ((2 * M_PI) / pparams[16]) * (t1 + Psi2 * pparams[17]);
    newx = Psi3 * cos(t2);
    newy = Psi3 * sin(t2);
  } else if (i == 38) { // Ngon
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double phi = atan(y / x);
    double t3 = phi - pparams[19] * floor(phi / pparams[19]);
    double t4;
    if (t3 > (pparams[19] / 2)) {
      t4 = t3;
    } else {
      t4 = t3 - pparams[19];
    }
    double k = (pparams[20] * ((1 / cos(t4) - 1) + pparams[21]) + pparams[22]) / pow(r, pparams[18]);
    newx = k * x;
    newy = k * y;
  } else if (i == 39) { // Curl
    double t1 = 1 + pparams[23] * x + pparams[24] * (pow(x, 2) - pow(y, 2));
    double t2 = pparams[23] * y + 2 * pparams[24] * x * y;
    double first = 1 / (pow(t1, 2) + pow(t2, 2));
    newx = first * (x * t1 + y * t2);
    newy = first * (y * t1 - x * t2);
  } else if (i == 40) { // Rectangles
    newx = (2 * floor(x / pparams[25]) + 1) * pparams[25] - x;
    newy = (2 * floor(y / pparams[26]) + 1) * pparams[26] - y;
  } else if (i == 41) { // Arch
    double Psi = R::runif(0, 1);
    newx = sin(Psi * M_PI * pparams[27]);
    newy = pow(sin(Psi * M_PI * pparams[27]), 2) / cos(Psi * M_PI * pparams[27]);
  } else if (i == 42) { // Tangent
    newx = sin(x) / cos(y);
    newy = tan(y);
  } else if (i == 43) { // Square
    double Psi1 = R::runif(0, 1);
    double Psi2 = R::runif(0, 1);
    newx = Psi1 - 0.5;
    newy = Psi2 - 0.5;
  } else if (i == 44) { // Rays
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double Psi = R::runif(0, 1);
    double first = (pparams[28] * tan(Psi * M_PI * pparams[28])) / pow(r, 2);
    newx = first * cos(x);
    newy = first * sin(x);
  } else if (i == 45) { // Rays
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double Psi = R::runif(0, 1);
    newx = x * (cos(Psi * r * pparams[29]) + sin(Psi * r * pparams[29]));
    newy = x * (cos(Psi * r * pparams[29]) - sin(Psi * r * pparams[29]));
  } else if (i == 46) { // Secant
    double r = sqrt(pow(x, 2) + pow(y, 2));
    newx = x;
    newy = 1 / (pparams[30] * cos(pparams[30] * r));
  } else if (i == 47) { // Twintrian
    double r = sqrt(pow(x, 2) + pow(y, 2));
    double Psi = R::runif(0, 1);
    double t = log10(pow(sin(Psi * r * pparams[31]), 2)) + cos(Psi * r * pparams[31]);
    newx = x * t;
    newy = x * (t - M_PI * sin(Psi * r * pparams[31]));
  } else if (i == 48) { // Cross
    double first = sqrt(1 / pow(pow(x, 2) - pow(y, 2), 2));
    newx = first * x;
    newy = first * y;
  }
  x = newx;
  y = newy;
}

// [[Rcpp::export]]
arma::cube iterate_flame(arma::cube canvas,
                         int iterations,
                         int resolution,
                         int edge,
                         bool blend,
                         bool weighted,
                         bool post,
                         bool final,
                         bool extra,
                         arma::mat colors,
                         Rcpp::DoubleVector functions,
                         Rcpp::DoubleVector funcWeights,
                         arma::mat funcPars,
                         Rcpp::DoubleVector variations,
                         arma::mat varWeights,
                         Rcpp::DoubleVector varParams,
                         arma::mat postPars,
                         Rcpp::DoubleVector finalPars,
                         Rcpp::DoubleVector extraPars) {
  int i, j, indx, indy, nvar = variations.length(), nfunc = functions.length();
  double xc, yc, xp, yp, x = R::runif(-1, 1), y = R::runif(-1, 1), c1 = R::runif(0, 1), c2 = R::runif(0, 1), c3 = R::runif(0, 1);
  bool vary = !((nvar == 1) && (variations[0] == 0));
  for (int iter = 1; iter < iterations; iter++) {
    if ((iter % 100) == 0) {
      Rcpp::checkUserInterrupt();
    }
    // Pick an affine function to use according to their weights
    if (weighted) {
      i = Rcpp::sample(functions, 1, false, funcWeights)[0];
    } else {
      i = floor(R::runif(0, nfunc));
    }
    // Apply the affine function to the current point
    transform(x, y, funcPars(i, 0), funcPars(i, 1), funcPars(i, 2), funcPars(i, 3), funcPars(i, 4), funcPars(i, 5));
    if (vary) {
      // Apply the variation(s) to the point
      if (blend) {
        xc = 0, yc = 0;
        for (int j = 0; j < nvar; j++) {
          xp = x, yp = y;
          variation(xp, yp, variations[j], funcPars(i, 0), funcPars(i, 1), funcPars(i, 2), funcPars(i, 3), funcPars(i, 4), funcPars(i, 5), varParams);
          xc += varWeights(i, j) * xp;
          yc += varWeights(i, j) * yp;
        }
        x = xc, y = yc;
      } else {
        if (weighted) {
          j = Rcpp::sample(variations, 1, false, Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(varWeights.row(i))))[0];
        } else {
          j = floor(R::runif(0, nvar));
        }
        variation(x, y, variations[j], funcPars(i, 0), funcPars(i, 1), funcPars(i, 2), funcPars(i, 3), funcPars(i, 4), funcPars(i, 5), varParams);
      }
    }
    // Apply a post transformation
    if (post) {
      transform(x, y, postPars(i, 0), postPars(i, 1), postPars(i, 2), postPars(i, 3), postPars(i, 4), postPars(i, 5));
    }
    // Apply a final transformation
    if (final) {
      transform(x, y, finalPars[0], finalPars[1], finalPars[2], finalPars[3], finalPars[4], finalPars[5]);
      // Apply an additional post transformation
      if (extra) {
        transform(x, y, extraPars[0], extraPars[1], extraPars[2], extraPars[3], extraPars[4], extraPars[5]);
      }
    }
    // Update color channels for the current iteration
    c1 = (c1 + colors(i, 0)) / 2;
    c2 = (c2 + colors(i, 1)) / 2;
    c3 = (c3 + colors(i, 2)) / 2;
    // Color the four channels
    if (iter > 20) {
      indx = (x * resolution / (2 * edge)) + resolution / 2;
      if ((indx > 0) && (indx < resolution)) {
        indy = (y * resolution / (2 * edge)) + resolution / 2;
        if ((indy > 0) && (indy < resolution)) {
          ++canvas(indx, indy, 0);
          canvas(indx, indy, 1) = canvas(indx, indy, 1) + c1;
          canvas(indx, indy, 2) = canvas(indx, indy, 2) + c2;
          canvas(indx, indy, 3) = canvas(indx, indy, 3) + c3;
        }
      }
    }
  }
  return canvas;
}
