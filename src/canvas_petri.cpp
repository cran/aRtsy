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
Rcpp::IntegerVector get_closest_node(const Rcpp::DoubleVector& attractor_x,
                                     const Rcpp::DoubleVector& attractor_y,
                                     const Rcpp::DoubleVector& nodes_x,
                                     const Rcpp::DoubleVector& nodes_y,
                                     const double& attraction_distance) {
  const int n_attractors = attractor_x.length(), n_nodes = nodes_x.length();
  Rcpp::IntegerVector nodes;
  for (int i = 0; i < n_attractors; ++i) {
    Rcpp::checkUserInterrupt();
    int min_dist_node = 0;
    double min_dist = attraction_distance;
    for (int j = 0; j < n_nodes; ++j) {
      const double dist = fabs(sqrt(pow(attractor_x[i] - nodes_x[j], 2) + pow(attractor_y[i] - nodes_y[j], 2)));
      if (dist < min_dist) {
        min_dist = dist;
        min_dist_node = j + 1;
      }
    }
    nodes.insert(i, min_dist_node);
  }
  return nodes;
}

// [[Rcpp::export]]
Rcpp::DataFrame kill_attractors(Rcpp::DoubleVector& attractor_x,
                                Rcpp::DoubleVector& attractor_y,
                                Rcpp::DoubleVector& nodes_x,
                                Rcpp::DoubleVector& nodes_y,
                                const double& kill_distance) {
  const int n_attractors = attractor_x.length(), n_nodes = nodes_x.length();
  Rcpp::DoubleVector new_attractor_x, new_attractor_y;
  for (int i = 0; i < n_attractors; ++i) {
    Rcpp::checkUserInterrupt();
    bool kill = false;
    for (int j = 0; j < n_nodes; ++j) {
      const double dist = fabs(sqrt(pow(attractor_x[i] - nodes_x[j], 2) + pow(attractor_y[i] - nodes_y[j], 2)));
      if (dist <= kill_distance) {
        kill = true;
        break;
      }
    }
    if (!kill) {
      new_attractor_x.push_back(attractor_x[i]);
      new_attractor_y.push_back(attractor_y[i]);
    }
  }
  Rcpp::DataFrame attractors = Rcpp::DataFrame::create(Rcpp::Named("x") = new_attractor_x,
                                                       Rcpp::Named("y") = new_attractor_y);
  return attractors;
}

// [[Rcpp::export]]
Rcpp::DataFrame draw_circle(const double& center_x,
                            const double& center_y,
                            const double& diameter,
                            const int& n) {
  const double r = diameter / 2, twopidivn = 2 * M_PI / n;
  double t = 0;
  Rcpp::DoubleVector x(n), y(n);
  for (int i = 0; i < n; ++i) {
    if (i % (n / 10) == 0) {
      Rcpp::checkUserInterrupt();
    }
    x[i] = center_x + r * cos(t);
    y[i] = center_y + r * sin(t);
    t += twopidivn;
  }
  Rcpp::DataFrame circle = Rcpp::DataFrame::create(Rcpp::Named("x") = x,
                                                   Rcpp::Named("y") = y);
  return circle;
}

// [[Rcpp::export]]
Rcpp::DataFrame get_directions(Rcpp::DoubleVector& attractor_x,
                               Rcpp::DoubleVector& attractor_y,
                               Rcpp::DoubleVector& nodes_x,
                               Rcpp::DoubleVector& nodes_y,
                               Rcpp::IntegerVector& closest_nodes) {
  const int n = nodes_x.length();
  Rcpp::DoubleVector directionx(n), directiony(n);
  for (int i = 0; i < n; ++i) {
    Rcpp::checkUserInterrupt();
    const Rcpp::LogicalVector indexes = (closest_nodes == (i + 1));
    const Rcpp::DoubleVector& nodex = attractor_x[indexes];
    directionx[i] = mean(nodex - nodes_x[i]);
    const Rcpp::DoubleVector& nodey = attractor_y[indexes];
    directiony[i] = mean(nodey - nodes_y[i]);
  }
  Rcpp::DataFrame X = Rcpp::DataFrame::create(Rcpp::Named("xend") = directionx,
                                              Rcpp::Named("yend") = directiony);
  return X;
}
