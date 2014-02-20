#pragma once

#include <Rcpp.h>
#include <vector>
#include <string>

class ROC {
  std::vector<double> controls, cases;
  std::vector<double> sensitivity, specificity;
  static ROC RocFromRList(Rcpp::List);
  double p_auc();
  double full_auc();
  
  public:
    ROC(): {}
    auc(bool partial = false, double from = 0.9, double to = 1, string focus = "specificity", bool correct = false) {
      if (partial) return pauc(from, to, focus, correct);
      else full_auc();
    }
};

