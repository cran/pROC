# pROC: Tools Receiver operating characteristic (ROC curves) with
# (partial) area under the curve, confidence intervals and comparison. 
# Copyright (C) 2010 Xavier Robin, Alexandre Hainard, Natacha Turck,
# Natalia Tiberti, Frédérique Lisacek, Jean-Charles Sanchez
# and Markus Müller
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Helper functions for the ROC curves. These functions should not be called directly as they peform very specific tasks and do nearly no argument validity checks. Not documented in RD and not exported.

# returns a vector with two elements, sensitivity and specificity, given the threshold at which to evaluate the performance, the values of controls and cases and the direction of the comparison, a character '>' or '<' as controls CMP cases
# sp <- roc.utils.perfs(...)[1,]
# se <- roc.utils.perfs(...)[2,]
roc.utils.perfs <- function(threshold, controls, cases, direction) {
  if (direction == '>') {
    tp <- sum(as.numeric(cases <= threshold))
    tn <- sum(as.numeric(controls > threshold))
  }
  else if (direction == '<') {
    tp <- sum(as.numeric(cases >= threshold))
    tn <- sum(as.numeric(controls < threshold))
  }
  # return(c(sp, se))
  return(c(sp=tn/length(controls), se=tp/length(cases)))
}

# as roc.utils.perfs, but for densities
roc.utils.perfs.dens <- function(threshold, x, dens.controls, dens.cases, direction) {
  if (direction == '>') {
    tp <- sum(dens.cases[x <= threshold])
    tn <- sum(dens.controls[x > threshold])
  }
  else if (direction == '<') {
    tp <- sum(dens.cases[x >= threshold])
    tn <- sum(dens.controls[x < threshold])
  }
  # return(c(sp, se))
  return(c(sp=tn/sum(dens.controls), se=tp/sum(dens.cases)))
}

# return the thresholds to evaluate in the ROC curve, given the 'predictor' values. Returns all unique values of 'predictor' plus 2 extreme values
roc.utils.thresholds <- function(predictor) {
  thresholds <- sort(unique(predictor))
  return((c(-Inf, thresholds) + c(thresholds, +Inf))/2)
}

# Find all the local maximas of the ROC curve. Returns a logical vector
roc.utils.max.thresholds.idx <- function(thresholds, sp, se) {
  if (is.unsorted(sp)) {
    # make sure SP is sorted increasingly, and sort thresholds accordingly
    thresholds <- rev(thresholds)
    sp <- rev(sp)
    se <- rev(se)
  }
  # Find the local maximas
  local.maximas <- ifelse(se[1] > se[2], TRUE, FALSE)
  for (i in 2:(length(thresholds)-1)) {
    if (sp[i] > sp[i-1] & se[i] > se[i+1])
      local.maximas <- c(local.maximas, TRUE)
    else if (sp[i] > sp[i-1] & se[i] == 0)
      local.maximas <- c(local.maximas, TRUE)
    else if (se[i] > se[i-1] & sp[i] == 1)
      local.maximas <- c(local.maximas, TRUE)
    else
      local.maximas <- c(local.maximas, FALSE)
  }
  local.maximas <- c(local.maximas, ifelse(sp[length(thresholds)] > sp[length(thresholds)-1], TRUE, FALSE))
  return(local.maximas)
}

# Define which progress bar to use
roc.utils.get.progress.bar <- function(name = getOption("pROCProgress")$name, title = "Bootstrap", label = "", width = getOption("pROCProgress")$width, char = getOption("pROCProgress")$char, style = getOption("pROCProgress")$style, ...) {
  if (name == "none")
    progress_none()
  else if (name == "text")
    progress_text(char=char, style=style, width=width)
  else if (name == "tk" || name == "win")
    match.fun(paste("progress", name, sep = "_"))(title=title, label=label, width=width)
  else # in the special case someone made a progress_other function
    match.fun(paste("progress", name, sep = "_"))(title=title, label=label, width=width, char=char, style=style)
}

# sort roc curve. Make sure specificities are increasing.
sort.roc <- function(roc) {
  if (is.unsorted(roc$specificities)) {
    roc$sensitivities <- rev(roc$sensitivities)
    roc$specificities <- rev(roc$specificities)
    roc$thresholds <- rev(roc$thresholds)
  }
  return(roc)
}

# sort smoothed roc curve. Make sure specificities are increasing.
sort.smooth.roc <- function(roc) {
  if (is.unsorted(roc$specificities)) {
    roc$sensitivities <- rev(roc$sensitivities)
    roc$specificities <- rev(roc$specificities)
  }
  return(roc)
}

# Mann-Whitney Kernel used by delong.test and ci.auc.delong
MW.kernel <- function(x, y) {
  # x, y: numeric vectors of length 1
  # returns: numeric vectors of length 1
  if (y < x) return(1)
  if (y == x) return(.5)
  if (y > x) return(0)
}
