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

ci.auc <- function(...) {
  UseMethod("ci.auc")
}

ci.auc.formula <- function(formula, data, ...) {
  ci.auc.roc(roc.formula(formula, data, ci=FALSE, ...), ...)
}

ci.auc.default <- function(response, predictor, ...) {
  ci.auc.roc(roc.default(response, predictor, ci=FALSE, ...), ...)
}

ci.auc.auc <- function(auc, ...) {
  ci.auc(attr(auc, "roc"), ...)
}

ci.auc.smooth.roc <- function(smooth.roc,
                   conf.level = 0.95,
                   boot.n = 2000,
                   boot.stratified = TRUE,
                   reuse.auc=TRUE,
                   progress = getOption("pROCProgress")$name,
                   ...
                   ) {
  if (conf.level > 1 | conf.level < 0)
    stop("conf.level must be within the interval [0,1].")

  # We need an auc
  if (is.null(smooth.roc$auc) | !reuse.auc)
    smooth.roc$auc <- auc(smooth.roc, ...)

  # Check if called with density.cases or density.controls
  if (is.null(smooth.roc$smoothing.args) || is.numeric(smooth.roc$smoothing.args$density.cases) || is.numeric(smooth.roc$smoothing.args$density.controls))
    stop("Cannot compute CI of ROC curves smoothed with numeric density.controls and density.cases.")

  # Get the non smoothed roc.
  roc <- attr(smooth.roc, "roc")
  roc$ci <- NULL # remove potential ci in roc to avoid infinite loop with smooth.roc()

  # do all the computations in fraction, re-transform in percent later if necessary
  percent <- smooth.roc$percent
  smooth.roc$percent <- FALSE
  roc$percent <- FALSE
  oldauc <- smooth.roc$auc
  if (percent) {
    attr(smooth.roc$auc, "percent") <- FALSE
    if (! identical(attr(smooth.roc$auc, "partial.auc"), FALSE)) {
      attr(smooth.roc$auc, "partial.auc") <- attr(smooth.roc$auc, "partial.auc") / 100
    }
  }

  # prepare the calls
  smooth.roc.call <- as.call(c(match.fun("smooth.roc"), smooth.roc$smoothing.args))
  auc.args <- attributes(smooth.roc$auc)[grep("partial.auc", names(attributes(smooth.roc$auc)))]
  auc.call <- as.call(c(match.fun("auc.smooth.roc"), auc.args))

  if(class(progress) != "list")
    progress <- roc.utils.get.progress.bar(progress, title="AUC confidence interval", label="Bootstrap in progress...", ...)

  if (boot.stratified) {
    aucs <- unlist(rlply(boot.n, stratified.ci.smooth.auc(roc, smooth.roc.call, auc.call), .progress=progress))
  }
  else {
    aucs <- unlist(rlply(boot.n, nonstratified.ci.smooth.auc(roc, smooth.roc.call, auc.call), .progress=progress))
  }

  if (sum(is.na(aucs)) > 0) {
    warning("NA value(s) produced during bootstrap were ignored.")
    aucs <- aucs[!is.na(aucs)]
  }
  # TODO: Maybe apply a correction (it's in the Tibshirani?) What do Carpenter-Bithell say about that?
  # Prepare the return value
  ci <- quantile(aucs, c(0+(1-conf.level)/2, .5, 1-(1-conf.level)/2))
  if (percent) {
    ci <- ci * 100
    aucs <- aucs * 100
  }
  attr(ci, "conf.level") <- conf.level
  attr(ci, "boot.n") <- boot.n
  attr(ci, "boot.stratified") <- boot.stratified
  attr(ci, "aucs") <- aucs
  attr(ci, "auc") <- oldauc
  class(ci) <- "ci.auc"
  return(ci)  
}

ci.auc.roc <- function(roc,
                   conf.level = 0.95,
                   boot.n = 2000,
                   boot.stratified = TRUE,
                   reuse.auc=TRUE,
                   progress = getOption("pROCProgress")$name,
                   ...
                   ) {
  if (conf.level > 1 | conf.level < 0)
    stop("conf.level must be within the interval [0,1].")

  # We need an auc
  if (is.null(roc$auc) | !reuse.auc)
    roc$auc <- auc(roc, ...)

  # do all the computations in fraction, re-transform in percent later if necessary
  percent <- roc$percent
  roc$percent <- FALSE
  oldauc <- roc$auc
  if (percent) {
    attr(roc$auc, "percent") <- FALSE
    if (! identical(attr(roc$auc, "partial.auc"), FALSE))
      attr(roc$auc, "partial.auc") <- attr(roc$auc, "partial.auc") / 100
  }

  if(class(progress) != "list")
    progress <- roc.utils.get.progress.bar(progress, title="AUC confidence interval", label="Bootstrap in progress...", ...)

  if (boot.stratified) {
    aucs <- unlist(rlply(boot.n, stratified.ci.auc(roc), .progress=progress))
  }
  else {
    aucs <- unlist(rlply(boot.n, nonstratified.ci.auc(roc), .progress=progress))
  }

  if (sum(is.na(aucs)) > 0) {
    warning("NA value(s) produced during bootstrap were ignored.")
    aucs <- aucs[!is.na(aucs)]
  }
  # TODO: Maybe apply a correction (it's in the Tibshirani?) What do Carpenter-Bithell say about that?
  # Prepare the return value
  ci <- quantile(aucs, c(0+(1-conf.level)/2, .5, 1-(1-conf.level)/2))
  if (percent) {
    ci <- ci * 100
    aucs <- aucs * 100
  }
  attr(ci, "conf.level") <- conf.level
  attr(ci, "boot.n") <- boot.n
  attr(ci, "boot.stratified") <- boot.stratified
  attr(ci, "aucs") <- aucs
  attr(ci, "auc") <- oldauc
  class(ci) <- "ci.auc"
  return(ci)
}

# Returns an auc in a stratified manner
stratified.ci.auc <- function(roc) {
  controls <- sample(roc$controls, replace=TRUE)
  cases <- sample(roc$cases, replace=TRUE)
  thresholds <- roc.utils.thresholds(c(cases, controls))
  
  perfs <- sapply(thresholds, roc.utils.perfs, controls=controls, cases=cases, direction=roc$direction)
  roc$sensitivities <- perfs[2,]
  roc$specificities <- perfs[1,]

  auc.roc(roc, partial.auc=attr(roc$auc, "partial.auc"), partial.auc.focus=attr(roc$auc, "partial.auc.focus"), partial.auc.correct=attr(roc$auc, "partial.auc.correct"))
}

# Returns an auc in a non stratified manner
nonstratified.ci.auc <- function(roc) {
  tmp.idx <- sample(1:length(roc$predictor), replace=TRUE)
  predictor <- roc$predictor[tmp.idx]
  response <- roc$response[tmp.idx]
  splitted <- split(predictor, response)
  controls <- splitted[[as.character(roc$levels[1])]]
  cases <- splitted[[as.character(roc$levels[2])]]
  thresholds <- roc.utils.thresholds(predictor)

  perfs <- sapply(thresholds, roc.utils.perfs, controls=controls, cases=cases, direction=roc$direction)
  roc$sensitivities <- perfs[2,]
  roc$specificities <- perfs[1,]
  
  auc.roc(roc, partial.auc=attr(roc$auc, "partial.auc"), partial.auc.focus=attr(roc$auc, "partial.auc.focus"), partial.auc.correct=attr(roc$auc, "partial.auc.correct"))
}

# Returns a smoothed auc in a stratified manner
stratified.ci.smooth.auc <- function(roc, smooth.roc.call, auc.call) {
  controls <- sample(roc$controls, replace=TRUE)
  cases <- sample(roc$cases, replace=TRUE)
  # need to rebuild a ROC and smooth it
  thresholds <- roc.utils.thresholds(c(cases, controls))
  
  perfs <- sapply(thresholds, roc.utils.perfs, controls=controls, cases=cases, direction=roc$direction)

  # update ROC
  roc$sensitivities <- perfs[2,]
  roc$specificities <- perfs[1,]
  roc$cases <- cases
  roc$controls <- controls
  roc$predictor <- c(controls, cases)
  roc$response <- c(rep(roc$levels[1], length(controls)), rep(roc$levels[2], length(cases)))
  roc$thresholds <- thresholds

  # call smooth.roc and auc.smooth.roc
  smooth.roc.call$roc <- roc
  auc.call$smooth.roc <- try(eval(smooth.roc.call))
  if (class(auc.call$smooth.roc) == "try-error")
    return(NA)
  return(eval(auc.call))
}

# Returns a smoothed auc in a non stratified manner
nonstratified.ci.smooth.auc <- function(roc, smooth.roc.call, auc.call) {
  tmp.idx <- sample(1:length(roc$predictor), replace=TRUE)
  predictor <- roc$predictor[tmp.idx]
  response <- roc$response[tmp.idx]
  splitted <- split(predictor, response)
  controls <- splitted[[as.character(roc$levels[1])]]
  cases <- splitted[[as.character(roc$levels[2])]]
  thresholds <- roc.utils.thresholds(predictor)

  perfs <- sapply(thresholds, roc.utils.perfs, controls=controls, cases=cases, direction=roc$direction)

  # update ROC
  roc$sensitivities <- perfs[2,]
  roc$specificities <- perfs[1,]
  roc$cases <- cases
  roc$controls <- controls
  roc$predictor <- predictor
  roc$response <- response
  roc$thresholds <- thresholds

  # call smooth.roc and auc.smooth.roc
  smooth.roc.call$roc <- roc
  auc.call$smooth.roc <- try(eval(smooth.roc.call))
  if (class(auc.call$smooth.roc) == "try-error")
    return(NA)
  return(eval(auc.call))
}
