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

roc.test <- function(...) {
  UseMethod("roc.test")
}

roc.test.formula <- function (formula, data, ...){
  cl <- match.call()
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  term.labels <- attr(attr(m, "terms"), "term.labels")
  response <- model.extract(m, "response")
  
  if (length(term.labels) != 2) {
    stop("Invalid formula: exactly 2 predictors are required in a formula of type response~predictor1+predictor2.")
  }
  if (length(response) == 0) {
    stop("Error in the formula: a response is required in a formula of type response~predictor1+predictor2.")
  }

  testres <- roc.test.default(response, m[[term.labels[1]]], m[[term.labels[2]]], ...)
  testres$call <- cl
  testres$data.names <- paste(term.labels, collapse=" and ")
  return(testres)
}

roc.test.default <- function(response, predictor1, predictor2=NULL, na.rm=TRUE, smooth.method=c("binormal", "density", "fitdistr"), ...) {
  if (is.matrix(predictor1) | is.data.frame(predictor1)) {
    if (!is.null(predictor2))
      stop("Predictor2 must not be specified if predictor1 is a matrix or a data.frame.")
    if (dim(predictor1)[2] == 2 & length(response) == dim(predictor1)[1]) {
      roc1 <- roc(response, predictor1[,1], ...)
      roc2 <- roc(response, predictor1[,2], ...)
      if (is.data.frame(predictor1))
        data.names <- paste(names(predictor1), collapse=" and ")
      else
        data.names <- paste(deparse(substitute(predictor1[,1])), "and", deparse(substitute(predictor1[,2])))
    }
    else {
      stop("Wrong dimension for predictor1 as a matrix or a data.frame.")
    }
  }
  else {
    if (missing(predictor2))
      stop("Missing argument predictor2 with predictor1 as a vector.")
    # Need to remove NAs
    if (na.rm) {
      nas <- is.na(response) | is.na(predictor1) | is.na(predictor2)
      response <- response[!nas]
      predictor1 <- predictor1[!nas]
      predictor2 <- predictor2[!nas]
    }
    roc1 <- roc(response, predictor1, method=smooth.method, ...)
    roc2 <- roc(response, predictor2, method=smooth.method, ...)
    call <- match.call()
    data.names <- paste(deparse(call$predictor1), "and", deparse(call$predictor2))
  }
  test <- roc.test.roc(roc1, roc2, ...)
  test$data.names <- data.names
  return(test)
}

roc.test.auc <- function(roc1, roc2, ...) {
  testres <- roc.test.roc(attr(roc1, "roc"), roc2, ...)
  testres$call <- match.call()
  testres$data.names <- paste(deparse(substitute(roc1)), "and", deparse(substitute(roc2)))
  return(testres)
}

roc.test.smooth.roc <- function(roc1, roc2, ...) {
  testres <- roc.test.roc(roc1, roc2, ...)
  testres$call <- match.call()
  testres$data.names <- paste(deparse(substitute(roc1)), "and", deparse(substitute(roc2)))
  return(testres)
}

roc.test.roc <- function(roc1, roc2, method=c("delong", "bootstrap"), alternative = c("two.sided", "less", "greater"), reuse.auc=TRUE, boot.n=2000, boot.stratified=TRUE, progress=getOption("pROCProgress")$name, ...) {
  alternative <- match.arg(alternative)
  data.names <- paste(deparse(substitute(roc1)), "and", deparse(substitute(roc2)))
  if (class(roc2) == "auc")
    roc2 <- attr(roc2, "roc")

  # store which objects are smoothed, and how
  smoothing.args <- list()
  if (class(roc1) == "smooth.roc") {
    smoothing.args$roc1 <- roc1$smoothing.args
    smoothing.args$roc1$smooth <- TRUE
    oroc1 <- roc1
    roc1 <- attr(roc1, "roc")
    roc1$auc <- oroc1$auc
  }
  else {
    smoothing.args$roc1 <- list(smooth=FALSE)
    oroc1 <- roc1
  }
  if (class(roc2) == "smooth.roc") {
    smoothing.args$roc2 <- roc2$smoothing.args
    smoothing.args$roc2$smooth <- TRUE
    oroc2 <- roc2
    roc2 <- attr(roc2, "roc")
    roc2$auc <- oroc2$auc
  }
  else {
    smoothing.args$roc2 <- list(smooth=FALSE)
    oroc2 <- roc2
  }
  # check that responses of roc 1 and 2 are identical
  if (!identical(roc1$response, roc2$response)) {
    # Make sure the difference is not only due to missing values
    # restore original data (with NAs)
    res1 <- roc1$response
    if (any(attr(res1, "na.action"))) {
      res1.with.nas <- rep(NA, length(res1) + length(attr(res1, "na.action")))
      res1.with.nas[-attr(res1, "na.action")] <- res1
      attributes(res1.with.nas) <- attributes(res1)
      attr(res1.with.nas, "na.action") <- NULL
    }
    else {
      res1.with.nas <- res1 <- roc1$response
    }
    res2 <- roc2$response
    if (any(attr(res2, "na.action"))) {
      res2.with.nas <- rep(NA, length(res2) + length(attr(res2, "na.action")))
      res2.with.nas[-attr(res2, "na.action")] <- res2
      attributes(res2.with.nas) <- attributes(res2)
      attr(res2.with.nas, "na.action") <- NULL
    }
    else {
      res2.with.nas <- res2 <- roc2$response
    }
    # re-remove NAs
    nas <- is.na(res1.with.nas) | is.na(res2.with.nas)
    # re-check identity
    if (identical(res1.with.nas[!nas], res2.with.nas[!nas])) {
      # If identical, we still need to remove the NAs and re-compute the ROC curves
      if (any(attr(res1, "na.action"))) {
        predictor1.with.nas <- rep(NA, length(res1) + length(attr(res1, "na.action")))
        predictor1.with.nas[-attr(res1, "na.action")] <- roc1$predictor
        attributes(predictor1.with.nas) <- attributes(roc1$predictor)
        attr(predictor1.with.nas, "na.action") <- NULL
      }
      else {
        predictor1.with.nas <- roc1$predictor
      }
      roc1.without.nas <- roc(res1.with.nas[!nas], predictor1.with.nas[!nas], levels=roc1$levels, percent=roc1$percent, na.rm=FALSE, direction=roc1$direction, auc=FALSE)
      if (!is.null(roc1$auc))
        roc1.without.nas$auc <- auc(roc1.without.nas, partial.auc=attr(roc1$auc, "partial.auc"), partial.auc.focus=attr(roc1$auc, "partial.auc.focus"), partial.auc.correct=attr(roc1$auc, "partial.auc.correct"))
      
      if (any(attr(res2, "na.action"))) {
        predictor2.with.nas <- rep(NA, length(res2) + length(attr(res2, "na.action")))
        predictor2.with.nas[-attr(res2, "na.action")] <- roc2$predictor
        attributes(predictor2.with.nas) <- attributes(roc2$predictor)
        attr(predictor2.with.nas, "na.action") <- NULL
      }
      else {
        predictor2.with.nas <- roc2$predictor
      }
      roc2.without.nas <- roc(res2.with.nas[!nas], predictor2.with.nas[!nas], levels=roc2$levels, percent=roc2$percent, na.rm=FALSE, direction=roc2$direction, auc=FALSE)
      if (!is.null(roc2$auc))
        roc2.without.nas$auc <- auc(roc2.without.nas, partial.auc=attr(roc2$auc, "partial.auc"), partial.auc.focus=attr(roc2$auc, "partial.auc.focus"), partial.auc.correct=attr(roc2$auc, "partial.auc.correct"))

      # replace the old roc by the new ones
      roc1 <- roc1.without.nas
      roc2 <- roc2.without.nas
    }
    else {
      stop("The ROC test is defined only on paired ROC curves")
    }
  }

  # check that the AUC was computed, or do it now
  if (is.null(roc1$auc) | !reuse.auc) {
    if (smoothing.args$roc1$smooth)
      roc1$auc <- auc(do.call("smooth.roc", c(list(roc=roc1), smoothing.args$roc1)), ...)
    else
      roc1$auc <- auc(roc1, ...)
  }
  if (is.null(roc2$auc) | !reuse.auc) {
    if (smoothing.args$roc2$smooth)
      roc2$auc <- auc(do.call("smooth.roc", c(list(roc=roc2), smoothing.args$roc2)), ...)
    else
      roc2$auc <- auc(roc2, ...)
  }
    
  # check that the full AUC was requested in auc. Otherwise, issue a warning
  if (!identical(attributes(roc1$auc)[names(attributes(roc1$auc))!="roc"], attributes(roc2$auc)[names(attributes(roc2$auc))!="roc"]))
    warning("Different AUC specifications in the ROC curves. Enforcing the inconsistency, but unexpected results may be produced.")
  # check that the full AUC was requested in auc. Otherwise, issue a warning
  if (!identical(smoothing.args$roc1, smoothing.args$roc2))
    warning("Different smoothing parameters in the ROC curves. Enforcing the inconsistency, but unexpected results may be produced.")

  # Check the method
  if (missing(method) | is.null(method)) {
    # determine method if missing
    if (is.numeric(attr(roc1$auc, "partial.auc")) && length(attr(roc1$auc, "partial.auc") == 2)) {
      # partial auc: go for bootstrap
      method <- "bootstrap"
    }
    else if (smoothing.args$roc1$smooth || smoothing.args$roc2$smooth) {
      # smoothing in one or both: bootstrap
      method <- "bootstrap"
    }
    else if (roc1$direction != roc2$direction) {
      # delong doesn't work well with opposite directions (will report high significance if roc1$auc and roc2$auc are similar and high)
      method <- "bootstrap"
    }
    else {
      method <- "delong"
    }
  }
  else {
    method <- match.arg(method, c("delong", "bootstrap"))
    # delong NA to pAUC: warn + change
    if (is.numeric(attr(roc1$auc, "partial.auc")) & length(attr(roc1$auc, "partial.auc") == 2) & method == "delong") {
      warning("Using DeLong's test for partial AUC is not supported. Using bootstrap test instead.")
      method <- "bootstrap"
    }
    else if (smoothing.args$roc1$smooth || smoothing.args$roc2$smooth) {
      warning("Using DeLong's test for smoothed ROCs is not supported. Using bootstrap test instead.")
      method <- "bootstrap"
    }
    if (roc1$direction != roc2$direction & method == "delong")
      warning("DeLong's test should not be applied to ROC curves with a different direction.")
  }

  # Prepare the return value htest
  estimate <- c(roc1$auc, roc2$auc)
  if (identical(attr(roc1$auc, "partial.auc"), FALSE)) {
    nest <- paste(ifelse(smoothing.args$roc1$smooth, "Smoothed ", ""), "AUC of roc1", sep="")
  }
  else {
    nest <- paste(ifelse (attr(roc1$auc, "partial.auc.correct"), "Corrected ", ""),
                  ifelse (smoothing.args$roc1$smooth, "Smoothed ", ""),
                  "pAUC (", attr(roc1$auc, "partial.auc")[1], "-", attr(roc1$auc, "partial.auc")[2], " ", attr(roc1$auc, "partial.auc.focus"),
                        ") of roc1", sep="")
  }
  if (identical(attr(roc2$auc, "partial.auc"), FALSE)) {
    nest <- c(nest, paste(ifelse(smoothing.args$roc2$smooth, "Smoothed ", ""), "AUC of roc2", sep=""))
  }
  else {
    nest <- c(nest, paste(ifelse (attr(roc2$auc, "partial.auc.correct"), "Corrected ", ""),
                          ifelse (smoothing.args$roc2$smooth, "Smoothed ", ""),
                          "pAUC (", attr(roc2$auc, "partial.auc")[1], "-", attr(roc2$auc, "partial.auc")[2], " ", attr(roc2$auc, "partial.auc.focus"),
                          ") of roc2", sep=""))
  }
  nest <- sub("Corrected Smoothed", "Corrected smoothed", nest) # no upper on smoothed if corrected.
  names(estimate) <- nest
  null.value <- 0
  names(null.value) <- "difference in AUC"
  htest <- list(
                alternative = alternative,
                data.names = data.names,
                estimate = estimate,
                null.value = null.value
                )
  class(htest) <- "htest"
  
  if (method == "delong") {
    stat <- delong.test(roc1, roc2)
    names(stat) <- "Z"
    htest$statistic <- stat
    htest$method <- "DeLong's test for two correlated ROC curves"
  }
  else {
    # Check if called with density.cases or density.controls
    if (is.null(smoothing.args) || is.numeric(smoothing.args$density.cases) || is.numeric(smoothing.args$density.controls))
      stop("Cannot compute the statistic on ROC curves smoothed with numeric density.controls and density.cases.")

    if(class(progress) != "list")
      progress <- roc.utils.get.progress.bar(progress, title="Bootstrap ROC test", label="Bootstrap in progress...", ...)
    stat <- bootstrap.test(roc1, roc2, boot.n, boot.stratified, smoothing.args, progress)
    stat <- as.vector(stat) # remove auc attributes
    names(stat) <- "D"
    htest$statistic <- stat
    parameter <- c(boot.n, boot.stratified)
    names(parameter) <- c("boot.n", "boot.stratified")
    htest$parameter <- parameter
    htest$method <- "Bootstrap test for two correlated ROC curves"
  }

  if (alternative == "two.sided")
    pval <- 2*pnorm(-abs(stat))
  else if (alternative == "less")
    pval <- pnorm(-stat)
  else
    pval <- pnorm(stat)

  htest$p.value <- pval
  htest$roc1 <- oroc1
  htest$roc2 <- oroc2
  return(htest)

}

# Delong's test, used by roc.test.roc
delong.test <- function(roc1, roc2) {

  # Get data from roc1 and roc2
  YR <- roc1$controls # = C2, n, YRj
  XR <- roc1$cases # = C1, m, XRi
  YS <- roc2$controls # = C2, n, YSj
  XS <- roc2$cases # = C1, m, XSi

  n <- length(YR)
  m <- length(XR)
  mn <- m*n

  # Compute Mann-Whitney statistics and deduce thetaR and thetaS
  MWR <- sapply(1:n, function(j) sapply(1:m, function(i, j) MW.kernel(XR[i], YR[j]), j=j))
  MWS <- sapply(1:n, function(j) sapply(1:m, function(i, j) MW.kernel(XS[i], YS[j]), j=j))

  thetaR <- sum(MWR)/mn
  thetaS <- sum(MWS)/mn

  # Delong-specific computations
  VR10 <- sapply(1:m, function(i) {sum(MWR[i,])})/n
  VR01 <- sapply(1:n, function(j) {sum(MWR[,j])})/m
  VS10 <- sapply(1:m, function(i) {sum(MWS[i,])})/n
  VS01 <- sapply(1:n, function(j) {sum(MWS[,j])})/m

  S10 <- matrix(NA, ncol=2, nrow=2)
  S10[1,1] <- sum((VR10 - thetaR) * (VR10 - thetaR))/(m-1)
  S10[1,2] <- sum((VR10 - thetaR) * (VS10 - thetaS))/(m-1)
  S10[2,1] <- sum((VS10 - thetaS) * (VR10 - thetaR))/(m-1)
  S10[2,2] <- sum((VS10 - thetaS) * (VS10 - thetaS))/(m-1)

  
  S01 <- matrix(NA, ncol=2, nrow=2)
  S01[1,1] <- sum((VR01 - thetaR) * (VR01 - thetaR))/(n-1)
  S01[1,2] <- sum((VR01 - thetaR) * (VS01 - thetaS))/(n-1)
  S01[2,1] <- sum((VS01 - thetaS) * (VR01 - thetaR))/(n-1)
  S01[2,2] <- sum((VS01 - thetaS) * (VS01 - thetaS))/(n-1)

  S <- S10/m + S01/n
  L <- c(1,-1)
  sig <- sqrt(L%*%S%*%L)
  zscore <- (thetaR-thetaS)/sig[1]
  if (is.nan(zscore) && thetaR == thetaS && sig[1] == 0)
    zscore <- 0 # special case: no difference between theta's produces a NaN
  return(zscore)
}

# Bootstrap test, used by roc.test.roc
bootstrap.test <- function(roc1, roc2, boot.n, boot.stratified, smoothing.args, progress) {

  # Prepare arguments for later calls to roc
  auc1skeleton <- attributes(roc1$auc)
  auc1skeleton$roc <- NULL
  auc1skeleton$direction <- roc1$direction
  auc1skeleton$class <- NULL
  auc1skeleton <- c(auc1skeleton, smoothing.args$roc1)
  auc2skeleton <- attributes(roc2$auc)
  auc2skeleton$roc <- NULL
  auc2skeleton$direction <- roc2$direction
  auc2skeleton$class <- NULL
  auc2skeleton <- c(auc2skeleton, smoothing.args$roc2)

  if (boot.stratified) { # precompute sorted responses if stratified
    response <- factor(c(rep(roc1$levels[1], length(roc1$controls)), rep(roc1$levels[2], length(roc1$cases))), levels=roc1$levels)
    auc1skeleton$response <- response
    auc2skeleton$response <- response
    aucs <- raply(boot.n, stratified.bootstrap.test(roc1, roc2, auc1skeleton, auc2skeleton), .progress=progress)
  }
  else {
    aucs <- raply(boot.n, nonstratified.bootstrap.test(roc1, roc2, auc1skeleton, auc2skeleton), .progress=progress)
  }

  # compute the statistics
  diffs <- aucs[,1] - aucs[,2]

  # are there NA values?
  if (sum(is.na(diffs)) > 0) {
    warning("NA value(s) produced during bootstrap were ignored.")
    diffs <- diffs[!is.na(diffs)]
  }

  sd <- sd(diffs)
  D <- (roc1$auc - roc2$auc) / sd
  if (is.nan(D) && all(diffs == 0) && roc1$auc == roc2$auc)
    D <- 0 # special case: no difference between AUCs produces a NaN

  return(D)
}

stratified.bootstrap.test <- function(roc1, roc2, auc1skeleton, auc2skeleton) {
  # sample control and cases separately for a stratified bootstrap
  idx.controls <- sample(1:length(roc1$controls), replace=TRUE)
  idx.cases <- sample(1:length(roc1$cases), replace=TRUE)
  # finish roc skeletons
  auc1skeleton$predictor <- c(roc1$controls[idx.controls], roc1$cases[idx.cases])
  auc2skeleton$predictor <- c(roc2$controls[idx.controls], roc2$cases[idx.cases])
  # Resampled ROC might not be smoothable: catch errors
  auc1 <- try(do.call("roc.default", auc1skeleton)$auc)
  if (class(auc1) == "try-error")
    auc1 <- NA
  auc2 <- try(do.call("roc.default", auc2skeleton)$auc)
  if (class(auc2) == "try-error")
    auc2 <- NA
  return(c(auc1, auc2))
#  return(c(do.call("roc.default", auc1skeleton)$auc, do.call("roc.default", auc2skeleton)$auc))
}

nonstratified.bootstrap.test <- function(roc1, roc2, auc1skeleton, auc2skeleton) {
  # sample all patients
  idx.all <- sample(1:length(roc1$response), replace=TRUE)
  # finish roc skeletons
  auc1skeleton$response <- roc1$response[idx.all]
  auc1skeleton$predictor <- roc1$predictor[idx.all]
  auc2skeleton$response <- roc2$response[idx.all]
  auc2skeleton$predictor <- roc2$predictor[idx.all]
  # Resampled ROC might not be smoothable: catch errors
  auc1 <- try(do.call("roc.default", auc1skeleton)$auc)
  if (class(auc1) == "try-error")
    auc1 <- NA
  auc2 <- try(do.call("roc.default", auc2skeleton)$auc)
  if (class(auc2) == "try-error")
    auc2 <- NA
  return(c(auc1, auc2))
#  return(c(do.call("roc.default", auc1skeleton)$auc, do.call("roc.default", auc2skeleton)$auc))
}
