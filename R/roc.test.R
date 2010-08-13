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

roc.test.default <- function(response, predictor1, predictor2=NULL, na.rm=TRUE, method=NULL, ...) {
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
    roc1 <- roc(response, predictor1, ...)
    roc2 <- roc(response, predictor2, ...)
    call <- match.call()
    data.names <- paste(deparse(call$predictor1), "and", deparse(call$predictor2))
  }
  test <- roc.test.roc(roc1, roc2, method=method, ...)
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

roc.test.roc <- function(roc1, roc2, method=c("delong", "bootstrap", "venkatraman"), alternative = c("two.sided", "less", "greater"), reuse.auc=TRUE, boot.n=2000, boot.stratified=TRUE, ties.method="first", progress=getOption("pROCProgress")$name, ...) {
  alternative <- match.arg(alternative)
  data.names <- paste(deparse(substitute(roc1)), "and", deparse(substitute(roc2)))
  if (class(roc2) == "auc")
    roc2 <- attr(roc2, "roc")

  # store which objects are smoothed, and how
  smoothing.args <- list()
  if (class(roc1) == "smooth.roc") {
    smoothing.args$roc1 <- roc1$smoothing.args
    smoothing.args$roc1$smooth <- TRUE
    roc1 <- attr(roc1, "roc")
    #oroc1$auc <- roc1$auc
  }
  else {
    smoothing.args$roc1 <- list(smooth=FALSE)
  }
  if (class(roc2) == "smooth.roc") {
    smoothing.args$roc2 <- roc2$smoothing.args
    smoothing.args$roc2$smooth <- TRUE
    roc2 <- attr(roc2, "roc")
    #oroc2$auc <- roc2$auc
  }
  else {
    smoothing.args$roc2 <- list(smooth=FALSE)
  }

  # Check ROC curves are paired
  paired <- are.paired(roc1, roc2, return.paired.rocs=TRUE, reuse.auc=TRUE, reuse.ci=FALSE, reuse.smooth=TRUE)
  if (!paired) {
    stop("The ROC test is defined only on paired ROC curves")
  }
  paired.roc1 <- attr(paired, "roc1")
  paired.roc2 <- attr(paired, "roc2")

  # check that the AUC was computed, or do it now
  if (is.null(paired.roc1$auc) | !reuse.auc) {
    if (smoothing.args$roc1$smooth) {
      paired.roc1$auc <- auc(smooth.roc=do.call("smooth.roc", c(list(roc=paired.roc1), smoothing.args$roc1)), ...)
    }
    else
      paired.roc1$auc <- auc(paired.roc1, ...)
  }
  if (is.null(paired.roc2$auc) | !reuse.auc) {
    if (smoothing.args$roc2$smooth)
      paired.roc2$auc <- auc(smooth.roc=do.call("smooth.roc", c(list(roc=paired.roc2), smoothing.args$roc2)), ...)
    else
      paired.roc2$auc <- auc(paired.roc2, ...)
  }
    
  # check that the full AUC was requested in auc. Otherwise, issue a warning
  if (!identical(attributes(paired.roc1$auc)[names(attributes(paired.roc1$auc))!="roc"], attributes(paired.roc2$auc)[names(attributes(paired.roc2$auc))!="roc"]))
    warning("Different AUC specifications in the ROC curves. Enforcing the inconsistency, but unexpected results may be produced.")
  # check that the full AUC was requested in auc. Otherwise, issue a warning
  if (!identical(smoothing.args$roc1, smoothing.args$roc2))
    warning("Different smoothing parameters in the ROC curves. Enforcing the inconsistency, but unexpected results may be produced.")

  # Check the method
  if (missing(method) | is.null(method)) {
    # determine method if missing
    if (is.numeric(attr(paired.roc1$auc, "partial.auc")) && length(attr(paired.roc1$auc, "partial.auc") == 2)) {
      # partial auc: go for bootstrap
      method <- "bootstrap"
    }
    else if (smoothing.args$roc1$smooth || smoothing.args$roc2$smooth) {
      # smoothing in one or both: bootstrap
      method <- "bootstrap"
    }
    else if (paired.roc1$direction != paired.roc2$direction) {
      # delong doesn't work well with opposite directions (will report high significance if paired.roc1$auc and paired.roc2$auc are similar and high)
      method <- "bootstrap"
    }
    else {
      method <- "delong"
    }
  }
  else {
    method <- match.arg(method)
    if (method == "delong") {
      # delong NA to pAUC: warn + change
      if (is.numeric(attr(paired.roc1$auc, "partial.auc")) && length(attr(paired.roc1$auc, "partial.auc") == 2)) {
        warning("Using DeLong's test for partial AUC is not supported. Using bootstrap test instead.")
        method <- "bootstrap"
      }
      if (smoothing.args$roc1$smooth || smoothing.args$roc2$smooth) {
        warning("Using DeLong's test for smoothed ROCs is not supported. Using bootstrap test instead.")
        method <- "bootstrap"
      }
      if (paired.roc1$direction != paired.roc2$direction)
        warning("DeLong's test should not be applied to ROC curves with a different direction.")
    }
    else if (method == "venkatraman") {
      if (is.numeric(attr(paired.roc1$auc, "partial.auc")) && length(attr(paired.roc1$auc, "partial.auc") == 2))
        warning("Partial AUC is ignored in Venkatraman's test.")
      if (smoothing.args$roc1$smooth || smoothing.args$roc2$smooth)
        stop("Using Venkatraman's test for smoothed ROCs is not supported.")
      if (paired.roc1$direction != paired.roc2$direction)
        warning("Venkatraman's test should not be applied to ROC curves with a different direction.")
      if (alternative != "two.sided") {
        warning("Only two-sided tests are available for Venkatraman.")
        alternative <- "two.sided"
      }
    }
  }

  # Prepare the return value htest
  if (smoothing.args$roc1$smooth)
    estimate <- do.call("smooth.roc", c(list(roc=paired.roc1), smoothing.args$roc1))$auc
  else
    estimate <- paired.roc1$auc
  if (smoothing.args$roc2$smooth)
    estimate <- c(estimate, do.call("smooth.roc", c(list(roc=paired.roc2), smoothing.args$roc2))$auc)
  else
    estimate <- c(estimate, paired.roc2$auc)
  if (identical(attr(paired.roc1$auc, "partial.auc"), FALSE)) {
    nest <- paste(ifelse(smoothing.args$roc1$smooth, "Smoothed ", ""), "AUC of roc1", sep="")
  }
  else {
    nest <- paste(ifelse (attr(paired.roc1$auc, "partial.auc.correct"), "Corrected ", ""),
                  ifelse (smoothing.args$roc1$smooth, "Smoothed ", ""),
                  "pAUC (", attr(paired.roc1$auc, "partial.auc")[1], "-", attr(paired.roc1$auc, "partial.auc")[2], " ", attr(paired.roc1$auc, "partial.auc.focus"),
                        ") of roc1", sep="")
  }
  if (identical(attr(paired.roc2$auc, "partial.auc"), FALSE)) {
    nest <- c(nest, paste(ifelse(smoothing.args$roc2$smooth, "Smoothed ", ""), "AUC of roc2", sep=""))
  }
  else {
    nest <- c(nest, paste(ifelse (attr(paired.roc2$auc, "partial.auc.correct"), "Corrected ", ""),
                          ifelse (smoothing.args$roc2$smooth, "Smoothed ", ""),
                          "pAUC (", attr(paired.roc2$auc, "partial.auc")[1], "-", attr(paired.roc2$auc, "partial.auc")[2], " ", attr(paired.roc2$auc, "partial.auc.focus"),
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
    stat <- delong.test(paired.roc1, paired.roc2)
    names(stat) <- "Z"
    htest$statistic <- stat
    htest$method <- "DeLong's test for two correlated ROC curves"

    if (alternative == "two.sided")
      pval <- 2*pnorm(-abs(stat))
    else if (alternative == "less")
      pval <- pnorm(-stat)
    else
      pval <- pnorm(stat)
    htest$p.value <- pval
  }
  else if (method == "venkatraman") {
    if(class(progress) != "list")
      progress <- roc.utils.get.progress.bar(progress, title="Venkatraman ROC test", label="Permutations in progress...", ...)
    stats <- venkatraman.test(paired.roc1, paired.roc2, boot.n, ties.method, progress)
    stat <- stats[[1]]
    names(stat) <- "E"
    htest$statistic <- stat
    parameter <- c(boot.n)
    names(parameter) <- "boot.n"
    htest$parameter <- parameter
    pval <- sum(stats[[2]]>=stats[[1]])/boot.n
    htest$p.value <- pval
    htest$method <- "Venkatraman's test for two paired ROC curves"
  }
  else { # method == "bootstrap"
    # Check if called with density.cases or density.controls
    if (is.null(smoothing.args) || is.numeric(smoothing.args$density.cases) || is.numeric(smoothing.args$density.controls))
      stop("Cannot compute the statistic on ROC curves smoothed with numeric density.controls and density.cases.")

    if(class(progress) != "list")
      progress <- roc.utils.get.progress.bar(progress, title="Bootstrap ROC test", label="Bootstrap in progress...", ...)
    stat <- bootstrap.test(paired.roc1, paired.roc2, boot.n, boot.stratified, smoothing.args, progress)
    stat <- as.vector(stat) # remove auc attributes
    names(stat) <- "D"
    htest$statistic <- stat
    parameter <- c(boot.n, boot.stratified)
    names(parameter) <- c("boot.n", "boot.stratified")
    htest$parameter <- parameter
    htest$method <- "Bootstrap test for two correlated ROC curves"

    if (alternative == "two.sided")
      pval <- 2*pnorm(-abs(stat))
    else if (alternative == "less")
      pval <- pnorm(-stat)
    else
      pval <- pnorm(stat)
    htest$p.value <- pval
  }

  htest$roc1 <- paired.roc1
  htest$roc2 <- paired.roc2
  # Restore smoothing if necessary
  if (smoothing.args$roc1$smooth)
    htest$roc1 <- do.call("smooth.roc", c(list(roc=roc1), smoothing.args$roc1))
  if (smoothing.args$roc2$smooth)
    htest$roc2 <- do.call("smooth.roc", c(list(roc=roc2), smoothing.args$roc2))
  return(htest)

}

venkatraman.test <- function(roc1, roc2, boot.n, ties.method="first", progress) {
  X <- roc1$predictor
  Y <- roc2$predictor
  R <- rank(X, ties.method = ties.method)
  S <- rank(Y, ties.method = ties.method)
  D <- roc1$response # because roc1&roc2 are paired

  E <- venkatraman.stat(R, S, D, roc1$levels)
  EP <- raply(boot.n, venkatraman.permutation(R, S, D, roc1$levels, ties.method), .progress=progress)
  return(list(E, EP))
}

venkatraman.permutation <- function(R, S, D, levels, ties.method) {
  # Break ties
  R2 <- R + runif(length(D)) - 0.5#, ties.method = ties.method) # Add small amount of random but keep same mean
  S2 <- S + runif(length(D)) - 0.5#, ties.method = ties.method)

  # Permutation
  q <- 1 - round(runif(length(D)))
  R3 <- R2 * q + (1 - q) * S
  S3 <- S2 * q + (1 - q) * R

  return(venkatraman.stat(rank(R3, ties.method=ties.method), rank(S3, ties.method=ties.method), D, levels))
}

venkatraman.stat <- function(R, S, D, levels) {
  R.controls <- R[D==levels[1]]
  R.cases <- R[D==levels[2]]
  S.controls <- S[D==levels[1]]
  S.cases <- S[D==levels[2]]
  n <- length(D)

  R.fn <- sapply(1:n, function(x) sum(R.cases <= x))
  R.fp <- sapply(1:n, function(x) sum(R.controls > x))
  S.fn <- sapply(1:n, function(x) sum(S.cases <= x))
  S.fp <- sapply(1:n, function(x) sum(S.controls > x))

  return(sum(abs((S.fn + S.fp) - (R.fn + R.fp))))
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

  # rename method into smooth.method for roc
  smoothing.args$roc1$smooth.method <- smoothing.args$roc1$method
  smoothing.args$roc1$method <- NULL
  smoothing.args$roc2$smooth.method <- smoothing.args$roc2$method
  smoothing.args$roc2$method <- NULL

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

  # Restore smoothing if necessary
  if (smoothing.args$roc1$smooth) {
    smoothing.args$roc1$method <- smoothing.args$roc1$smooth.method
    roc1 <- do.call("smooth.roc", c(list(roc=roc1), smoothing.args$roc1))
  }
  if (smoothing.args$roc2$smooth) {
    smoothing.args$roc2$method <- smoothing.args$roc2$smooth.method
    roc2 <- do.call("smooth.roc", c(list(roc=roc2), smoothing.args$roc2))
  }
  
  D <- (roc1$auc - roc2$auc) / sd(diffs)
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
  auc1 <- try(do.call("roc.default", auc1skeleton)$auc, silent=TRUE)
  if (class(auc1) == "try-error")
    auc1 <- NA
  auc2 <- try(do.call("roc.default", auc2skeleton)$auc, silent=TRUE)
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
  auc1 <- try(do.call("roc.default", auc1skeleton)$auc, silent=TRUE)
  if (class(auc1) == "try-error")
    auc1 <- NA
  auc2 <- try(do.call("roc.default", auc2skeleton)$auc, silent=TRUE)
  if (class(auc2) == "try-error")
    auc2 <- NA
  return(c(auc1, auc2))
#  return(c(do.call("roc.default", auc1skeleton)$auc, do.call("roc.default", auc2skeleton)$auc))
}
