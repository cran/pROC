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

coords <- function(...)
  UseMethod("coords")

coords.smooth.roc <- function(smooth.roc, x, input=c("specificity", "sensitivity"), ret=c("specificity", "sensitivity"), as.list=FALSE, ...) {
  # make sure x was provided
  if (missing(x))
    stop("'x' must be a numeric or character vector.")
  # match input 
  input <- match.arg(input)
  # match return
  ret <- match.arg(ret, several.ok=TRUE)

  if (is.character(x)) {
    x <- match.arg(x, c("best")) # no thresholds in smoothed roc: only best is possible
    partial.auc <- attr(smooth.roc$auc, "partial.auc")
    if (is.null(smooth.roc$auc) || identical(partial.auc, FALSE)) {
      sesp <- smooth.roc$se+smooth.roc$sp
      best.log <- sesp==max(sesp)
      se <- smooth.roc$sensitivities[best.log]
      sp <- smooth.roc$specificities[best.log]
    }
    else {
      if (attr(smooth.roc$auc, "partial.auc.focus") == "sensitivity") {
        ses <- smooth.roc$sensitivities[smooth.roc$se <= partial.auc[1] & smooth.roc$se >= partial.auc[2]]
        sps <- smooth.roc$specificities[smooth.roc$se <= partial.auc[1] & smooth.roc$se >= partial.auc[2]]
        sesp <- ses+sps
        best.log <- sesp==max(sesp)
        se <- ses[best.log]
        sp <- sps[best.log]
      }
      else {
        ses <- smooth.roc$sensitivities[smooth.roc$sp <= partial.auc[1] & smooth.roc$sp >= partial.auc[2]]
        sps <- smooth.roc$specificities[smooth.roc$sp <= partial.auc[1] & smooth.roc$sp >= partial.auc[2]]
        sesp <- ses+sps
        best.log <- sesp==max(sesp)
        se <- ses[best.log]
        sp <- sps[best.log]
      }
    }
    if (length(se) == 1) {
      if (as.list) {
        return(list(sensitivity=se, specificity=sp)[ret])
      }
      else {
        return(c(sensitivity=se, specificity=sp)[ret])
      }
    }
    else if (length(se) > 1) {
      if (as.list) {
        co <- apply(rbind(sensitivity=se, specificity=sp), 2, as.list)
        names(co) <- rep("best", length(co))
        return(co)
      }
      else {
        co <- rbind(sensitivity=se, specificity=sp)
        colnames(co) <- rep(x, dim(co)[2])
        return(co)
      }
    }
  }

  # use coords.roc
  smooth.roc$thresholds <- rep(NA, length(smooth.roc$specificities))
  coords.roc(smooth.roc, x, input, ret, as.list, ...)
}

coords.roc <- function(roc, x, input=c("threshold", "specificity", "sensitivity"), ret=c("threshold", "specificity", "sensitivity"), as.list=FALSE, ...) {
  # make sure x was provided
  if (missing(x) || length(x) == 0)
    stop("'x' must be a numeric or character vector of positive length.")
  # match input 
  input <- match.arg(input)
  # match return
  ret <- match.arg(ret, several.ok=TRUE)
  # make sure the sort of roc is correct
  roc <- sort(roc)

  if (is.character(x)) {
    x <- match.arg(x, c("all", "local maximas", "best"))
    partial.auc <- attr(roc$auc, "partial.auc")
    if (x == "all") {
      # Pre-filter thresholds based on partial.auc
      if (is.null(roc$auc) || identical(partial.auc, FALSE)) {
        se <- roc$se
        sp <- roc$sp
        thres <- roc$thresholds
      }
      else {
        if (attr(roc$auc, "partial.auc.focus") == "sensitivity") {
          se <- roc$se[roc$se <= partial.auc[1] & roc$se >= partial.auc[2]]
          sp <- roc$sp[roc$se <= partial.auc[1] & roc$se >= partial.auc[2]]
          thres <- roc$thresholds[roc$se <= partial.auc[1] & roc$se >= partial.auc[2]]
        }
        else {
          se <- roc$se[roc$sp <= partial.auc[1] & roc$sp >= partial.auc[2]]
          sp <- roc$sp[roc$sp <= partial.auc[1] & roc$sp >= partial.auc[2]]
          thres <- roc$thresholds[roc$sp <= partial.auc[1] & roc$sp >= partial.auc[2]]
        }
      }
      co <- coords(roc, x=thres, input="threshold", ret=ret, as.list=as.list)
      if (class(co) == "matrix")
        colnames(co) <- rep(x, dim(co)[2])
      else if (class(co) == "list" && class(co[[1]]) == "list")
        names(co) <- rep(x, length(co))
      return(co)
    }
    else if (x == "local maximas") {
      # Pre-filter thresholds based on partial.auc
      if (is.null(roc$auc) || identical(partial.auc, FALSE)) {
        se <- roc$se
        sp <- roc$sp
        thres <- roc$thresholds
      }
      else {
        if (attr(roc$auc, "partial.auc.focus") == "sensitivity") {
          se <- roc$se[roc$se <= partial.auc[1] & roc$se >= partial.auc[2]]
          sp <- roc$sp[roc$se <= partial.auc[1] & roc$se >= partial.auc[2]]
          thres <- roc$thresholds[roc$se <= partial.auc[1] & roc$se >= partial.auc[2]]
        }
        else {
          se <- roc$se[roc$sp <= partial.auc[1] & roc$sp >= partial.auc[2]]
          sp <- roc$sp[roc$sp <= partial.auc[1] & roc$sp >= partial.auc[2]]
          thres <- roc$thresholds[roc$sp <= partial.auc[1] & roc$sp >= partial.auc[2]]
        }
      }
      lm.idx <- roc.utils.max.thresholds.idx(thres, sp=sp, se=se)
      co <- coords(roc, x=thres[lm.idx], input="threshold", ret=ret, as.list=as.list)
      if (class(co) == "matrix")
        colnames(co) <- rep(x, dim(co)[2])
      else if (class(co) == "list" && class(co[[1]]) == "list")
        names(co) <- rep(x, length(co))
      return(co)
    }
    else { # x == "best"
      # Pre-filter thresholds based on partial.auc
      if (is.null(roc$auc) || identical(partial.auc, FALSE)) {
        sesp <- roc$se+roc$sp
        thres <- roc$thresholds[sesp==max(sesp)]
      }
      else {
        if (attr(roc$auc, "partial.auc.focus") == "sensitivity") {
          sesp <- (roc$se+roc$sp)[roc$se <= partial.auc[1] & roc$se >= partial.auc[2]]
          thres <- roc$thresholds[roc$se <= partial.auc[1] & roc$se >= partial.auc[2]][sesp==max(sesp)]
        }
        else {
          sesp <- (roc$se+roc$sp)[roc$sp <= partial.auc[1] & roc$sp >= partial.auc[2]]
          thres <- roc$thresholds[roc$sp <= partial.auc[1] & roc$sp >= partial.auc[2]][sesp==max(sesp)]
        }
      }
      co <- coords(roc, x=thres, input="threshold", ret=ret, as.list=as.list)
      if (class(co) == "matrix")
        colnames(co) <- rep(x, dim(co)[2])
      else if (class(co) == "list" && class(co[[1]]) == "list")
        names(co) <- rep(x, length(co))
      return(co)
    }
  }
  else if (is.numeric(x)) {
    if (length(x) > 1) { # make this function a vector function
      if (as.list) {
        res <- lapply(x, function(x) coords.roc(roc, x, input, ret, as.list))
        names(res) <- x
      }
      else {
        res <- sapply(x, function(x) coords.roc(roc, x, input, ret, as.list))
        if (length(ret) == 1) {# sapply returns a vector instead of a matrix
          res <- t(res)
          rownames(res) <- ret
        }
        colnames(res) <- x
      }
      return(res)
    }
    if (input == "threshold") {
      res <- c(x, as.vector(roc.utils.perfs(x, roc$controls, roc$cases, roc$direction)) * ifelse(roc$percent, 100, 1))
    }
    if (input == "specificity") {
      if (x < 0 || x > ifelse(roc$percent, 100, 1))
        stop("Input specificity not within the ROC space.")
      if (x %in% roc$sp) {
        idx <- match(x, roc$sp)
        res <- c(roc$thresholds[idx], roc$sp[idx], roc$se[idx])
      }
      else { # need to interpolate
        idx.next <- match(TRUE, roc$sp > x)
        proportion <-  (x - roc$sp[idx.next - 1]) / (roc$sp[idx.next] - roc$sp[idx.next - 1])
        int.se <- roc$se[idx.next - 1] - proportion * (roc$se[idx.next - 1] - roc$se[idx.next])
        res <- c(NA, x, int.se)
      }
    }
    if (input == "sensitivity") {
      if (x < 0 || x > ifelse(roc$percent, 100, 1))
        stop("Input sensitivity not within the ROC space.")
      if (x %in% roc$se) {
        idx <- length(roc$se) + 1 - match(TRUE, rev(roc$se) == x)
        res <- c(roc$thresholds[idx], roc$sp[idx], roc$se[idx])
      }
      else { # need to interpolate
        idx.next <- match(TRUE, roc$se < x)
        proportion <- (x - roc$se[idx.next]) / (roc$se[idx.next - 1] - roc$se[idx.next])
        int.sp <- roc$sp[idx.next] + proportion * (roc$sp[idx.next - 1] - roc$sp[idx.next])
        res <- c(NA, int.sp, x)
      }
    }
    if (as.list) {
      list <- list(threshold=res[1], specificity=res[2], sensitivity=res[3])
      return(list[ret])
    }
    else {
      names(res) <- c("threshold", "specificity", "sensitivity")
      return(res[ret])
    }
  }
  else {
    stop("'x' must be a numeric or character vector.")
  }
}
