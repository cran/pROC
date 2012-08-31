pkgname <- "pROC"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('pROC')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("aSAH")
### * aSAH

flush(stderr()); flush(stdout())

### Name: aSAH
### Title: Subarachnoid hemorrhage data
### Aliases: aSAH
### Keywords: datasets

### ** Examples

# load the dataset
data(aSAH)

# Gender, outcome and set
with(aSAH, table(gender, outcome))

# Age
with(aSAH, by(age, outcome, mean))
with(aSAH, by(age, outcome,
     function(x) sprintf("mean: %.1f (+/- %.1f), median: %.1f (%i-%i)",
                         mean(x), sd(x), median(x), min(x), max(x))))

# WFNS score
with(aSAH, table(wfns=ifelse(wfns<=2, "1-2", "3-4-5"), outcome))




cleanEx()
nameEx("are.paired")
### * are.paired

flush(stderr()); flush(stdout())

### Name: are.paired
### Title: Are two ROC curves paired?
### Aliases: are.paired are.paired.smooth.roc are.paired.auc are.paired.roc
### Keywords: programming logic roc

### ** Examples

data(aSAH)
aSAH.copy <- aSAH

# artificially insert NAs for demonstration purposes
aSAH.copy$outcome[42] <- NA
aSAH.copy$s100b[24] <- NA
aSAH.copy$ndka[1:10] <- NA

# Call roc() on the whole data
roc1 <- roc(aSAH.copy$outcome, aSAH.copy$s100b)
roc2 <- roc(aSAH.copy$outcome, aSAH.copy$ndka)
# are.paired can still find that the curves were paired
are.paired(roc1, roc2) # TRUE

# Removing the NAs manually before passing to roc() un-pairs the ROC curves
nas <- is.na(aSAH.copy$outcome) | is.na(aSAH.copy$ndka)
roc2b <- roc(aSAH.copy$outcome[!nas], aSAH.copy$ndka[!nas])
are.paired(roc1, roc2b) # FALSE

# Getting the two paired ROC curves with additional smoothing and ci options
roc2$ci <- ci(roc2)
paired <- are.paired(smooth(roc1), roc2, return.paired.rocs=TRUE, reuse.ci=TRUE)
paired.roc1 <- attr(paired, "roc1")
paired.roc2 <- attr(paired, "roc2")




cleanEx()
nameEx("auc")
### * auc

flush(stderr()); flush(stdout())

### Name: auc
### Title: Compute the area under the ROC curve
### Aliases: auc auc.default auc.formula auc.roc auc.smooth.roc
###   auc.multiclass.roc
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

# Syntax (response, predictor):
auc(aSAH$outcome, aSAH$s100b)

# With a roc object:
rocobj <- roc(aSAH$outcome, aSAH$s100b)
# Full AUC:
auc(rocobj)
# Partial AUC:
auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE)

# Alternatively, you can get the AUC directly from roc():
roc(aSAH$outcome, aSAH$s100b)$auc
roc(aSAH$outcome, aSAH$s100b,
    partial.auc=c(1, .8), partial.auc.focus="se",
    partial.auc.correct=TRUE)$auc



cleanEx()
nameEx("ci")
### * ci

flush(stderr()); flush(stdout())

### Name: ci
### Title: Compute the confidence interval of a ROC curve
### Aliases: ci ci.default ci.formula ci.roc ci.smooth.roc
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

# Syntax (response, predictor):
ci(aSAH$outcome, aSAH$s100b)

# With a roc object:
rocobj <- roc(aSAH$outcome, aSAH$s100b)

# Of an AUC 
ci(rocobj)
ci(rocobj, of="auc")
# this is strictly equivalent to:
ci.auc(rocobj)

# Of thresholds, sp, se...
## Not run: 
##D ci(rocobj, of="thresholds")
##D ci(rocobj, of="thresholds", thresholds=0.51)
##D ci(rocobj, of="thresholds", thresholds="all")
##D ci(rocobj, of="sp", sensitivities=c(.95, .9, .85))
##D ci(rocobj, of="se")
## End(Not run)

# Alternatively, you can get the CI directly from roc():
rocobj <- roc(aSAH$outcome, aSAH$s100b, ci=TRUE, of="auc")
rocobj$ci




cleanEx()
nameEx("ci.auc")
### * ci.auc

flush(stderr()); flush(stdout())

### Name: ci.auc
### Title: Compute the confidence interval of the AUC
### Aliases: ci.auc ci.auc.auc ci.auc.default ci.auc.formula ci.auc.roc
###   ci.auc.smooth.roc
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

# Syntax (response, predictor):
ci.auc(aSAH$outcome, aSAH$s100b)

# With a roc object:
rocobj <- roc(aSAH$outcome, aSAH$s100b)
# default values
ci.auc(rocobj)
ci(rocobj)
ci(auc(rocobj))
ci(rocobj$auc)
ci(rocobj$auc, method="delong")

# Partial AUC and customized bootstrap:
ci.auc(aSAH$outcome, aSAH$s100b,
       boot.n=100, conf.level=0.9, stratified=FALSE, partial.auc=c(1, .8),
       partial.auc.focus="se", partial.auc.correct=TRUE)

# Note that the following will NOT give a CI of the partial AUC:
ci.auc(rocobj, boot.n=500, conf.level=0.9, stratified=FALSE,
       partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE)
# This is because rocobj$auc is not a partial AUC.
## Not run: 
##D # You can overcome this problem with reuse.auc:
##D ci.auc(rocobj, boot.n=500, conf.level=0.9, stratified=FALSE,
##D        partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE,
##D        reuse.auc=FALSE)
## End(Not run)

# Alternatively, you can get the CI directly from roc():
rocobj <- roc(aSAH$outcome, aSAH$s100b, ci=TRUE, of="auc")
rocobj$ci

## Not run: 
##D # On a smoothed ROC, the CI is re-computed automatically
##D smooth(rocobj)
##D # Or you can compute a new one:
##D ci.auc(smooth(rocobj, method="density", reuse.ci=FALSE), boot.n=100)
## End(Not run)



cleanEx()
nameEx("ci.se")
### * ci.se

flush(stderr()); flush(stdout())

### Name: ci.se
### Title: Compute the confidence interval of sensitivities at given
###   specificities
### Aliases: ci.se ci.se.default ci.se.formula ci.se.roc ci.se.smooth.roc
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

## Not run: 
##D # Syntax (response, predictor):
##D ci.se(aSAH$outcome, aSAH$s100b)
##D 
##D # With a roc object and less bootstrap:
##D rocobj <- roc(aSAH$outcome, aSAH$s100b)
##D ci.se(rocobj, boot.n=100)
##D 
##D # Customized bootstrap and specific specificities:
##D ci.se(rocobj, c(.95, .9, .85), boot.n=500, conf.level=0.9, stratified=FALSE)
## End(Not run)

# Alternatively, you can get the CI directly from roc():
rocobj <- roc(aSAH$outcome,
              aSAH$s100b, ci=TRUE, of="se", boot.n=100)
rocobj$ci

# Plotting the CI
plot(rocobj)
plot(rocobj$ci)

## Not run: 
##D # On a smoothed ROC, the CI is re-computed automatically
##D smooth(rocobj)
##D # Or you can compute a new one:
##D ci.se(smooth(rocobj, method="density", reuse.ci=FALSE), boot.n=100)
## End(Not run)



cleanEx()
nameEx("ci.sp")
### * ci.sp

flush(stderr()); flush(stdout())

### Name: ci.sp
### Title: Compute the confidence interval of specificities at given
###   sensitivities
### Aliases: ci.sp ci.sp.default ci.sp.formula ci.sp.roc ci.sp.smooth.roc
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

## Not run: 
##D # Syntax (response, predictor):
##D ci.sp(aSAH$outcome, aSAH$s100b)
##D 
##D # With a roc object:
##D rocobj <- roc(aSAH$outcome, aSAH$s100b)
##D ci.sp(rocobj)
##D 
##D # Customized bootstrap and specific specificities:
##D ci.sp(rocobj, c(.95, .9, .85), boot.n=500, conf.level=0.9, stratified=FALSE)
## End(Not run)

# Alternatively, you can get the CI directly from roc():
rocobj <- roc(aSAH$outcome,
              aSAH$s100b, ci=TRUE, of="sp", boot.n=100)
rocobj$ci

# Plotting the CI
plot(rocobj)
plot(rocobj$ci)

## Not run: 
##D # On a smoothed ROC, the CI is re-computed automatically
##D smooth(rocobj)
##D # Or you can compute a new one:
##D ci.sp(smooth(rocobj, method="density", reuse.ci=FALSE), boot.n=100)
## End(Not run)



cleanEx()
nameEx("ci.thresholds")
### * ci.thresholds

flush(stderr()); flush(stdout())

### Name: ci.thresholds
### Title: Compute the confidence interval of thresholds
### Aliases: ci.thresholds ci.thresholds.default ci.thresholds.formula
###   ci.thresholds.roc ci.thresholds.smooth.roc
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

## Not run: 
##D # Syntax (response, predictor):
##D ci.thresholds(aSAH$outcome, aSAH$s100b)
##D 
##D # With a roc object:
##D rocobj <- roc(aSAH$outcome, aSAH$s100b)
##D ci.thresholds(rocobj)
##D 
##D # Customized bootstrap and specific thresholds:
##D ci.thresholds(aSAH$outcome, aSAH$s100b,
##D               boot.n=500, conf.level=0.9, stratified=FALSE,
##D               thresholds=c(0.5, 1, 2))
## End(Not run)

# Alternatively, you can get the CI directly from roc():
rocobj <- roc(aSAH$outcome,
              aSAH$s100b, ci=TRUE, of="thresholds")
rocobj$ci

# Plotting the CI
plot(rocobj)
plot(rocobj$ci)



cleanEx()
nameEx("coords")
### * coords

flush(stderr()); flush(stdout())

### Name: coords
### Title: Coordinates of a ROC curve
### Aliases: coords coords.roc coords.smooth.roc
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

# Print a roc object:
rocobj <- roc(aSAH$outcome, aSAH$s100b)

coords(rocobj, 0.55)
coords(rocobj, 0.9, "specificity", as.list=TRUE)
coords(rocobj, 0.5, "se", ret="se")
# fully qualified but identical:
coords(roc=rocobj, x=0.5, input="sensitivity", ret="sensitivity")

# Compare with drop=FALSE
coords(rocobj, 0.55, drop=FALSE)
coords(rocobj, 0.9, "specificity", as.list=TRUE, drop=FALSE)

# Same in percent
rocobj <- roc(aSAH$outcome, aSAH$s100b, percent=TRUE)

coords(rocobj, 0.55)
coords(rocobj, 90, "specificity", as.list=TRUE)
coords(rocobj, x=50, input="sensitivity", ret=c("sen", "spec"))

# And with a smoothed ROC curve
coords(smooth(rocobj), 90, "specificity")
coords(smooth(rocobj), 90, "specificity", drop=FALSE)
coords(smooth(rocobj), 90, "specificity", as.list=TRUE)
coords(smooth(rocobj), 90, "specificity", as.list=TRUE, drop=FALSE)

# Get the sensitivities for all thresholds
sensitivities <- coords(rocobj, rocobj$thresholds, "thr", "se")
# This is equivalent to taking sensitivities from rocobj directly
stopifnot(all.equal(as.vector(rocobj$sensitivities), as.vector(sensitivities)))
# You could also write:
sensitivities <- coords(rocobj, "all", ret="se")
stopifnot(all.equal(as.vector(rocobj$sensitivities), as.vector(sensitivities)))

# Get the best threshold
coords(rocobj, "b", ret="t")

# Get the best threshold according to different methods
rocobj <- roc(aSAH$outcome, aSAH$ndka, percent=TRUE)
coords(rocobj, "b", ret="t", best.method="youden") # default
coords(rocobj, "b", ret="t", best.method="closest.topleft")
# and with different weights
coords(rocobj, "b", ret="t", best.method="youden", best.weights=c(50, 0.2))
coords(rocobj, "b", ret="t", best.method="closest.topleft", best.weights=c(5, 0.2))
# and plot them
plot(rocobj, print.thres="best", print.thres.best.method="youden")
plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft")
plot(rocobj, print.thres="best", print.thres.best.method="youden",
                                 print.thres.best.weights=c(50, 0.2)) 
plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft",
                                 print.thres.best.weights=c(5, 0.2)) 

# Return more values:
coords(rocobj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                           "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                           "1-sensitivity", "1-npv", "1-ppv"))
coords(smooth(rocobj), "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                           "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                           "1-sensitivity", "1-npv", "1-ppv"))
coords(smooth(rocobj), 0.5, ret=c("threshold", "specificity", "sensitivity", "accuracy",
                           "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                           "1-sensitivity", "1-npv", "1-ppv"))



cleanEx()
nameEx("cov")
### * cov

flush(stderr()); flush(stdout())

### Name: cov.roc
### Title: Covariance of two paired ROC curves
### Aliases: cov cov.default cov.auc cov.smooth.roc cov.roc
### Keywords: multivariate nonparametric utilities roc

### ** Examples

data(aSAH)

# Basic example with 2 roc objects
roc1 <- roc(aSAH$outcome, aSAH$s100b)
roc2 <- roc(aSAH$outcome, aSAH$wfns)
cov(roc1, roc2)

## Not run: 
##D # The latter used Delong. To use bootstrap:
##D cov(roc1, roc2, method="bootstrap")
##D # Decrease boot.n for a faster execution:
##D cov(roc1, roc2, method="bootstrap", boot.n=1000)
## End(Not run)

## Not run: 
##D # Comparison can be done on smoothed ROCs
##D # Smoothing is re-done at each iteration, and execution is slow
##D cov(smooth(roc1), smooth(roc2))
## End(Not run)
# or from an AUC (no smoothing)
cov(auc(roc1), roc2)

## Not run: 
##D # With bootstrap and return.values, one can compute the variances of the
##D # ROC curves in one single bootstrap run:
##D cov.rocs <- cov(roc1, roc2, method="bootstrap", boot.return=TRUE)
##D # var(roc1):
##D var(attr(cov.rocs, "resampled.values")[,1])
##D # var(roc2):
##D var(attr(cov.rocs, "resampled.values")[,2])
## End(Not run)

## Not run: 
##D # Covariance of partial AUC:
##D roc3 <- roc(aSAH$outcome, aSAH$s100b, partial.auc=c(1, 0.8), partial.auc.focus="se")
##D roc4 <- roc(aSAH$outcome, aSAH$wfns, partial.auc=c(1, 0.8), partial.auc.focus="se")
##D cov(roc3, roc4)
##D # This is strictly equivalent to:
##D cov(roc3, roc4, method="bootstrap")
##D 
##D # Alternatively, we could re-use roc1 and roc2 to get the same result:
##D cov(roc1, roc2, reuse.auc=FALSE, partial.auc=c(1, 0.8), partial.auc.focus="se")
## End(Not run)

# Spurious use of DeLong's test with different direction:
roc5 <- roc(aSAH$outcome, aSAH$s100b, direction="<")
roc6 <- roc(aSAH$outcome, aSAH$s100b, direction=">")
cov(roc5, roc6, method="delong")

## Test data from Hanley and Hajian-Tilaki, 1997
disease.present <- c("Yes", "No", "Yes", "No", "No", "Yes", "Yes", "No", "No", "Yes", "No", "No", "Yes", "No", "No")
field.strength.1 <- c(1, 2, 5, 1, 1, 1, 2, 1, 2, 2, 1, 1, 5, 1, 1)
field.strength.2 <- c(1, 1, 5, 1, 1, 1, 4, 1, 2, 2, 1, 1, 5, 1, 1)
roc7 <- roc(disease.present, field.strength.1)
roc8 <- roc(disease.present, field.strength.2)
# Assess the covariance:
cov(roc7, roc8)

## Not run: 
##D # With bootstrap:
##D cov(roc7, roc8, method="bootstrap")
## End(Not run)




cleanEx()
nameEx("has.partial.auc")
### * has.partial.auc

flush(stderr()); flush(stdout())

### Name: has.partial.auc
### Title: Does the ROC curve have a partial AUC?
### Aliases: has.partial.auc has.partial.auc.smooth.roc has.partial.auc.auc
###   has.partial.auc.roc
### Keywords: programming logic roc

### ** Examples

data(aSAH)

# Full AUC
roc1 <- roc(aSAH$outcome, aSAH$s100b)
has.partial.auc(roc1)
has.partial.auc(auc(roc1))
has.partial.auc(smooth(roc1))

# Partial AUC
roc2 <- roc(aSAH$outcome, aSAH$s100b, partial.auc = c(1, 0.9))
has.partial.auc(roc2)
has.partial.auc(smooth(roc2))

# No AUC
roc3 <- roc(aSAH$outcome, aSAH$s100b, auc = FALSE)
has.partial.auc(roc3)



cleanEx()
nameEx("lines.roc")
### * lines.roc

flush(stderr()); flush(stdout())

### Name: lines.roc
### Title: Add a ROC line to a ROC plot
### Aliases: lines.roc lines.roc.roc lines.smooth.roc lines.roc.smooth.roc
###   lines.roc.default lines.roc.formula
### Keywords: univar nonparametric utilities aplot hplot roc

### ** Examples

data(aSAH)

rocobj <- plot.roc(aSAH$outcome, aSAH$s100b, type="n")
lines(rocobj, type="b", pch=21, col="blue", bg="grey")

# Without using 'lines':
rocobj <- plot.roc(aSAH$outcome, aSAH$s100b, type="b", pch=21, col="blue", bg="grey")




cleanEx()
nameEx("multiclass")
### * multiclass

flush(stderr()); flush(stdout())

### Name: multiclass.roc
### Title: Multi-class AUC
### Aliases: multiclass.roc multiclass.roc.default multiclass.roc.formula
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

# Basic example
multiclass.roc(aSAH$gos6, aSAH$s100b)
# Produces an innocuous warning because one level has no observation

# Select only 3 of the aSAH$gos6 levels:
multiclass.roc(aSAH$gos6, aSAH$s100b, levels=c(3, 4, 5))

# Give the result in percent
multiclass.roc(aSAH$gos6, aSAH$s100b, percent=TRUE)






cleanEx()
nameEx("pROC-package")
### * pROC-package

flush(stderr()); flush(stdout())

### Name: pROC-package
### Title: pROC
### Aliases: pROC-package pROC
### Keywords: package univar nonparametric utilities print htest aplot
###   hplot roc

### ** Examples

data(aSAH)

# Build a ROC object and compute the AUC
roc(aSAH$outcome, aSAH$s100b)
roc(outcome ~ s100b, aSAH)

# Smooth ROC curve
roc(outcome ~ s100b, aSAH, smooth=TRUE)

# more options, CI and plotting
roc1 <- roc(aSAH$outcome,
            aSAH$s100b, percent=TRUE,
            # arguments for auc
            partial.auc=c(100, 90), partial.auc.correct=TRUE,
            partial.auc.focus="sens",
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE)

# Add to an existing plot. Beware of 'percent' specification!
roc2 <- roc(aSAH$outcome, aSAH$wfns,
            plot=TRUE, add=TRUE, percent=roc1$percent)

## Coordinates of the curve ##
coords(roc1, "best", ret=c("threshold", "specificity", "1-npv"))
coords(roc2, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))

## Confidence intervals ##

# CI of the AUC
ci(roc2)

## Not run: 
##D # CI of the curve
##D sens.ci <- ci.se(roc1, specificities=seq(0, 100, 5))
##D plot(sens.ci, type="shape", col="lightblue")
##D plot(sens.ci, type="bars")
## End(Not run)

# need to re-add roc2 over the shape
plot(roc2, add=TRUE)

## Not run: 
##D # CI of thresholds
##D plot(ci.thresholds(roc2))
## End(Not run)


## Comparisons ##

# Test on the whole AUC
roc.test(roc1, roc2, reuse.auc=FALSE)

## Not run: 
##D # Test on a portion of the whole AUC
##D roc.test(roc1, roc2, reuse.auc=FALSE, partial.auc=c(100, 90),
##D          partial.auc.focus="se", partial.auc.correct=TRUE)
##D 
##D # With modified bootstrap parameters
##D roc.test(roc1, roc2, reuse.auc=FALSE, partial.auc=c(100, 90),
##D          partial.auc.correct=TRUE, boot.n=1000, boot.stratified=FALSE)
## End(Not run)



cleanEx()
nameEx("plot.ci")
### * plot.ci

flush(stderr()); flush(stdout())

### Name: plot.ci
### Title: Plot confidence intervals
### Aliases: plot.ci plot.ci.thresholds plot.ci.se plot.ci.sp
### Keywords: univar nonparametric utilities aplot hplot roc

### ** Examples

data(aSAH)
## Not run: 
##D # Start a ROC plot
##D rocobj <- plot.roc(aSAH$outcome, aSAH$s100b)
##D plot(rocobj)
##D # Thresholds
##D ci.thresolds.obj <- ci.thresholds(rocobj)
##D plot(ci.thresolds.obj)
##D # Specificities
##D plot(rocobj) # restart a new plot
##D ci.sp.obj <- ci.sp(rocobj, boot.n=500)
##D plot(ci.sp.obj)
##D # Sensitivities
##D plot(rocobj) # restart a new plot
##D ci.se.obj <- ci(rocobj, of="se", boot.n=500)
##D plot(ci.se.obj)
##D 
##D # Plotting a shape. We need more
##D ci.sp.obj <- ci.sp(rocobj, sensitivities=seq(0, 1, .01), boot.n=100)
##D plot(rocobj) # restart a new plot
##D plot(ci.sp.obj, type="shape", col="blue")
##D 
##D # Direct syntax (response, predictor):
##D plot.roc(aSAH$outcome, aSAH$s100b,
##D          ci=TRUE, of="thresholds")
## End(Not run)



cleanEx()
nameEx("plot.roc")
### * plot.roc

flush(stderr()); flush(stdout())

### Name: plot.roc
### Title: Plot a ROC curve
### Aliases: plot.roc plot.roc.roc plot.smooth.roc plot.roc.smooth.roc
###   plot.roc.default plot.roc.formula
### Keywords: univar nonparametric utilities aplot hplot roc

### ** Examples

data(aSAH)

# Syntax (response, predictor):
plot.roc(aSAH$outcome, aSAH$s100b)

# With a roc object:
rocobj <- roc(aSAH$outcome, aSAH$s100b)
# identical:
plot(rocobj)
plot.roc(rocobj)

# Add a smoothed ROC:
plot.roc(smooth(rocobj), add=TRUE, col="blue")
legend("bottomright", legend=c("Empirical", "Smoothed"),
       col=c(par("fg"), "blue"), lwd=2)

# With more options:
plot(rocobj, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="blue", print.thres=TRUE)

# To plot a different partial AUC, we need to ignore the existing value
# with reuse.auc=FALSE:
plot(rocobj, print.auc=TRUE, auc.polygon=TRUE, partial.auc=c(1, 0.8),
     partial.auc.focus="se", grid=c(0.1, 0.2), grid.col=c("green", "red"),
     max.auc.polygon=TRUE, auc.polygon.col="blue", print.thres=TRUE,
     reuse.auc=FALSE)

# Add a line to the previous plot:
plot.roc(aSAH$outcome, aSAH$wfns, add=TRUE)

# Alternatively, you can get the plot directly from roc():
roc(aSAH$outcome, aSAH$s100b, plot=TRUE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("print")
### * print

flush(stderr()); flush(stdout())

### Name: print
### Title: Print a ROC curve object
### Aliases: print.roc print.smooth.roc print.multiclass.roc print.ci.auc
###   print.ci.thresholds print.ci.se print.ci.sp print.auc
###   print.multiclass.auc
### Keywords: univar nonparametric utilities print roc

### ** Examples

data(aSAH)

# Print a roc object:
rocobj <- roc(aSAH$outcome, aSAH$s100b)
print(rocobj)

# Print a smoothed roc object
print(smooth(rocobj))

# implicit printing
 roc(aSAH$outcome, aSAH$s100b)

# Print an auc and a ci object, from the ROC object or calling
# the dedicated function:
print(rocobj$auc)
print(ci(rocobj))



cleanEx()
nameEx("roc")
### * roc

flush(stderr()); flush(stdout())

### Name: roc
### Title: Build a ROC curve
### Aliases: roc roc.formula roc.default
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

# Basic example
roc(aSAH$outcome, aSAH$s100b,
    levels=c("Good", "Poor"))
# As levels aSAH$outcome == c("Good", "Poor"),
# this is equivalent to:
roc(aSAH$outcome, aSAH$s100b)
# In some cases, ignoring levels could lead to unexpected results
# Equivalent syntaxes:
roc(outcome ~ s100b, aSAH)
roc(aSAH$outcome ~ aSAH$s100b)
with(aSAH, roc(outcome, s100b))
with(aSAH, roc(outcome ~ s100b))

# With a formula:
roc(outcome ~ s100b, data=aSAH)

# With controls/cases
roc(controls=aSAH$s100b[aSAH$outcome=="Good"], cases=aSAH$s100b[aSAH$outcome=="Poor"])

# Inverted the levels: "Poor" are now controls and "Good" cases:
roc(aSAH$outcome, aSAH$s100b,
    levels=c("Poor", "Good"))

# The result was exactly the same because of direction="auto".
# The following will give an AUC < 0.5:
roc(aSAH$outcome, aSAH$s100b,
    levels=c("Poor", "Good"), direction="<")

# If we prefer counting in percent:
roc(aSAH$outcome, aSAH$s100b, percent=TRUE)

# Plot and CI (see plot.roc and ci for more options):
roc(aSAH$outcome, aSAH$s100b,
    percent=TRUE, plot=TRUE, ci=TRUE)

# Smoothed ROC curve
roc(aSAH$outcome, aSAH$s100b, smooth=TRUE)
# this is not identical to
smooth(roc(aSAH$outcome, aSAH$s100b))
# because in the latter case, the returned object contains no AUC



cleanEx()
nameEx("roc.test")
### * roc.test

flush(stderr()); flush(stdout())

### Name: roc.test
### Title: Compare the AUC of two ROC curves
### Aliases: roc.test roc.test.default roc.test.roc roc.test.formula
###   roc.test.auc roc.test.smooth.roc
### Keywords: multivariate nonparametric utilities htest roc

### ** Examples

data(aSAH)

# Basic example with 2 roc objects
roc1 <- roc(aSAH$outcome, aSAH$s100b)
roc2 <- roc(aSAH$outcome, aSAH$wfns)
roc.test(roc1, roc2)

## Not run: 
##D # The latter used Delong's test. To use bootstrap test:
##D roc.test(roc1, roc2, method="bootstrap")
##D # Increase boot.n for a more precise p-value:
##D roc.test(roc1, roc2, method="bootstrap", boot.n=10000)
## End(Not run)

# Alternative syntaxes
roc.test(aSAH$outcome, aSAH$s100b, aSAH$wfns)
roc.test(aSAH$outcome, data.frame(aSAH$s100b, aSAH$wfns))

# If we had a good a priori reason to think that wfns gives a
# better classification than s100b (in other words, AUC of roc1
# should be lower than AUC of roc2):
roc.test(roc1, roc2, alternative="less")

## Not run: 
##D # Comparison can be done on smoothed ROCs
##D # Smoothing is re-done at each iteration, and execution is slow
##D roc.test(smooth(roc1), smooth(roc2))
##D # or:
##D roc.test(aSAH$outcome, aSAH$s100b, aSAH$wfns, smooth=TRUE, boot.n=100)
## End(Not run)
# or from an AUC (no smoothing)
roc.test(auc(roc1), roc2)

## Not run: 
##D # Comparison of partial AUC:
##D roc3 <- roc(aSAH$outcome, aSAH$s100b, partial.auc=c(1, 0.8), partial.auc.focus="se")
##D roc4 <- roc(aSAH$outcome, aSAH$wfns, partial.auc=c(1, 0.8), partial.auc.focus="se")
##D roc.test(roc3, roc4)
##D # This is strictly equivalent to:
##D roc.test(roc3, roc4, method="bootstrap")
##D 
##D # Alternatively, we could re-use roc1 and roc2 to get the same result:
##D roc.test(roc1, roc2, reuse.auc=FALSE, partial.auc=c(1, 0.8), partial.auc.focus="se")
##D 
##D # Comparison on specificity and sensitivity
##D roc.test(roc1, roc2, method="specificity", specificity=0.9)
##D roc.test(roc1, roc2, method="sensitivity", sensitivity=0.9)
## End(Not run)

# Spurious use of DeLong's test with different direction:
roc5 <- roc(aSAH$outcome, aSAH$s100b, direction="<")
roc6 <- roc(aSAH$outcome, aSAH$s100b, direction=">")
roc.test(roc5, roc6, method="delong")

## Not run: 
##D # Comparisons of the ROC curves
##D roc.test(roc1, roc2, method="venkatraman")
## End(Not run)

# Unpaired tests
roc7 <- roc(aSAH$outcome, aSAH$s100b)
# artificially create an roc8 unpaired with roc7
roc8 <- roc(aSAH$outcome[1:100], aSAH$s100b[1:100])
## Not run: 
##D roc.test(roc7, roc8, paired=FALSE, method="delong")
##D roc.test(roc7, roc8, paired=FALSE, method="bootstrap")
##D roc.test(roc7, roc8, paired=FALSE, method="venkatraman")
##D roc.test(roc7, roc8, paired=FALSE, method="specificity", specificity=0.9)
## End(Not run)



cleanEx()
nameEx("smooth.roc")
### * smooth.roc

flush(stderr()); flush(stdout())

### Name: smooth.roc
### Title: Smooth a ROC curve
### Aliases: smooth smooth.roc smooth.smooth.roc smooth.default
### Keywords: univar nonparametric utilities roc smooth

### ** Examples

data(aSAH)

##  Basic example
rocobj <- roc(aSAH$outcome, aSAH$s100b)
smooth(rocobj)
# or directly with roc()
roc(aSAH$outcome, aSAH$s100b, smooth=TRUE)

# plotting
plot(rocobj)
rs <- smooth(rocobj, method="binormal")
plot(rs, add=TRUE, col="green")
rs2 <- smooth(rocobj, method="density")
plot(rs2, add=TRUE, col="blue")
rs3 <- smooth(rocobj, method="fitdistr", density="lognormal")
plot(rs3, add=TRUE, col="magenta")
rs4 <- smooth(rocobj, method="logcondens")
plot(rs4, add=TRUE, col="brown")
rs5 <- smooth(rocobj, method="logcondens.smooth")
plot(rs5, add=TRUE, col="orange")
legend("bottomright", legend=c("Empirical", "Binormal", "Density", "Log-normal",
                               "Log-concave density", "Smoothed log-concave density"),
       col=c("black", "green", "blue", "magenta", "brown", "orange"), lwd=2)

## Advanced smoothing

# if we know the distributions are normal with sd=0.1 and an unknown mean:
smooth(rocobj, method="fitdistr", density=dnorm, start=list(mean=1), sd=.1)
# different distibutions for controls and cases:
smooth(rocobj, method="fitdistr", density.controls="normal", density.cases="lognormal")

# with densities
bw <- bw.nrd0(rocobj$predictor)
density.controls <- density(rocobj$controls, from=min(rocobj$predictor) - 3 * bw,
                            to=max(rocobj$predictor) + 3*bw, bw=bw, kernel="gaussian")
density.cases <- density(rocobj$cases, from=min(rocobj$predictor) - 3 * bw,
                            to=max(rocobj$predictor) + 3*bw, bw=bw, kernel="gaussian")
smooth(rocobj, method="density", density.controls=density.controls$y, 
       density.cases=density.cases$y)
# which is roughly what is done by a simple:
smooth(rocobj, method="density")

# With logcondens:
smooth(rocobj, method="logcondens")
smooth(rocobj, method="logcondens.smooth")

## Not run: 
##D ## Smoothing artificial ROC curves
##D 
##D rand.unif <- runif(1000, -1, 1)
##D rand.exp <- rexp(1000)
##D rand.norm <- 
##D rnorm(1000)
##D 
##D # two normals
##D roc.norm <- roc(controls=rnorm(1000), cases=rnorm(1000)+1, plot=TRUE)
##D plot(smooth(roc.norm), col="green", lwd=1, add=TRUE)
##D plot(smooth(roc.norm, method="density"), col="red", lwd=1, add=TRUE)
##D plot(smooth(roc.norm, method="fitdistr"), col="blue", lwd=1, add=TRUE)
##D plot(smooth(roc.norm, method="logcondens"), col="brown", lwd=1, add=TRUE)
##D plot(smooth(roc.norm, method="logcondens.smooth"), col="orange", lwd=1, add=TRUE)
##D legend("bottomright", legend=c("empirical", "binormal", "density", "fitdistr",
##D                                "logcondens", "logcondens.smooth"), 
##D        col=c(par("fg"), "green", "red", "blue", "brown", "orange"), lwd=c(2, 1, 1, 1))
##D        
##D # deviation from the normality
##D roc.norm.exp <- roc(controls=rnorm(1000), cases=rexp(1000), plot=TRUE)
##D plot(smooth(roc.norm.exp), col="green", lwd=1, add=TRUE)
##D plot(smooth(roc.norm.exp, method="density"), col="red", lwd=1, add=TRUE)
##D # Wrong fitdistr: normality assumed by default
##D plot(smooth(roc.norm.exp, method="fitdistr"), col="blue", lwd=1, add=TRUE)
##D # Correct fitdistr
##D plot(smooth(roc.norm.exp, method="fitdistr", density.controls="normal",
##D             density.cases="exponential"), col="purple", lwd=1, add=TRUE)
##D plot(smooth(roc.norm.exp, method="logcondens"), col="brown", lwd=1, add=TRUE)
##D plot(smooth(roc.norm.exp, method="logcondens.smooth"), col="orange", lwd=1, add=TRUE)
##D legend("bottomright", legend=c("empirical", "binormal", "density",
##D                                "wrong fitdistr", "correct fitdistr",
##D                                "logcondens", "logcondens.smooth"),
##D        col=c(par("fg"), "green", "red", "blue", "purple", "brown", "orange"), lwd=c(2, 1, 1, 1, 1))
##D 
##D # large deviation from the normality
##D roc.unif.exp <- roc(controls=runif(1000, 2, 3), cases=rexp(1000)+2, plot=TRUE)
##D plot(smooth(roc.unif.exp), col="green", lwd=1, add=TRUE)
##D plot(smooth(roc.unif.exp, method="density"), col="red", lwd=1, add=TRUE)
##D plot(smooth(roc.unif.exp, method="density", bw="ucv"), col="magenta", lwd=1, add=TRUE)
##D # Wrong fitdistr: normality assumed by default (uniform distributions not handled)
##D plot(smooth(roc.unif.exp, method="fitdistr"), col="blue", lwd=1, add=TRUE)
##D plot(smooth(roc.unif.exp, method="logcondens"), col="brown", lwd=1, add=TRUE)
##D plot(smooth(roc.unif.exp, method="logcondens.smooth"), col="orange", lwd=1, add=TRUE)
##D legend("bottomright", legend=c("empirical", "binormal", "density",
##D                                "density ucv", "wrong fitdistr",
##D                                "logcondens", "logcondens.smooth"),
##D        col=c(par("fg"), "green", "red", "magenta", "blue", "brown", "orange"), lwd=c(2, 1, 1, 1, 1))
## End(Not run)

# 2 uniform distributions with a custom density function
unif.density <- function(x, n, from, to, bw, kernel, ...) {
  smooth.x <- seq(from=from, to=to, length.out=n)
  smooth.y <- dunif(smooth.x, min=min(x), max=max(x))
  return(smooth.y)
}
roc.unif <- roc(controls=runif(1000, -1, 1), cases=runif(1000, 0, 2), plot=TRUE)
s <- smooth(roc.unif, method="density", density=unif.density)
plot(roc.unif)
plot(s, add=TRUE, col="grey")

## Not run: 
##D # you can bootstrap a ROC curve smoothed with a density function:
##D ci(s, boot.n=100)
## End(Not run)



cleanEx()
nameEx("var")
### * var

flush(stderr()); flush(stdout())

### Name: var.roc
### Title: Variance of a ROC curve
### Aliases: var var.roc var.auc var.smooth.roc var.default
### Keywords: univar nonparametric utilities roc

### ** Examples

data(aSAH)

##  Basic example
roc1 <- roc(aSAH$outcome, aSAH$s100b)
roc2 <- roc(aSAH$outcome, aSAH$wfns)
var(roc1)
var(roc2)

# We could also write it in one line:
var(roc(aSAH$outcome, aSAH$s100b))

## Not run: 
##D # The latter used Delong. To use bootstrap:
##D var(roc1, method="bootstrap")
##D # Decrease boot.n for a faster execution
##D var(roc1,method="bootstrap", boot.n=1000)
## End(Not run)

## Not run: 
##D # Variance of smoothed ROCs:
##D # Smoothing is re-done at each iteration, and execution is slow
##D var(smooth(roc1))
## End(Not run)

# or from an AUC (no smoothing)
var(auc(roc1))

## Test data from Hanley and Hajian-Tilaki, 1997
disease.present <- c("Yes", "No", "Yes", "No", "No", "Yes", "Yes", "No", "No", "Yes", "No", "No", "Yes", "No", "No")
field.strength.1 <- c(1, 2, 5, 1, 1, 1, 2, 1, 2, 2, 1, 1, 5, 1, 1)
field.strength.2 <- c(1, 1, 5, 1, 1, 1, 4, 1, 2, 2, 1, 1, 5, 1, 1)
roc3 <- roc(disease.present, field.strength.1)
roc4 <- roc(disease.present, field.strength.2)
# Assess the variance:
var(roc3)
var(roc4)

## Not run: 
##D # With bootstrap:
##D var(roc3, method="bootstrap")
##D var(roc4, method="bootstrap")
## End(Not run)




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
