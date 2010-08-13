\encoding{UTF-8}
\name{roc.test}
\alias{roc.test}
\alias{roc.test.default}
\alias{roc.test.roc}
\alias{roc.test.formula}
\alias{roc.test.auc}
\alias{roc.test.smooth.roc}

\title{
  Compare the AUC of two correlated ROC curves
}
\description{
  This function compares the AUC or partial AUC of two correlated (or paired) ROC
  curves. Several syntaxes are available: two object of class roc (which must
  have the same response data, but can be AUC or smoothed ROC), or either three vectors (response,
  predictor1, predictor2) or a response vector and a matrix or
  data.frame with two columns (predictors).
}
\usage{
roc.test(...)
\S3method{roc.test}{roc}(roc1, roc2, method=c("delong", "bootstrap",
"venkatraman"), alternative = c("two.sided", "less", "greater"),
reuse.auc=TRUE, boot.n=2000, boot.stratified=TRUE, ties.method="first",
progress=getOption("pROCProgress")$name, ...)
\S3method{roc.test}{auc}(roc1, roc2, ...)
\S3method{roc.test}{smooth.roc}(roc1, roc2, ...)
\S3method{roc.test}{formula}(formula, data, ...)
\S3method{roc.test}{default}(response, predictor1, predictor2=NULL,
na.rm=TRUE, method=NULL, ...)
}

\arguments{
  \item{roc1, roc2}{the two ROC curves to compare. Either
	\dQuote{\link{roc}}, \dQuote{\link{auc}} or
	\dQuote{\link{smooth.roc}} objects (types can be mixed as long as
	the original ROC curve are paired).
  }
  \item{response}{a vector or factor, as for the \link{roc} function.}
  \item{predictor1}{a numeric or ordered vector as for the \link{roc} function, or
  a matrix or data.frame with predictors two colums.}
  \item{predictor2}{only if predictor1 was a vector, the second
    predictor as a numeric vector.
  }
  \item{formula}{a formula of the type response~predictor1+predictor2.}
  \item{data}{a matrix or data.frame containing the variables in the
    formula. See \code{\link{model.frame}} for more details.}
  \item{na.rm}{if \code{TRUE}, the observations with \code{NA} values
    will be removed.
  }
  \item{method}{the method to use, either \dQuote{delong},
    \dQuote{bootstrap} or \dQuote{venkatraman}. The first letter is sufficient. If omitted, the
	appropriate method is selected as explained in details.
  }
  \item{alternative}{specifies the alternative hypothesis. Either of
    \dQuote{two.sided}, \dQuote{less} or \dQuote{greater}. The first letter is
    sufficient. Default: \dQuote{two.sided}. Only \dQuote{two.sided} is available
    with \code{method="venkatraman"}.
  }
  \item{reuse.auc}{if \code{TRUE} (default) and the \dQuote{roc} objects
    contain an \dQuote{auc} field, re-use these specifications for the
    test. See details.
  }
  \item{boot.n}{for \code{method="bootstrap"} and \code{method="venkatraman"} only: the number of
    bootstrap replicates or permutations. Default: \var{2000}.
  }
  \item{boot.stratified}{for \code{method="bootstrap"} only:
    should the bootstrap be stratified (same number 
    of cases/controls in each replicate than in the original sample) or
    not. Ignored with \code{method="venkatraman"}. Default: \var{TRUE}.
  }
  \item{ties.method}{for \code{method="venkatraman"} only: argument for
    \code{\link{rank}} specifying how ties are handled. Defaults to
    \dQuote{first} as described in the paper.
  }
  \item{progress}{the name of progress bar to display. Typically
    \dQuote{none}, \dQuote{win}, \dQuote{tk} or \dQuote{text} (see the
    \code{name} argument to \code{\link[plyr]{create_progress_bar}} for
    more information), but a list as returned by \code{\link[plyr]{create_progress_bar}}
    is also accepted. See also the \dQuote{Progress bars} section of
    \link[=pROC-package]{this package's documentation}.
  }
  \item{\dots}{further arguments passed to or from other methods,
    especially arguments for \code{\link{roc}} and \code{roc.test.roc}
    when calling \code{roc.test.default} or \code{roc.test.formula}.
    Arguments for \code{\link{auc}},
    and \code{\link{txtProgressBar}} (only \code{char} and \code{style})
    if applicable.
  }
}

\details{
  This function compares two ROC curves. It is typically called with the two \link{roc} objects to
  compare. \code{roc.test.default} is provided as a convenience
  method and creates two \link{roc} objects before calling
  \code{roc.test.roc}.

  Three methods are available: \dQuote{delong}, \dQuote{bootstrap} and \dQuote{venkatraman} (see
  \dQuote{Computational details} section below). \dQuote{delong} and
  \dQuote{bootstrap} are tests over the AUC whereas \dQuote{venkatraman}
  compares the the ROC curves themselves.

  Default is to use \dQuote{delong} method except for comparison of partial AUC, smoothed
  curves and curves with different \code{direction}, where \code{bootstrap}
  is used. Using \dQuote{delong} for partial AUC and smoothed ROCs is not
  supported in pROC (a warning is produced and \dQuote{bootstrap} is employed instead).
  It is spurious to use \dQuote{delong} for \code{\link{roc}} with different
  \code{direction} (a warning is issued but the spurious comparison is
  enforced). \dQuote{venkatraman}'s test cannot be employed to compare smoothed
  ROC curves. Additionally, partial AUC specifications are ignored (with
  a warning), and comparison of ROC curves with different
  \code{direction} should be used with care (a warning is produced as well).

  If \code{alternative="two.sided"}, a two-sided test for difference in AUC is performed. If
  \code{alternative="less"}, the alternative is that the AUC of roc1 is
  smaller than the AUC of roc2. For \code{method="venkatraman"}, only
  \dQuote{two.sided} test is available.

  For \link[=smooth.roc]{smoothed ROC curves}, smoothing is performed again at each
  bootstrap replicate with the parameters originally provided.
  If a density smoothing was performed with user-provided
  \code{density.cases} or \code{density.controls} the bootstrap cannot
  be performed and an error is issued.
}

\section{AUC specification}{
  The comparison of the AUC of the ROC curves needs a specification of the
  AUC. The specification is defined by:
  \enumerate{
    \item the \dQuote{auc} field in the \dQuote{\link{roc}} objects if
    \code{reuse.auc} is set to \code{TRUE} (default)
    \item passing the specification to \code{\link{auc}} with \dots
    (arguments \code{partial.auc}, \code{partial.auc.correct} and
    \code{partial.auc.focus}). In this case, you must ensure either that
    the \code{\link{roc}} object do not contain an \code{auc} field (if
    you called \code{\link{roc}} with \code{auc=FALSE}), or set
    \code{reuse.auc=FALSE}.
  }

  If \code{reuse.auc=FALSE} the \code{\link{auc}} function will always
  be called with \code{\dots} to determine the specification, even if
  the \dQuote{\link{roc}} objects do contain an \code{auc} field.

  As well if the \dQuote{\link{roc}} objects do not contain an \code{auc}
  field, the \code{\link{auc}} function will always be called with
  \code{\dots} to determine the specification.

  The AUC specification is ignored in the Venkatraman test.

  Warning: if the roc object passed to roc.test contains an \code{auc}
  field and \code{reuse.auc=TRUE}, \link{auc} is not called and
  arguments such as \code{partial.auc} are silently ignored.
}


\section{Computation details}{
  With \code{method="bootstrap"}, the processing is done as follow:
  \enumerate{
    \item \code{boot.n} bootstrap replicates are drawn from the
    data. If \code{stratified} is \var{TRUE}, each replicate contains
    exactly the same number of controls and cases than the original
    sample, otherwise if \var{FALSE} the numbers can vary.
    \item for each bootstrap replicate, the AUC of the two ROC curves
    are computed and the difference is stored.
    \item The following formula is used:
    \deqn{D=\frac{AUC1-AUC2}{s}}{D=(AUC1-AUC2)/s}
    where s is the standard deviation of
    the bootstrap differences and AUC1 and AUC2 the AUC of the two
    (original) ROC curves.
    \item \var{D} is then compared to the normal distribution,
    according to the value of \code{alternative}.
  }
  
  With \code{method="delong"}, the processing is done as described in
  DeLong \emph{et al.} (1988). Only comparison of two ROC curves is
  implemented.

  With \code{method="venkatraman"}, the processing is done as described
  in Venkatraman and Begg (1996) with \code{boot.n} permutation of sample ranks.
}

\section{Warnings}{
  If \dQuote{auc} specifications are different in both roc objects, the
  warning \dQuote{Different AUC specifications in the ROC
    curves. Enforcing the inconsistency, but unexpected results may be
    produced.} is issued. Unexpected results may be produced.

  If \code{method="delong"} and the AUC specification specifies a
  partial AUC, the warning \dQuote{Using DeLong's test for partial AUC is
    not supported. Using bootstrap test instead.} is issued. The
  \code{method} argument is ignored and \dQuote{bootstrap} is used instead.

  If \code{method="delong"} and the ROC curve is smoothed, the warning
  \dQuote{Using DeLong's test for smoothed ROCs is not supported. Using
    bootstrap test instead.} is issued. The \code{method} argument is
  ignored and \dQuote{bootstrap} is used instead.

  If \code{method="venkatraman"}, and the AUC specification specifies a
  partial AUC, the AUC specification is ignored with the warning
  \dQuote{Partial AUC is ignored in Venkatraman's test.}.

  If \code{method="venkatraman"}, and \code{alternative} is
  \dQuote{less} or \dQuote{greater}, the warning \dQuote{Only two-sided
    tests are available for Venkatraman.} is produced and a two tailed
  test is performed instead.

  Both DeLong and Venkatraman's test ignores the direction of the ROC curve so that if two
  ROC curves have a different differ in the value of
  \code{direction}, the warning \dQuote{(DeLong|Venkatraman)'s test should not be
    applied to ROC curves with a different direction.} is
  printed. However, the spurious test is enforced.

  If \code{boot.stratified=FALSE} and the sample has a large imbalance between
  cases and controls, it could happen that one or more of the replicates
  contains no case or control observation, or that there are not enough
  points for smoothing, producing a \code{NA} area.
  The warning \dQuote{NA value(s) produced during bootstrap were ignored.}
  will be issued and the observation will be ignored. If you have a large
  imbalance in your sample, it could be safer to keep
  \code{boot.stratified=TRUE}.
}

\section{Errors}{
  Both Delong and Bootstrap tests work only on paired data. This
  assumption is enforced by the verification that the \code{response}s
  of \code{roc1} and \code{roc2} are \link{identical}. If they are found
  different, and the difference cannot be explained by missing values,
  the error \dQuote{The ROC test is defined only on paired ROC curves}
  is produced.

  An error will also occur if you give a \code{predictor2} when
  \code{predictor1} is a \code{\link{matrix}} or a
  \code{\link{data.frame}}, if \code{predictor1} has more than two
  columns, or if you do not give a \code{predictor2} when
  \code{predictor1} is a vector.
  
  If \code{density.cases} and \code{density.controls} were provided
  for smoothing, the error \dQuote{Cannot compute the statistic on ROC
    curves smoothed with density.controls and density.cases.} is
  issued. 

  With \code{method="venkatraman"}, the processing is done as described
  in Venkatraman and Begg (1996) with \code{boot.n} permutation of
  sample ranks (with ties breaking). For consistency reasons, the same argument \code{boot.n} as
  in bootstrap defines the number of permutations to execute,
  even though no bootstrap is performed.

}

\value{
  A list of class "htest" with following content:
  \item{p.value}{the p-value of the test.}
  \item{statistic}{the value of the Z (\code{method="delong"}) or D
    (\code{method="bootstrap"}) statistics.
  }
  \item{alternative}{the alternative hypothesis.}
  \item{method}{the character string \dQuote{DeLong's test for two
      correlated ROC curves} (if \code{method="delong"}) or
    \dQuote{Bootstrap test for two correlated ROC curves} (if
    \code{method="bootstrap"}).
  }
  \item{null.value}{the expected value of the statistic under the null
    hypothesis, that is 0.}
  \item{estimate}{the AUC in the two ROC curves.}
  \item{data.name}{the names of the data that was used.}
  \item{parameter}{for \code{method="bootstrap"} only: the values of the
    \code{boot.n} and \code{boot.stratified} arguments.
  }
}

\section{Acknowledgements}{
  We would like to thank E. S. Venkatraman and Colin B. Begg for their
  support in the implementation of their test.
}

\references{
  Elisabeth R. DeLong, David M. DeLong and Daniel L. Clarke-Pearson
  (1988) ``Comparing the areas under two or more correlated receiver
  operating characteristic curves: a nonparametric
  approach''. \emph{Biometrics} \bold{44}, 837--845.
  
  James A. Hanley and Barbara J. McNeil (1982) ``The meaning and use of
  the area under a receiver operating characteristic (ROC)
  curve''. \emph{Radiology} \bold{143}, 29--36.
  
  Xavier Robin, Natacha Turck, Jean-Charles Sanchez and Markus Müller
  (2009) ``Combination of protein biomarkers''. \emph{useR! 2009}, Rennes.
  \url{http://www.agrocampus-ouest.fr/math/useR-2009/abstracts/user_author.html}

  E. S. Venkatraman and Colin B. Begg (1996) ``A distribution-free
  procedure for comparing receiver operating characteristic curves from
  a paired experiment''. \emph{Biometrika} \bold{83}, 835--848.
  DOI: 10.1093/biomet/83.4.835
}

\seealso{
  \code{\link{roc}}
}

\examples{
data(aSAH)

# Basic example with 2 roc objects
roc1 <- roc(aSAH$outcome, aSAH$s100b)
roc2 <- roc(aSAH$outcome, aSAH$wfns)
roc.test(roc1, roc2)

#@#\dontrun{
# The latter used Delong's test. To use bootstrap test:
roc.test(roc1, roc2, method="bootstrap")
# Increase boot.n for a more precise p-value:
roc.test(roc1, roc2, method="bootstrap", boot.n=10000)
#@#}

# Alternative syntaxes
roc.test(aSAH$outcome, aSAH$s100b, aSAH$wfns)
roc.test(aSAH$outcome, data.frame(aSAH$s100b, aSAH$wfns))

# If we had a good a priori reason to think that wfns gives a
# better classification than s100b (in other words, AUC of roc1
# should be lower than AUC of roc2):
roc.test(roc1, roc2, alternative="less")

#@#\dontrun{
# Comparison can be done on smoothed ROCs
# Smoothing is re-done at each iteration, and execution is slow
roc.test(smooth(roc1), smooth(roc2))
# or:
roc.test(aSAH$outcome, aSAH$s100b, aSAH$wfns, smooth=TRUE,
smooth.method="density", boot.n=100)
#@#}
# or from an AUC (no smoothing)
roc.test(auc(roc1), roc2)

#@#\dontrun{
# Comparison of partial AUC:
roc3 <- roc(aSAH$outcome, aSAH$s100b, partial.auc=c(1, 0.8), partial.auc.focus="se")
roc4 <- roc(aSAH$outcome, aSAH$wfns, partial.auc=c(1, 0.8), partial.auc.focus="se")
roc.test(roc3, roc4)
# This is strictly equivalent to:
roc.test(roc3, roc4, method="bootstrap")

# Alternatively, we could re-use roc1 and roc2 to get the same result:
roc.test(roc1, roc2, reuse.auc=FALSE, partial.auc=c(1, 0.8), partial.auc.focus="se")
#@#}

# Spurious use of DeLong's test with different direction:
roc5 <- roc(aSAH$outcome, aSAH$s100b, direction="<")
roc6 <- roc(aSAH$outcome, aSAH$s100b, direction=">")
roc.test(roc5, roc6, method="delong")

# Comparisons of the ROC curves
roc.test(roc1, roc2, method="venkatraman")
}

\keyword{multivariate}
\keyword{nonparametric}
\keyword{utilities}
\keyword{htest}
\keyword{roc}
