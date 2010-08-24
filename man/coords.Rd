\encoding{UTF-8}
\name{coords}
\alias{coords}
\alias{coords.roc}
\alias{coords.smooth.roc}

\title{
  Coordinates of a ROC curve
}
\description{
  This function returns the coordinates of the ROC curve at the
  specified point.
}
\usage{
coords(...)
\S3method{coords}{roc}(roc, x, input=c("threshold", "specificity",
"sensitivity"), ret=c("threshold", "specificity", "sensitivity"),
as.list=FALSE, ...)
\S3method{coords}{smooth.roc}(smooth.roc, x, input=c("specificity",
"sensitivity"), ret=c("specificity", "sensitivity"), as.list=FALSE, ...)
}
		   
\arguments{
  \item{roc, smooth.roc}{a \dQuote{roc} object from the
	\code{\link{roc}} function, or a \dQuote{smooth.roc} object from the
	\code{\link{smooth.roc}} function.
  }
  \item{x}{
		the coordinates to look for. Numeric (if so, their meaning is
		defined by the \code{input} argument) or one of \dQuote{all} (all
		the points of the ROC curve), \dQuote{local maximas} (the local
		maximas of the ROC curve) or \dQuote{best} (the point with the best
		sum of sensitivity and specificity).
  }
  \item{input}{
		If \code{x} is numeric, the kind of input coordinate (x). One of
		\dQuote{threshold}, \dQuote{specificity} or
    \dQuote{sensitivity}. Can be shortenend (for example to
    \dQuote{thr}, \dQuote{sens} and \dQuote{spec}, or even to
    \dQuote{t}, \dQuote{se} and \dQuote{sp}). Note that
		\dQuote{threshold} is not allowed in
		\code{coords.smooth.roc}, and that the argument is ignored when
		\code{x} is a character.
  }
  \item{ret}{The coordinates to return. One or more of
    \dQuote{threshold}, \dQuote{specificity} or
    \dQuote{sensitivity}. Can be shortenend (for example to
    \dQuote{thr}, \dQuote{sens} and \dQuote{spec}, or even to
    \dQuote{t}, \dQuote{se} and \dQuote{sp}). Note that
		\dQuote{threshold} is not allowed in \code{coords.smooth.roc}.
  }
  \item{as.list}{If the returned object must be a list. If \code{FALSE}
    (default), a named numeric vector is returned.}
  \item{\dots}{further arguments passed to or from other methods. Ignored.}
}

\details{
  This function takes a \dQuote{roc}  or \dQuote{smooth.roc} object as
  first argument, on which the coordinates will be determined. The
  coordinates are defined by the \code{x} and \code{input}
  arguments. \dQuote{threshold} coordinates cannot be determined in a
  smoothed ROC.

  If \code{input="threshold"}, the coordinates for the threshold
  are reported, even if the exact threshold do not define the ROC
  curve. The following convenience characters are allowed: \dQuote{all},
  \dQuote{local maximas} and \dQuote{best}. They will return all the
  thresholds, only the thresholds defining local maximas (upper angles of the
  ROC curve), or only the threshold(s) corresponding to the best sum of
  sensitivity + specificity respectively. Note that \dQuote{best} can
  return more than one threshold. If \code{x} is a character, the
  coordinates are limited to the thresholds within the partial AUC if it
  has been defined, and not necessarily to the whole curve.
  
  For \code{input="specificity"} and \code{input="sensitivity"},
  the function checks if the specificity or sensitivity is one of the
  points of the ROC curve (in \code{roc$sensitivities} or
  \code{roc$specificities}). More than one point may match (in
  \emph{step} curves), then only the upper-left-most point coordinates
  are returned. Otherwise,
  the specificity and specificity of the point is interpolated and
  \code{NA} is returned as threshold.

  The coords function in this package is a generic, but it might be
  superseded by functions in other packages such as
  \pkg{colorspace} or \pkg{spatstat} if they are loaded after
  \pkg{pROC}. In this case, call the \code{coords.roc} or
  \code{coords.smooth.roc} functions directly.
}

\value{
  Depending on the length of \code{x} and \code{as.list} argument.

  
  \tabular{lll}{
	\tab
	length(x) == 1 \tab
	length(x) > 1
	\cr
	
    \code{as.list=TRUE} \tab
	a list of the length of, in the order of, and named after, \code{ret}. \tab
	a list of the length of, and named after, \code{x}. Each element of this list is a list of the length of, in the order of, and named after, \code{ret}. \cr

    \code{as.list=FALSE}  \tab
	a numeric vector of the length of, in the order of, and named after, \code{ret}.\tab
	a numeric matrix with one row for each \code{ret} and one column for each \code{x}\cr
  }
  
  In all cases if \code{input="specificity"} or \code{input="sensitivity"}
  and interpolation was required, threshold is returned as \code{NA}.

  Note that if giving a character as \code{x} (\dQuote{all},
  \dQuote{local maximas} or \dQuote{best}), you cannot predict the
  dimension of the return value, so be prepared to receive either a
  numeric vector or a matrix (if \code{as.list=FALSE}) or either a list
  of numeric or a list of lists (if \code{as.list=TRUE}). Even
  \dQuote{best} may return more than one value (for example if the ROC
  curve is below the identity line, both extreme points).
}

\seealso{
\code{\link{roc}}
}
\examples{
data(aSAH)

# Print a roc object:
rocobj <- roc(aSAH$outcome, aSAH$s100b)

coords(rocobj, 0.55)
coords(rocobj, 0.9, "specificity", as.list=TRUE)
coords(rocobj, 0.5, "se", ret="se")
# fully qualified but identical:
coords(roc=rocobj, x=0.5, input="sensitivity", ret="sensitivity")

# Same in percent
rocobj <- roc(aSAH$outcome, aSAH$s100b,
              percent=TRUE)

coords(rocobj, 0.55)
coords(rocobj, 90, "specificity", as.list=TRUE)
coords(rocobj, x=50, input="sensitivity", ret=c("sen", "spec"))

# Get the sensitivities for all thresholds
sensitivities <- coords(rocobj, rocobj$thresholds, "thr", "se")
# This is equivalent to taking sensitivities from rocobj directly
stopifnot(all.equal(as.vector(rocobj$sensitivities), as.vector(sensitivities)))
# You could also write:
sensitivities <- coords(rocobj, "all", ret="se")
stopifnot(all.equal(as.vector(rocobj$sensitivities), as.vector(sensitivities)))

# Get the best threshold
coords(rocobj, "b", ret="t")
}

\keyword{univar}
\keyword{nonparametric}
\keyword{utilities}
\keyword{roc}
