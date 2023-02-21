\name{summary.forecast.lpacf}
\alias{summary.forecast.lpacf}
\title{Print out summary information about a \code{forecast.lpacf} object
}
\description{Print out summary information about a \code{forecast.lpacf} object.
}
\usage{
\method{summary}{forecast.lpacf}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{The object you want to print out summary info for.
}
  \item{\dots}{Other arguments
}
}
\details{Prints out the maximum number of steps ahead considered in the
	object, prints out the first few predictions (up to 6), and
	their standard errors. The smoothing binwidth associated with
	the localized partial autocorrelation object used to compute the
	predictions is printed. The order, p, of the localized partial
	autocorrelation is printed. A note of whether differencing was
	actioned is printed.
}
\value{None
}
\references{
OUR PAPER HERE
}
\author{
Guy Nason
}

\seealso{\code{\link{forecast.lpacf}}, \code{\link{print.forecast.lpacf}}}
\examples{
#
# Example for print.forecast.lpacf contains a call to summary.forecast.lpacf
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
