\name{print.forecast.lpacf}
\alias{print.forecast.lpacf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prints a \code{forecast.lpacf} object
}
\description{Prints a \code{forecast.lpacf} object, basically telling
	you what's there.
}
\usage{
\method{print}{forecast.lpacf}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The \code{forecast.lpacf} object}
  \item{\dots}{Other arguments (not used)}
}
\details{Prints a \code{forecast.lpacf} object, basically telling
        you what's there.
}
\value{None.
}
\references{
OUT PAPER HERE
}
\author{
Guy Nason
}

\seealso{\code{\link{forecast.lpacf}}, \code{\link{summary.forecast.lpacf}}
}
\examples{
#
# Simulate an example
#
x.test <- tvar2sim()
#
# Do a two-step ahead forecast
#
x.fl <- forecast.lpacf(x.test, h=2, forecast.type="recursive")
#
# Print out the object
#
print(x.fl)
#
# This is what gets output
#
#Class 'forecast.lpacf' : Forecast from Locally Stationary Time Series:
#       ~~~~  : List with 8 components with names
#              mean std.err lpacf ci binwidth p x d 
#
#
#summary(.):
#----------
#Number of steps ahead predicted:  2 
#Predictions are (3dp):  1.52 -0.365 
#Std err are (3dp):  0.952 0.955 
#Smoothing binwidth was:  293 
#Forecast was based on a p-backlag value selected as:  3 
#There was no explicit differencing.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
