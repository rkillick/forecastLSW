\name{print}
\alias{print.forecastlpacf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prints a \code{forecastlpacf} object
}
\description{Prints a \code{forecastlpacf} object, basically telling
	you what's there.
}
\usage{
\method{print}{forecastlpacf}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The \code{forecastlpacf} object}
  \item{\dots}{Other arguments (not used)}
}
\details{Prints a \code{forecastlpacf} object, basically telling
        you what's there.
}
\value{None.
}
\references{
Killick, R., Knight, M.I., Nason, G.P., Nunes M.A., Eckley I.A. (2023) Automatic Locally Stationary Time Series Forecasting with
application to predicting U.K. Gross Value Added Time Series under sudden shocks caused by the COVID pandemic 
\href{https://arxiv.org/abs/2303.07772}{arXiv:2303.07772}
}
\author{
Guy Nason
}

\seealso{\code{\link{forecastlpacf}}, \code{\link{summary.forecastlpacf}}
}
\examples{
#
# Simulate an example
#
x.test <- tvar2sim()
#
# Do a two-step ahead forecast
#
x.fl <- forecastlpacf(x.test, h=2, forecast.type="recursive")
#
# Print out the object
#
print(x.fl)
#
# This is what gets output
#
#Class 'forecastlpacf' : Forecast from Locally Stationary Time Series:
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
