\name{fp.forecast}
\alias{fp.forecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Do automatic Box-Jenkins ARIMA fit and forecast.
}
\description{This function merely wraps some excellent functions
	from the forecast package up and returns the forecast values
	and their lower and upper prediction intervals.
}
\usage{
fp.forecast(x, h = 1, conf.level = 95)
}
\arguments{
  \item{x}{The time series you wish to forecast.
}
  \item{h}{The number of steps ahead (forecast horizon)
}
  \item{conf.level}{The confidence level for the forecast prediction
	interval expressed as a value between 0 and 100.
}
}
\details{This function entirely relies on existing functions from
	the forecast package. It applies \code{auto.arima} to \code{x}
	to fit an ARIMA model to the series with an automatic choice
	of parameters. Then the \code{forecast} function is applied
	to the ARIMA object to obtain forecasts and prediction intervals.

}
\value{A matrix with \code{h} rows and three columns. The first column
	contains the forecasted values. The second and third columns
	contrain the lower and upper prediction intervals.
}
\references{
Killick, R., Knight, M.I., Nason, G.P., Nunes M.A., Eckley I.A. (2023) Automatic Locally Stationary Time Series Forecasting with
application to predicting U.K. Gross Value Added Time Series under sudden shocks caused by the COVID pandemic 
\href{https://arxiv.org/abs/2303.07772}{arXiv:2303.07772}

Hyndman, R.J. and Khandakar, Y. (2008) Automatic Time Series Forecasting:
	The forecast package for R. \emph{Journal of Statistical Software},
	\bold{27}, Issue 3.
}
\author{
G.P. Nason
}

\examples{
#
# Generate random test series
#
x.test <- tvar2sim()
#
# Produce stationary Box-Jenkins forecasts and prediction intervals for
# two-steps ahead
#
fp.forecast(x.test, h=2)
}
\keyword{ts}
\keyword{wavelet}
\keyword{forecast}
