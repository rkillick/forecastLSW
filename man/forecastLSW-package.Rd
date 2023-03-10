\name{forecastLSW-package}
\alias{forecastLSW-package}
\docType{package}
\title{Forecasting for locally stationary (wavelet) time series based on the local partial autocorrelation function.
}
\description{This package computes forecasts for a time series
	with prediction errors. The forecasting methodology is designed
	with an underlying locally stationary wavelet model in mind. However,
	it is possible that the forecasting methodology will work well for
	other time series, including those where an underlying model is not
	necessarily known. Note: the methodology can work with any
	length of time series. The package also contains functions to
	display the forecasts and their prediction intervals or a fan chart,
	a function to evaluate the performance of the new forecasting methods
	and compare it to Box-Jenkins ARMA-based forecasting and a routine
	to identify wavelets that enable the forecasting routines to perform
	well. 
}
\details{
\tabular{ll}{
Package: \tab lpacf\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2023-02-20\cr
License: \tab GPL-2\cr
}
The \code{\link{forecast.lpacf}} function computes forecasts of a locally
stationary (wavelet) time series using the localized partial autocorrelation
to help with history identification. The results of such forecasting
can be printed using \code{\link{print.forecast.lpacf}} or plotted
with \code{\link{plot.forecast.lpacf}}.

Two other useful functions are \code{\link{forecast.test}} which 
runs some testing on forecasting some end values of a series using earlier
values and compares the new forecasting with standard Box-Jenkins ARMA
forecasting (visualisation via \code{\link{forecast.panel}}) and 
\code{\link{which.wavelet.best}} which attempts to identify which wavelet is 
well-suited to forecasting a particular series.
}
\author{
Rebecca Killick, Marina Knight, Guy Nason, Matt Nunes

Maintainer: Rebecca Killick <r.killick@lancs.ac.uk>
}
\references{
TBC
}
\keyword{ package }
\seealso{\code{\link{forecast.lpacf}},
	\code{\link{forecast.test}},
	\code{\link{which.wavelet.best}}
}
\examples{
#
# See examples in each of the functions' help pages linked above.
#
}
