\name{analyze.windanomaly}
\alias{analyze.windanomaly}
\title{
Analyzes the windanomaly data, see below for more details.
}
\description{
Takes the windanomaly data and analyzes it.
}
\usage{
analyze.windanomaly(h=10,atTime=NULL,atLag=NULL)
}
\arguments{
  \item{h}{Numeric vector for a h-steps ahead forecast.  In reality we treat the \code{data[1:(length(data)-h)]} as known and try to forecast h-steps ahead from \code{data[length(data)-h]}
}
  \item{atTime}{Vector of the times (rows) of the \code{lpacf} to be plotted. Note that not all times can be plotted, the range of plausible values depends on the bandwidth selected for the data.  At the time of writing binwidth for windanomaly is 1173 and thus the plausible values are [587,680].
}
  \item{atLag}{Vector of the lags (columns) of the \code{lpacf} to be plotted.  The default maximum lag is \code{floor(10 * log10(n))} which is 31 for windanomaly.
}
}
\details{
Takes the windanomaly data and analyzes it. Specifically the following is produced:
\itemize{
  \item time series plot of the windanomaly data
  \item the lpacf for the windanomaly data
  \item plots of the lpacf + CI for the specified times and lags
  \item the forecast for h to last data point(s) using the lpacf method
  \item the forecast for h to last data point(s) using the standard ARMA method
  \item plot of the original data, forecasts and confidence intervals for both methods, red=lpacf, blue=ARMA.
}
}
\value{
List containing the lpacf, forecast + accuracy measures using the lpacf method and forecast +accuracy measures using the ARMA method.
}
\references{
Killick, R., Knight, M.I., Nason, G.P., Nunes M.A., Eckley I.A. (2023) Automatic Locally Stationary Time Series Forecasting with
application to predicting U.K. Gross Value Added Time Series under sudden shocks caused by the COVID pandemic 
\href{https://arxiv.org/abs/2303.07772}{arXiv:2303.07772}
}
\author{
Rebecca Killick
}

\seealso{
\code{\link[lpacf]{lpacf.plot}}, \code{\link{forecastlpacf}}
}
\examples{
\dontrun{
	data(windanomaly)
	out=analyze.windanomaly()
}
}
\keyword{ts}
\keyword{wavelet}
