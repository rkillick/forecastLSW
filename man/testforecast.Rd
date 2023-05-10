\name{testforecast}
\alias{testforecast}
\title{Compare locally stationary forecasting with Box-Jenkins-type
	forecasting, by predicting the final values of a time series.
}
\description{A good way of evaluating a forecasting method is to
	apply the method to most of a series (apart from the last few values)
	to forecast those last few values. Then, the forecasts and the true
	values can be compared to see how good the forecast is. This function
	performs this for the locally stationary forecasting based
	on wavelet processes in \code{\link{forecastlpacf}} and
	a version of the Box-Jenkins forecasting, and also produces
	both plots and returns results of the testing.
}
\usage{
testforecast(x, n.to.test, go.back=0,  plot.it = TRUE, regularize = TRUE, 
    lag.max = max(10, 2 * n.to.test), truth.pch = 23, truth.col = 3, zoom = TRUE, 
    zoom.no = 30, forecast.type = NULL, conf.level = 0.95, stycol = 6, silent = TRUE,
	  lapplyfn=lapply, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The time series you want to use in testing.
}
  \item{n.to.test}{Suppose the length of \code{x} is T. This function
	uses the first T-\code{n.to.test} observations to predict the
	last \code{n.to.test} observations.
}
  \item{go.back}{If \code{go.back=0} then a single forecasting operation
	forecasting the last n.to.test observations from the previous
	data is conducted. If \code{go.back} is an integer greater than
	zero then the same forecasting as with \code{go.back} occurs but
	each time the end of the series is moved back one point. This
	shifting back occurs from one shift to \code{go.back} shifts.
	The purpose of this is to repeat the exercise for using previous
	data to forecast \code{n.to.test} points at the end of the
	series, but to then repeat this for the series one step earlier,
	then two steps earlier, ..., back to \code{go.back} steps earlier.
	The results of each forecast are combined into an overall
	root-mean-squared error result for each forecast horizon (there
	will be \code{n.to.test} values) for both of the Box-Jenkins
	and the new forecast methodology provided by 
	\code{\link{forecastlpacf}}. These additionally forecasts
	will be computed in parallel if the \code{parallel} package
	is loaded and \code{mclapply} is used as an argument to
	\code{lapplyfn}. 
}
  \item{plot.it}{If \code{TRUE} a plot is produced showing the original
	time series, the stationary and locally stationary forecasts,
	and their prediction intervals.  If \code{FALSE} then no plot
	is produced.
}
  \item{regularize}{Passed through to \code{forecastlpacf}}
  \item{lag.max}{Passed through to \code{forecastlpacf}}
  \item{truth.pch}{The type of plotting character used for the true
	values, see \code{pch} argument to \code{points} function in R.}
  \item{truth.col}{Colour of plot symbol used for true values.}
  \item{zoom}{Typically, we're interested in the later values of a time
	series when doing forecasting. If this argument is \code{TRUE}
	then only the last \code{zoom.no} observations are plotted, so one
	can focus on the end of the series.}
  \item{zoom.no}{If \code{zoom=TRUE} then this argument controls how much
	of the end of the series is plotted.  }
  \item{forecast.type}{Passed through to \code{forecastlpacf} }
  \item{conf.level}{Controls the width of the prediction intervals for
	both stationary and nonstationary forecasting.
}
  \item{stycol}{The colour of both the stationary forecasts and their
	confidence intervals. 
} \item{silent}{If \code{TRUE} then nothing gets printed, otherwise
	messages get printed.
}
  \item{lapplyfn}{For single-processor use this argument should
	be \code{lapply} (the default). However, you can set the argument 
	to \code{mclapply} if you have the \code{parallel} package loaded.
	Remember to set the number of processors you want to use
	with the \code{mc.cores} option, e.g. \code{options(mc.cores=4)}
	if you had four cores available.
}
  \item{\dots}{Other arguments to the \code{forecastlpacf} call}
}
\details{
Suppose \code{n.to.test=1}. Then this function uses all the values of
the time series \code{x} apart from the last to generate two forecasts
of the last value. The two methods used to forecast are
the locally stationary method \code{forecastlpacf} and a Box-Jenkins
ARIMA alternative for stationary series coded in \code{\link{fp.forecast}}.

Then, if \code{plot.it=TRUE} a plot of the time series \code{x} is produced,
overlaid with both types of forecast and their related prediction intervals
(the locally stationary ones are hached thin rectangles, the stationary
ones indicated by vertical < > symbols. The true value is also indicated
by a character whose visual characteristics are controlled by
the \code{truth.pch} and \code{truth.col} arguments, but by default
are a green diamond.

If \code{n.to.test} is bigger than 1 then all of the data, apart from
the last \code{n.to.test} values are used in constructing the forecasts
(both stationary and locally stationary) for the last \code{n.to.test}
values.

Values of the empirical root mean squared error of the two forecast
methods are printed out (unless \code{silent=TRUE}). The predictions
and their standard errors for the \code{n.to.test} values are printed out.
}
\value{If \code{go.back=0} a matrix with \code{n.to.test}
	values with four columns is returned. The
first column is the actual true value of the time series in the last
\code{n.to.test} positions. The second and fourth columns are the forecast
values from the locally stationary and stationary methods. The third column
are the locally stationary prediction error values.

If \code{go.back} is a positive integer then a data frame with two
	columns. The first column corresponds to stationary forecasting
	using the standard Box-Jenkins type method encapsulated by
	\code{\link{fp.forecast}}. The second column corresponds to
	the locally stationary forecasting encapsulated by
	\code{forecastlpacf}. Each row of the frame corresponds to
	a different forecasting horizon, the horizon is indicated by
	the row name of the data frame.
}
\references{
Killick, R., Knight, M.I., Nason, G.P., Nunes M.A., Eckley I.A. (2023) Automatic Locally Stationary Time Series Forecasting with
application to predicting U.K. Gross Value Added Time Series under sudden shocks caused by the COVID pandemic 
\href{https://arxiv.org/abs/2303.07772}{arXiv:2303.07772}
}
\author{
G.P. Nason
}
\seealso{\code{\link{forecastlpacf}},
\code{\link{fp.forecast}},
\code{\link{plot.forecastlpacf}}
}
\examples{
#
# Generate simulated time series from TVAR(2) model.
#
x.test <- tvar2sim()
#
# Now run testforecast on this example time series.
# We've only supplied plot.it=FALSE because its in an R help page, normally
# you would set plot.it=TRUE, which is the default, because you want to see
# the plot.
#
tmp <- testforecast(x.test, n.to.test=3, forecast.type="recursive",
	plot.it=FALSE)
}
\keyword{ts}
\keyword{wavelet}
\keyword{forecast}
