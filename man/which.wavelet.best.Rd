\name{which.wavelet.best}
\alias{which.wavelet.best}
\title{Find out what wavelet is good for forecasting your series.
}
\description{A big question with many wavelet methods is which wavelet
	should one use for a particular task. This function tries some
	forecasting on your time series with all Daubechies compactly
	supported wavelets available to it and returns a list of the
	forecasting performance for each choice, and indicates which
	wavelet gave the best results. This wavelet can then be used
	in future forecasting.
}
\usage{
which.wavelet.best(x, n.to.test = 10, go.back=5,
	forecast.type = "recursive", lapplyfn = lapply)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Your times series (not necessarily of dyadic length!)
}
  \item{n.to.test}{How many observations at the end to test as part
	of the assessment process. The default, 10, means that 10 observations
	at the end of the series will all be forecast. This number should
	be reasonably big to enable forecasts of more than a few data points,
	but not too large.
}
  \item{go.back}{Controls the \code{go.back} argument to
	\code{\link{testforecast}}. Number of repeats of the
	procedure on successively one-unit of time earlier series.}
  \item{forecast.type}{The type of forecasting as detailed in
	\code{\link{forecastlpacf}}.
}
  \item{lapplyfn}{Type of list processing function. By default it
	uses R's \code{lapply} function. However, if you use the
	\code{parallel} library you can replace this with
	\code{mclapply} which will make this function go faster using
	parallel computation. Don't forget to set the
	\code{options(mc.cores=4)} argument to what you wish (here it is
	set to 4 in this example, but you should set it to something that
	is appropriate for your machine environment).
}
}
\details{This function uses all choices of wavelet to forecast the
	last \code{n.to.test} observations of your time series. It
	works out the forecasting error in doing so for each choice of
	wavelet and returns a list telling you which wavelet did best.
}
\value{A data frame containing information on the root mean squared
	forecasting error performance of the locally stationary forecasting
	method for different wavelets. The data frame has four columns and
	a row for each wavelet tried. The first and second column give
	the filter number and family for each wavelet. The third column
	gives the root mean squared error for each combination of
	wavelet. The fourth column contains an indicator that shows
	which wavelet was best (there might be more than one).
}
\references{
Killick, R., Knight, M.I., Nason, G.P., Nunes M.A., Eckley I.A. (2023) Automatic Locally Stationary Time Series Forecasting with
application to predicting U.K. Gross Value Added Time Series under sudden shocks caused by the COVID pandemic 
\href{https://arxiv.org/abs/2303.07772}{arXiv:2303.07772}
}
\author{
Guy Nason
}

\seealso{\code{\link{testforecast}} }
\examples{
#
# Generate simulated example
#
x <- tvar2sim()
#
# Work out which wavelet is best for forecasting this series
#
# Note: to speed up I also do:
#	library("parallel")
#	options(mc.cores=4)	# You have a four core machine, eg 
#	tmp <- which.wavelet.best(x, lapplyfn=mclapply)
#
# Note2: The following command can take a few minutes to run, even on
#	a fairly recent (2013) machine. You can speed it up by using
#	parallel execution as noted above, or by reducing go.back or
#	by reducing n.to.test, and also shortening the time series x to
#	more recent values. However, you need to be careful if you shorten
#	x too much then you are not basing the best wavelet decision on the
#	right time series. Similarly, by reducing go.back you are not
#	insuring your answer across runs across many internal forecasts. 
#
\dontrun{tmp <- which.wavelet.best(x)}
#
# Print out what the result was:
#
\dontrun{print(tmp)}
#   filter.number      family       mse    min.mse
#1              1 DaubExPhase 0.2139173 <- Min MSE
#2              2 DaubExPhase 0.5040532           
#3              3 DaubExPhase 0.4064091           
#4              4 DaubExPhase 0.3077695           
#5              5 DaubExPhase 0.3706422           
#6              6 DaubExPhase 0.6617254           
#7              7 DaubExPhase 0.5477581           
#8              8 DaubExPhase 0.6881407           
#9              9 DaubExPhase 0.5514298           
#10            10 DaubExPhase 0.5551846           
#11             4 DaubLeAsymm 0.3134285           
#12             5 DaubLeAsymm 0.3910101           
#13             6 DaubLeAsymm 0.7480980           
#14             7 DaubLeAsymm 0.5700830           
#15             8 DaubLeAsymm 0.5661297           
#16             9 DaubLeAsymm 0.5689345           
#17            10 DaubLeAsymm 0.5580267           
}
\keyword{ts}
