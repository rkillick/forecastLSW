\name{forecastpanel}
\alias{forecastpanel}
\title{
Function to produce a plot of data forecasts.
}
\description{
This function produces a plot of the data forecast with confidence intervals (if supplied) and, if supplied, against the truth.  Optionally, summaries of the forecast fit are returned.
}
\usage{
forecastpanel(forecastobj,truth=NULL,add=FALSE,summary=TRUE,test="all",move=0,
conf.level=95,col="red",pch=c(17,19,95),...)
}
\arguments{
  \item{forecastobj}{Either an object of class \code{forecast}, \code{forecastlpacf} or a vector of forecasts.
}
  \item{truth}{The true values of the signal that has been forecast.
}
  \item{add}{If FALSE a new plot is created, otherwise points are added to the active graphics device.
}
  \item{summary}{If TRUE a summary of the forecast fit is supplied, see \code{\link[forecast]{accuracy}}.
}
  \item{test}{Argument supplied to \code{\link[forecast]{accuracy}} to determine which summary measures are returned.
}
  \item{move}{If move does not equal 0 then this is the amount to move the points+confidence intervals for the forecasts to the left (if negative) and to the right (if positive) to offset the plotted location (0) to potentially make the graphic clearer.
}
  \item{conf.level}{Confidence level used for the \code{forecastobj}. If \code{forecastobj} is lpacf it can be calculated for any confidence level.  If \code{forecastobj} is of class forecast then the level needs to match the one given when the forecast was calculated. A number between 1 and 100.
}
  \item{col}{Specifies the colour of forecasts on the plot, see \code{\link[graphics]{par}} for details.
}
  \item{pch}{Length 3 vector specifying the plotting character (pch) of the truth, forecast and CI in that order.}
  \item{\dots}{Additional arguments can be supplied which will be passed to \code{plot}, \code{points} and \code{segments}.
}
}
\details{
Plots the forecast data, confidence intervals and true signal if supplied.  If \code{summary=TRUE} then the output of \code{\link[forecast]{accuracy}} is returned.
}
\value{
If \code{summary=TRUE} then the output of \code{\link[forecast]{accuracy}} is returned.
}
\author{
Rebecca Killick
}

\seealso{
\code{\link{forecastlpacf}},\code{\link[forecast]{accuracy}}
}
\examples{
# first generate a time-varying process
x=tvar2sim()

# forecast the last 12 data points using the lpacf
ans<-forecastlpacf(x[1:500],h=12,forecast.type='recursive')

# then plot it and get summaries to see how we did
\dontrun{plot(ans,truth=x[501:512],move=0.05)}
}
\keyword{ts}
\keyword{wavelet}
