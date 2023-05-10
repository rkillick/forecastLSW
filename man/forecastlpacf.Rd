\name{forecastlpacf}
\alias{forecastlpacf}
\alias{dforecastlpacf}
\title{
Forecasts future values of the time series \code{x} \code{h}-steps ahead. (for the specified horizon \code{h}) using the lpacf to decide the dimension of the generalized Yule-Walker equations.
}
\description{This function forecasts a \code{x} time series \code{h}-steps
	ahead. The time series is assumed to be locally stationary
	(actualy locally stationary wavelet) and uses a local prediction
	method. The function makes use of the localized partial autocorrelation
	function to decide the order of the local Yule-Walker equations
	used in the forecast.
}
\usage{
forecastlpacf(x,h=1,regularize=TRUE,lag.max=max(10,2*h),forecast.type=NULL,...)
}
\arguments{
  \item{x}{Vector containing time series to generate forecasts for.
}
  \item{h}{Integer. Maximum prediction horizon. Forecasts will be given for 
	one to \code{h} time steps ahead. Currently, for \code{dforecastlpacf}
	\code{h} is hard-coded to be 1. If you want to forecast further
	ahead for differenced data then you will have to difference the time
	series manually and supply it to \code{forecastlpacf}.
}
  \item{regularize}{
Logical.  If \code{regularize=TRUE} then the Yule-Walker matrix is regularized
	before prediction using the method from Xie et al. (2007).
	 If \code{regularize=FALSE} then no regularization takes place.
}
  \item{lag.max}{Maximum lag that the \code{\link{lpacf}} is calculated to.
	If this is set too low, i.e. the automated estimation of the
	dimension of the Yule-Walker matrix is equal to \code{max.lag},
	then the function will print a warning message.
}
  \item{forecast.type}{
Options are \code{fixed}, \code{recursive} or \code{extend}, see details for further information.
}
  \item{...}{
Other parameters to be passed to the periodogram and lacv (local autocovariance) estimation, e.g. \code{filter.number} and \code{family} detailing the wavelet to be used.
}
}
\details{
The function calculates the wavelet periodogram followed by the lacv and
\code{\link{lpacf}}. 
NOTE: Often when local (windowed) estimates are created one assigns the
estimated value to the central point in the window.
This is NOT the approach we take here when calculating the lacv and
\code{\link{lpacf}}.
Instead we operate a rear facing window where the estimate is assigned to the
final point in the window.

The lpacf is used to decide the dimension of the local Yule-Walker equations used for forecasting.
The periodogram is then smoothed using a running mean smoother,
and then to get forecast lacv estimates.
The Yule-Walker equations give the forecast mean for \code{h} steps ahead.
The standard deviation of the forecasts is also returned.

When we are trying to forecast \code{h} steps ahead we use the lpacf to decide how many values (\code{p}) we should use for prediction.  The original method of Fryzlewicz et al. (2003) decides on \code{p} and then does a \code{h} step ahead forecast only using the \code{p} last values.  This is what \code{forecast.type='fixed'} does, regardless of the size of \code{p} in relation to \code{h}.  Note that the left hand side of the Yule-Walker matrix is fixed and only the right hand size (the forecast lacv) is changing.  Thus the size of \code{h} is not explicitly taken into account, there is just an inflated variance in the lacv estimate.
One other option is to use the intermediate forecast values as if they were observed and perform a recursive forecast - this is what \code{forecast.type='recursive'} does.  Here everything in the Yule-Walker equations is different for each forecast value.

A third option is to use \code{forecast.type='fixed'} when \code{p} is greater or equal to \code{h} but then when we are trying to forecast beyond this we extend the Yule-Walker equations to be the same dimension as the forecast horizon.  Thus using \code{h} previous values instead of \code{p}.  This is what \code{forecast.type='extend'} does.

The method closest to the stationary world is \code{forecast.type='recursive'}.


The \code{dforecastlpacf} internally differences the time series
and then performs the local forecasting as in \code{forecastlpacf}
but only for one-step ahead. The advantage is that subsequent plotting
routines can nicely show the original time series, with the forecasts
on the original (not differenced) scale with the forecast and appropriate
confidence interval.
}
\value{An object of class \code{\link{forecastlpacf}} which is a list
	with the following components.
\item{mean}{Returns time series forecasts from one to \code{h}-steps ahead.
When \code{h} is greater than one multiple predictions are returned in this
vector. In this case, item in position \code{n} corresponds to \code{n} steps ahead. For example, if \code{h=2} then this vector will contain two elements.
The first one corresponds to the prediction one-step-head and the second entry
to the two-steps-ahead prediction.}
\item{std.err}{Returns the prediction error, which can be used for assessing the prediction intervals. Item \code{n} corresponds to the prediction \code{n}-steps ahead, as for the \code{mean} component.}
 
\item{lpacf}{Returns the estimated local partial autocovariance function}
\item{ci}{The confidence interval on lpacf which was used used for the automatic calculation of p}
\item{binwidth}{The automatic bandwidth used for the running mean smoother}
\item{p}{Returns the automatic choice of p - the dimension of the generalized Yule-Walker equations.}
\item{x}{The supplied original time series}
\item{d}{Differencing that was applied to the input series before forecasting.
	For \code{forecastlpacf} this is \code{d=0}. For differencing once see
	the function \code{\link{dforecastlpacf}} which returns
	\code{d=1}.}
}
\references{
Killick, R., Knight, M.I., Nason, G.P., Nunes M.A., Eckley I.A. (2023) Automatic Locally Stationary Time Series Forecasting with
application to predicting U.K. Gross Value Added Time Series under sudden shocks caused by the COVID pandemic 
\href{https://arxiv.org/abs/2303.07772}{arXiv:2303.07772}

Fryzlewicz, P., Van Bellegem, S. and von Sachs, R. (2003) Forecasting
	non-stationary time series by wavelet process modelling.
	\emph{Annals of the Institute of Statistical Mathematics},
	\bold{55}, 737-764.

Nason, G.P., von Sachs, R., Kroisandt, G. (2000) Wavelet processes and adaptive estimation of the evolutionary wavelet spectrum.
\emph{J. Roy. Statist. Soc. B}, \bold{62}, 271-292.

Xi, Y., Yu, J., Ranneby, B. (2007) Forecasting Using Locally Stationary Wavelet Processes.

}
\author{
R. Killick
}

\seealso{
\code{\link{lpacf}}, \code{\link{forecastpanel}},
	\code{\link{plot.forecastlpacf}},
	\code{\link{print.forecastlpacf}},
	\code{\link{summary.forecastlpacf}}}
\examples{
# first generate some non-stationary data we want to forecast
set.seed(1)
x=tvar2sim()

#predict 1-step ahead using Daubechies wavelets with 2 vanishing moments, although 
#other choices for the wavelet family and filter are possible (including Haar)
pred<-forecastlpacf(x,h=1,filter.number=2,family="DaubExPhase",forecast.type='recursive')

#pred$mean gives the predicted value, while pred$std.err gives the prediction error
}
\keyword{ts}
\keyword{wavelet}
\keyword{forecast}
