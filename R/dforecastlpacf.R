dforecastlpacf <-
function (x, regularize = TRUE, lag.max = 10, forecast.type = NULL, ...) 
{
# Like forecastlpacf, but (i) differences data once, (ii) forecasts the
#	differences (iii) reintegrates the series
# Currently, only can difference one time. This is because we haven't
# worked out the joint distribution of the forecast errors.
#
# Likewise, can only do one-step ahead. This function is a prototype.


    lx <- length(x)
    xd <- diff(x)

    xd.f.l <- forecastLSW::forecastlpacf(x=xd, h=1, regularize=regularize, lag.max=lag.max,
	forecast.type=forecast.type, ...)

    pmean <- xd.f.l$mean + x[lx] 
    std.err <- xd.f.l$std.err
    ci <- xd.f.l$ci + x[lx]


    out = list(mean = pmean, std.err = std.err, lpacf = xd.f.l$lpacf, 
        ci = ci, binwidth = xd.f.l$binwidth, p = xd.f.l$p, x = x,
	d=1)
    class(out) = "forecastlpacf"
    return(out)
}
