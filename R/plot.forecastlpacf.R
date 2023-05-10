plot.forecastlpacf <-
function (x, extra.y = NULL, f.col = 4, show.pi = "standard", pi.col = 2, xlab= "Time", ylab="Time Series",zoom = FALSE, zoom.no = 30, sw = 0.2, conf.level = 95, pc.fan = (1:9) * 10, fan.seps = FALSE, fan.rgb.col = c(1, 0, 0), ...)
{
#
# Work out critical quantile value
#
if (conf.level < 0 || conf.level > 100)
        stop("conf.level has to be between 0 and 100")
    siz <- 1 - conf.level/100
    qval <- qnorm(1 - siz/2)



#
# Work out various lengths of input vectors
#
lx <- length(x$x)
h <- length(x$mean) 
new.lx <- lx + h

#
# Now compute prediction credible intervals, if required
#

if (show.pi == "standard")	{
	x0v <- x1v <- yuv <- ylv <- NULL
	for (i in 1:h)	{
		x0v <- c(x0v, lx + i - sw/2)
		x1v <- c(x1v, lx + i + sw/2)
		yuv <- c(yuv, x$mean[i] + qval * x$std.err[i])
		ylv <- c(ylv, x$mean[i] - qval * x$std.err[i])
		}
	}
else if (show.pi == "fan")	{
	lpc.fan <- length(pc.fan)
	fp.vals <- array(0, dim=c(h, lpc.fan, 2))

	for(i in 1:h)	{
		for(j in 1:lpc.fan)	{

			siz <- 1 - pc.fan[j]/100
		        qval <- qnorm(1 - siz/2)

			fp.vals[i,j,1] <- x$mean[i] + qval*x$std.err[i]
			fp.vals[i,j,2] <- x$mean[i] - qval*x$std.err[i]
			}
		}

	# These two quantities are only specified to get the vertical
	# size of the plot right, to save introducing further variables
	# to the range computations later.
	yuv <- as.vector(fp.vals[, , 1])
	ylv <- as.vector(fp.vals[, , 2])
	}
else if (show.pi == "none")	{
	# These two quantities are only specified to get the vertical
	# size of the plot right, to save introducing further variables
	# to the range computations later.
	# No prediction intervals required, so just reuse time series values
	ylv <- yuv <- x$x[1]
	}


#
# Plot original time series
#
if (is.null(extra.y))
	rx <- range(c(x$x,yuv,ylv))	# Get vertical scale
else
	rx <- range(c(x$x,yuv,ylv,extra.y))	# Get vertical scale


if (zoom == FALSE)	{
	plot(x=c(1, new.lx), y=rx, type="n", xlab=xlab, ...)
	}
else	{
	bx <- lx-zoom.no+1
	if (is.null(extra.y))
		rx <- range(x$x[bx:lx] ,yuv, ylv)
	else
		rx <- range(x$x[bx:lx] ,yuv, ylv, extra.y)
	plot(x=c(bx, new.lx), y=rx, type="n", xlab=xlab, ...)
	}


if (show.pi == "standard")	{
	for (i in 1:h)	{
	    polygon(x = c(x0v[i], x1v[i], x1v[i], x0v[i]),
		      y = c(ylv[i], ylv[i], yuv[i], yuv[i]),
		      density = 50, col = pi.col)
		}
	}

else if (show.pi == "fan")	{

	#
	# Compute polygons for fan plots
	#

	mf <- seq(from=1, to=0, length=lpc.fan+1)
	mf <- mf[-length(mf)]

	for(j in lpc.fan:1)	{

		lyc <- x$x[lx]
		lxc <- lx

		for(i in 1:h)	{
			lxc <- c(lxc, lx + i)
			lyc <- c(lyc, fp.vals[i, j, 1])
			}

		for(i in h:1)	{
			lxc <- c(lxc, lx + i)
			lyc <- c(lyc, fp.vals[i, j, 2])
			}

		the.col <- rgb(red = fan.rgb.col[1],
			green = fan.rgb.col[2],
			blue = fan.rgb.col[3], alpha=mf[j])

		if (fan.seps==FALSE)
			polygon(lxc, lyc, col=the.col, border=the.col)
		else
			polygon(lxc, lyc, col=the.col, border=1)

		}
	}

if (zoom == FALSE)	{
	lines(1:lx, x$x)
	}
else	{
	bx <- lx-zoom.no+1
	if (is.null(extra.y))
		rx <- range(x$x[bx:lx] ,yuv, ylv)
	else
		rx <- range(x$x[bx:lx] ,yuv, ylv, extra.y)
	lines(bx:lx, x$x[bx:lx])
	}

#
# Now plot the predicted values
#
points(lx + (1:h), x$mean, col = f.col)
lines(c(lx, lx + (1:h)), c(x$x[lx], x$mean), col = f.col)

if (show.pi == "fan")
	return(fp.vals)
}
