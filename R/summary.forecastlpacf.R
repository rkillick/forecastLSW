summary.forecastlpacf <-
function (object, ...) 
{
    h <- length(object$mean)
    cat("Number of steps ahead predicted: ", h, "\n")

    if (h < 6)	{
	cat("Predictions are (3dp): ", signif(object$mean[1:h],3), "\n")
	cat("Std err are (3dp): ", signif(object$std.err[1:h], 3), "\n")
	    }
    else 
	    cat("h>=6, so not able to print out all predictions. Extract components.\n")

    cat("Smoothing binwidth was: ", object$binwidth, "\n")
    cat("Forecast was based on a p-backlag value selected as: ", object$p, "\n")
    if (object$d==0)
	cat("There was no explicit differencing.\n")
    else if (object$d==1)
	cat("There was differencing: d=1\n")

}
