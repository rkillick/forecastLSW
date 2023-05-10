which.wavelet.best <-
function (x, n.to.test = 10, go.back=5, forecast.type = "recursive", lapplyfn = lapply) 
{
    calc.mse <- function(ff, x, n.to.test, forecast.type) {
        filter.number = ff$filter.number
        family = ff$family
        ans <- forecastLSW::testforecast(x = x, n.to.test = n.to.test, forecast.type = forecast.type, 
            filter.number = filter.number, family = family, plot.it = FALSE,
		go.back=go.back, lapplyfn=lapplyfn)
	if (go.back==0)	{
	ans <- sqrt(sum((ans[, 1] - ans[, 2])^2))/n.to.test

	}
	else
		ans <- sqrt(sum((ans[, 2])^2))/n.to.test
        return(ans)
    }
    l.in <- vector("list", 17)
    fnv <- rep(0, 17)
    fam <- rep("", 17)
    for (i in 1:10) {
        l.in[[i]] <- list(filter.number = i, family = "DaubExPhase")
        fnv[i] <- i
        fam[i] <- "DaubExPhase"
    }
    for (i in 11:17) {
        l.in[[i]] <- list(filter.number = i - 7, family = "DaubLeAsymm")
        fnv[i] <- i - 7
        fam[i] <- "DaubLeAsymm"
    }
    ans <- lapplyfn(l.in, calc.mse, x = x, n.to.test = n.to.test, 
        forecast.type = forecast.type)
    ans <- unlist(ans)
    ans.min.ix <- ans == min(ans)
    the.min <- rep("", 17)
    the.min[ans.min.ix] <- "<- Min MSE"
    ansdf <- data.frame(filter.number = fnv, family = fam, rmse = ans, 
        min.mse = the.min)
    return(ansdf)
}
