forecastlpacf <-
function (x, h = 1, regularize = TRUE, lag.max = max(10, 2 * 
    h), forecast.type = NULL, ...) 
{
    xdata = x

    if (sum(is.na(x)) > 0) {
        stop("Time series must not contain NA values.")
    }
    if ((h <= 0) | !is.wholenumber(h)) {
        stop("Forecast horizon, h, must be a positive integer.")
    }
    if (!is.logical(regularize)) {
        stop("regularize must be logical.")
    }
    if (lag.max <= h) {
        stop(paste("lag.max must be atleast:", h + 1))
    }
    if (lag.max > length(x)) {
        stop("lag.max must be within the range of the data")
    }
    if (is.null(forecast.type)) {
        stop("forecast.type must be either recursive, fixed or extend.")
    }
    else if (!(forecast.type %in% c("recursive", "fixed", "extend"))) {
        stop("forecast.type must be either recursive, fixed or extend.")
    }

    len = length(x)

    sp.lacv = lacv.fc.leftwp(x, h, ...)
    lpacf = lpacf.leftwp.end(x, lag.max = lag.max, ...)$lpacf
    ci = qnorm((1 + 0.95)/2)/sqrt(sp.lacv$binwidth)
    p = ifelse(!length(which(((lpacf > ci) | (lpacf < -ci)) == 
        FALSE)), lag.max, which(((lpacf > ci) | (lpacf < -ci)) == 
        FALSE)[1] - 1)
    h = 1:h
    if (p == 0) {
        p = 1
    }
    if (p == lag.max) {
        warning("It is advised that you increase your maximum lag to aid your forecast estimation")
    }
    pmean = NULL
    std.err = NULL
    if (forecast.type != "recursive") {
        p = rep(p, length(h))
        if (any(p <= max(h))) {
            warning("p is less than the maximum forecast horizon, using forecast.type=\"extend\" or \"fixed\" matters!")
            if (forecast.type == "extend") {
                p[p < h] = h[p < h]
            }
        }
        B <- matrix(0, max(p), max(p))
        for (i in 1:max(p)) {
            B[i, i:max(p)] = sp.lacv$lacv[len - max(p) + i, 1:length(i:max(p))]
        }
        tmp = diag(B)
        B = t(B) + B
        diag(B) = tmp
        for (i in 1:length(p)) {
            if (p[i] == 1) {
                RHS = 0
            }
            else {
                RHS <- c(0, sp.lacv$lacv[len + h[i], 2:p[i]])
            }
            extra.var = sp.lacv$lacv[len + h[i], 1]
            pr = list(B = B[(max(p) - p[i] + 1):max(p), (max(p) - 
                p[i] + 1):max(p)], RHS = RHS, extra.var = extra.var)
            class(pr) = "predeq"
            if (regularize == TRUE) {
                pr = reg.xyr(pr)
            }
            b <- solve(pr$B, pr$RHS)
            pmean[i] <- sum(x[(len - p[i] + 1):len] * b)
            std.err[i] <- sqrt(max(sig.sq(pr, b), 1e-10))
        }
    }
    else {
        xext = x
        for (H in h) {
          B <- matrix(0, p, p)
          for (i in 1:p[1]) {
                B[i, i:p] = sp.lacv$lacv[len - p + i, 1:length(i:p)]
            }
            tmp = diag(B)
            B = t(B) + B
            diag(B) = tmp
            if (p == 1) {
                RHS = 0
            }
            else {
                RHS <- c(0, sp.lacv$lacv[len + H, 2:p])
            }
            extra.var = sp.lacv$lacv[len + H, 1]
            pr = list(B = B, RHS = RHS, extra.var = extra.var)
            class(pr) = "predeq"
            if (regularize == TRUE) {
                pr = reg.xyr(pr)
            }
            b <- solve(pr$B, pr$RHS)
            pmean[H] = sum(xext[(length(xext) - p + 1):length(xext)] * 
                b)
            xext = c(xext, pmean[H])
            std.err[H] <- sqrt(max(sig.sq(pr, b), 1e-10))
        }
    }
    out = list(mean = pmean, std.err = std.err, lpacf = lpacf, 
        ci = ci, binwidth = sp.lacv$binwidth, p = p, x = xdata, d=0)
    class(out) = "forecastlpacf"
    return(out)
}
