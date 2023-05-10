testforecast <-
function (x, n.to.test, go.back = 0, plot.it = TRUE, regularize = TRUE, 
          lag.max = max(10, 2 * n.to.test), truth.pch = 23, truth.col = 3, 
          zoom = TRUE, zoom.no = 30, forecast.type = NULL, conf.level = 0.95, 
          stycol = 6, silent = TRUE, lapplyfn = lapply, ...) 
{
  if (go.back == 0) {
    x.orig <- x
    lx <- length(x)
    remove.ids <- (lx - n.to.test + 1):lx
    x <- x[-remove.ids]
    if (length(x) <= 0) 
      stop("You can't remove that many points\n")
    my.pred <- forecastlpacf(x = x, h = n.to.test, regularize = regularize, 
                              lag.max = lag.max, forecast.type = forecast.type, 
                              ...)
    st.pred <- fp.forecast(x = x, h = n.to.test, conf.level = conf.level)
    sy <- range(x.orig)
    sy <- sy[2] - sy[1]
    if (plot.it == TRUE) {
      plot(my.pred, extra.y = c(x.orig[remove.ids],st.pred[,2],st.pred[,3]), zoom = zoom, 
           zoom.no = zoom.no, conf.level = conf.level,ylab='')
      points(remove.ids, x.orig[remove.ids], pch = truth.pch, 
             col = truth.col, bg = truth.col)
      points(remove.ids, st.pred[, 1], col = stycol)
      for (i in 1:length(remove.ids)) {
        ci.sym(remove.ids[i], st.pred[i, 2], xdel = lx * 
                 0.001, ydel = -sy * 0.01, col = stycol)
        ci.sym(remove.ids[i], st.pred[i, 3], xdel = lx * 
                 0.001, ydel = sy * 0.01, col = stycol)
      }
    }
    ansm <- cbind(x.orig[remove.ids], my.pred$mean, my.pred$std.err, 
                  st.pred[, 1])
    dimnames(ansm) <- list(NULL, c("True", "LSPred", "StdErr", 
                                   "SPred"))
    s.mse <- sqrt(sum((st.pred[, 1] - x.orig[remove.ids])^2))
    l.mse <- sqrt(sum((my.pred$mean - x.orig[remove.ids])^2))
    if (silent == FALSE) {
      cat("Box-Jenkins RMSE: ", s.mse, "\n")
      cat("LSW/LPACF RMSE: ", l.mse, "\n")
    }
    return(ansm)
  }
  else {
    s.mse <- l.mse <- rep(0, n.to.test)
    l.in <- vector("list", go.back + 1)
    lx <- length(x)
    for (g in 0:go.back) {
      l.in[[g + 1]] <- x[1:(lx - g)]
    }
    l.out <- lapplyfn(l.in, testforecast, n.to.test = n.to.test, 
                      plot.it = FALSE, regularize = regularize, lag.max = lag.max, 
                      forecast.type = forecast.type, conf.level = conf.level, 
                      silent = silent, ...)
    for (g in 0:go.back) {
      ans <- l.out[[g + 1]]
      s.mse <- s.mse + (as.numeric(ans[, 1] - ans[, 4]))^2
      l.mse <- l.mse + (as.numeric(ans[, 1] - ans[, 2]))^2
    }
    s.mse <- s.mse/(go.back + 1)
    l.mse <- l.mse/(go.back + 1)
    s.mse <- sqrt(s.mse)
    l.mse <- sqrt(l.mse)
    ll <- data.frame(StyRMSE = s.mse, LocStyRMSE = l.mse, 
                     row.names = as.character(1:n.to.test))
    return(ll)
  }
}
