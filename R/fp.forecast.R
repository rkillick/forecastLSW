fp.forecast <-
function (x, h=1, conf.level=95) 
{
#conf.level <- conf.level*100

aax <- forecast::auto.arima(x)

ans <- forecast::forecast(aax, h=h, level=conf.level)


return(cbind(ans$mean, ans$lower, ans$upper))
}
