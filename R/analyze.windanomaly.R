analyze.windanomaly=function(h=10,atTime=NULL,atLag=NULL){
  # function to analyze the windanomaly data set.

  data(windanomaly, package = "forecastLSW", envir = environment())
  windanomaly = get("windanomaly")  

  #pdf(file='windanomaly.ts.pdf')
  plot(seq(1900,2005+5/12,by=1/12),windanomaly,type='l',xlab='Year',ylab='Wind Anomaly Index (m/s)') 
  # second differences remove 3 observations, we remove these from the beginning for plotting purposes
  #dev.off()
  
  #pdf(file='windanomaly.lpacf.pdf')
  windanomaly.lpacf=lpacf::lpacf(windanomaly)
  if(is.null(atTime)){atTime=1:nrow(windanomaly.lpacf$lpacf)}
  else{atTime=which(!is.na(pmatch(windanomaly.lpacf$the.x,atTime)))}
  if(is.null(atLag)){atLag=1:ncol(windanomaly.lpacf$lpacf)}
  lpacf::lpacf.plot(windanomaly.lpacf,atTime=atTime,atLag=atLag)
  #dev.off()
  
  #pdf(file='windanomaly.forecast.pdf')
  #first do lpacf forecast
  windanomaly.f.rec=forecastLSW::forecastlpacf(windanomaly[1:(length(windanomaly)-h)],h=h,forecast.type='recursive')
  windanomaly.acc.rec=forecastLSW::forecastpanel(windanomaly.f.rec,truth=windanomaly[(length(windanomaly)-h+1):length(windanomaly)],xlab='Year',xaxt='n',lwd=3,move=-0.05,col='blue')
  axis(side=1,at=1:h,labels=round(seq(1900,2005+5/12,by=1/12)[(length(windanomaly)-h+1):length(windanomaly)],2))
  
  windanomaly.f.fix=forecastLSW::forecastlpacf(windanomaly[1:(length(windanomaly)-h)],h=h,forecast.type='fixed')
  windanomaly.acc.fix=forecastLSW::forecastpanel(windanomaly.f.fix,truth=windanomaly[(length(windanomaly)-h+1):length(windanomaly)],xlab='Year',xaxt='n',lwd=3,move=-0.1,col='red',add=TRUE)
  
  windanomaly.f.ex=forecastLSW::forecastlpacf(windanomaly[1:(length(windanomaly)-h)],h=h,forecast.type='extend')
  windanomaly.acc.ex=forecastLSW::forecastpanel(windanomaly.f.ex,truth=windanomaly[(length(windanomaly)-h+1):length(windanomaly)],xlab='Year',xaxt='n',lwd=3,move=0.05,col='green',add=TRUE)
  
  #now do ARMA forecast
  windanomaly.fit=forecast::auto.arima(windanomaly[1:(length(windanomaly)-h)]) # no constraints on arima fit
  windanomaly.f2=forecast::forecast(windanomaly.fit,h=h)
  windanomaly.acc2=forecastLSW::forecastpanel(windanomaly.f2,truth=windanomaly[(length(windanomaly)-h+1):length(windanomaly)],add=TRUE,col='orange',lwd=2,move=0.1)
  
  title(main='windanomaly forecasts: red=fixed, blue=recursive, green=extend, orange=arma')
  #dev.off()
  
  return(list(lpacf=windanomaly.lpacf,forecastlpacf=list(fix=windanomaly.f.fix,ex=windanomaly.f.ex,rec=windanomaly.f.rec),
              acc.lpacf=list(fix=windanomaly.acc.fix,ex=windanomaly.acc.ex,rec=windanomaly.acc.rec),
              forecast.arma=windanomaly.f2,acc.arma=windanomaly.acc2))
}
