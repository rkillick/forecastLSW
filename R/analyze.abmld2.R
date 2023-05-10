analyze.abmld2=function(h=10,atTime=NULL,atLag=NULL){
  # function to analyze the abmld2 data set.

  data(abmld2, package = "forecastLSW", envir = environment())  
  abmld2 = get("abmld2")    

  #pdf(file='abmld2.ts.pdf')
  plot(seq(1956,2010.75,by=0.25),abmld2,type='l',xlab='Year',ylab='ABML second differences') 
  # second differences remove 3 observations, we remove these from the beginning for plotting purposes
  #dev.off()
  
  #pdf(file='abmld2.lpacf.pdf')
  abmld2.lpacf=lpacf::lpacf(abmld2)
  if(is.null(atTime)){atTime=1:nrow(abmld2.lpacf$lpacf)}
  else{atTime=which(!is.na(pmatch(abmld2.lpacf$the.x,atTime)))}
  if(is.null(atLag)){atLag=1:ncol(abmld2.lpacf$lpacf)}
  lpacf::lpacf.plot(abmld2.lpacf,atTime=atTime,atLag=atLag)
  #dev.off()
  
  #pdf(file='abmld2.forecast.pdf')
  #first do lpacf forecast
  abmld2.f.rec=forecastLSW::forecastlpacf(abmld2[1:(length(abmld2)-h)],h=h,forecast.type='recursive')
  abmld2.acc.rec=forecastLSW::forecastpanel(abmld2.f.rec,truth=abmld2[(length(abmld2)-h+1):length(abmld2)],xlab='Year',xaxt='n',lwd=3,move=-0.05,col='blue')
  axis(side=1,at=1:h,labels=seq(1956,2010.75,by=0.25)[(length(abmld2)-h+1):length(abmld2)])
  
  abmld2.f.fix=forecastLSW::forecastlpacf(abmld2[1:(length(abmld2)-h)],h=h,forecast.type='fixed')
  abmld2.acc.fix=forecastLSW::forecastpanel(abmld2.f.fix,truth=abmld2[(length(abmld2)-h+1):length(abmld2)],xlab='Year',xaxt='n',lwd=3,move=-0.1,col='red',add=TRUE)

  abmld2.f.ex=forecastLSW::forecastlpacf(abmld2[1:(length(abmld2)-h)],h=h,forecast.type='extend')
  abmld2.acc.ex=forecastLSW::forecastpanel(abmld2.f.ex,truth=abmld2[(length(abmld2)-h+1):length(abmld2)],xlab='Year',xaxt='n',lwd=3,move=0.05,col='green',add=TRUE)

  #now do ARMA forecast
  abmld2.fit=forecast::auto.arima(abmld2[1:(length(abmld2)-h)]) # no constraints on arima fit
  abmld2.f2=forecast::forecast(abmld2.fit,h=h)
  abmld2.acc2=forecastLSW::forecastpanel(abmld2.f2,truth=abmld2[(length(abmld2)-h+1):length(abmld2)],add=TRUE,col='orange',lwd=2,move=0.1)
  
  title(main='ABMLd2 forecasts: red=fixed, blue=recursive, green=extend, orange=arma')
  #dev.off()
  
  return(list(lpacf=abmld2.lpacf,forecastlpacf=list(fix=abmld2.f.fix,ex=abmld2.f.ex,rec=abmld2.f.rec),
              acc.lpacf=list(fix=abmld2.acc.fix,ex=abmld2.acc.ex,rec=abmld2.acc.rec),
              forecast.arma=abmld2.f2,acc.arma=abmld2.acc2))
}
