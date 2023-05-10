forecastpanel=function(forecastobj,truth=NULL,add=FALSE,summary=TRUE,test="all",move=0,conf.level=95,col='red',pch=c(17,19,95),...){
  # function to plot the truth along with the forecast (and confidence intervals)
  # if add=F then it produces a new plot, add=T adds to an existing plot
  # also provides a summary of the fit if summary=T.
  # ... can be extra information for plotting
  
  # extract what we need depending on the inputs
  if(is(forecastobj,"forecastlpacf")){
    forecast=forecastobj$mean
    std.err=forecastobj$std.err
    lower=forecast-qnorm((100+conf.level)/200)*std.err
    upper=forecast+qnorm((100+conf.level)/200)*std.err
  }
  else if(is(forecastobj,"forecast")){
    levels=colnames(forecastobj$lower)
    levels=as.numeric(substr(levels,start=1,stop=2))
    index=match(conf.level,levels)
    if(is.na(index)){stop("conf.level and the precomputed confidence level within the forecastobj do not match.")}
    forecast=as.vector(forecastobj$mean)
    lower=forecastobj$lower[,index]
    upper=forecastobj$upper[,index]
  }
  else if(is.vector(forecastobj)){
    forecast=forecastobj
    lower=NULL
    upper=NULL
  }
  else{stop("Invalid argument: forecastobj. \n Please provide either a vector of forecasts or an object of class 'forecast' or 'forecastlpacf'.")}

  if(!add){
    plot(forecast,type='n',ylim=c(1.1*min(c(truth,lower),na.rm=T),1.1*max(c(truth,upper),na.rm=T)),...)
  }

  if(!is.null(truth)){points(truth,pch=pch[1],...)}
  points(1:length(forecast)+move,forecast,pch=pch[2],col=col)
  if(!is.null(lower) & !is.null(upper)){
    points(1:length(upper)+move,upper,pch=pch[3],col=col,lwd=3)
    points(1:length(lower)+move,lower,pch=pch[3],col=col,lwd=3)
    segments(1:length(lower)+move,lower,1:length(lower)+move,upper,col=col,...)
  }
  
  if(summary){
    if(is.null(truth)){warning("Cannot provide a summary of the forecast fit due to no true information, i.e. truth=NULL.")}
    else{
      return(accuracy(forecast,truth,test=test))
    }
  }
}