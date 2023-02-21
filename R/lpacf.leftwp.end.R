lpacf.leftwp.end=function (x, binwidth, lag.max=NULL, filter.number=1, family="DaubExPhase", smooth.dev=var,
	AutoReflect=TRUE, tol=0.1, maxits=5, ABBverbose=0, lapplyfn=lapply) 
{

  # this is the lpacf function from GPN 
  # this is still central facing, code for modified to be left facing is commented out
  # also modified to do non 2^J lengths
  # it only returns the lpacf for the final time point
  # thus alot of the function has been removed as we only need to do it for one timepoint

  # below has been taken from lacv.fc.leftwp
  # rest of function
  TT <- length(x)
  filter<-wavethresh::filter.select(filter.number,family)
  
  Jp <-ceiling(logb(TT,2))
  add<-2^Jp-TT
  
  Nh<-length(filter$H!=0) # NvSK says Nh is # of non-zero elements in the filter.
  
  # The support of the discrete wavelets is something like this:
  # Lj<-(2^j -1)*(Nh-1)+1
  #
  # For Haar wavelets, this is 2^j, not really surprising.  The modified periodogram in Fryzlewicz 2003
  # extends the left hand wavelet value to the left hand edge, for locations 0,...,Lj-2.
  #
  
  xa<-c(rep(0,times=add),x)
  lxa<-length(xa)#should be 2^(J+1) if TT not equal to  2^J
  
  dsname = deparse(substitute(x))
  
  if (missing(binwidth) || binwidth == 0) 
    binwidth <- locits::AutoBestBW(x = xa, filter.number = filter.number, 
                           family = family, smooth.dev = smooth.dev, AutoReflect = AutoReflect, 
                           tol = tol, maxits = maxits, plot.it = FALSE, 
                      
                                verbose = ABBverbose,ReturnAll=FALSE)
  
  if(binwidth>=TT){
    # i.e. binwidth is larger than data length
    binwidth=locits::AutoBestBW(x=x[(TT-2^{Jp-1}+1):TT],filter.number = filter.number, family = family, 
                        smooth.dev = smooth.dev, AutoReflect = AutoReflect,tol = tol, maxits = maxits,
                        plot.it = FALSE, verbose = ABBverbose,ReturnAll=FALSE)
  }
  
  if(binwidth<=lag.max){stop(paste('Automatic Bandwidth Selection not compatible with lag.max-step ahead forecast.  Choose a forecast horizon less than',binwidth,'.'))}
  
  # above has been taken from lacv.fc.leftwp  

  n <- length(x)

  if (is.null(lag.max)) 
        lag.max <- floor(10 * (log10(n)))

  start <- (n - round(binwidth/2)) # (n-binwidth+1) # commented is for left facing
  end <- n

  out=list(lpacf=pacf(x[seq(from=start,to=end)],lag.max=lag.max,plot=FALSE)$acf[,,1],binwidth=binwidth)
	class(out)='lpacf'
	return(out)

}

