lacv.fc.leftwp<-function(x, h=0, filter.number = 1, family = "DaubExPhase", smooth.dev = var, AutoReflect = TRUE, lag.max = NULL, WPsmooth.type = "RM", ...){
  # modification of the lacv function in the locits package
  # modified to do non-2^J 
  # left facing code is in the below but is commented out - thus this is still a central window calculation
  # modified to do automatic bandwidth selection
  # Also removed return of most of the list which we do not require and changed default to Haar
  
  # Added h parameter for forecast horizon, uses Running Mean smoother to do the forecast on the spectral level
  # Defaults to 0 which is no forecast, i.e. same as lacv.leftwp function

  # this function is not exported so checks on inputs are done by parent functions.
  
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
  binwidth=locits::AutoBestBW(x=xa,filter.number = filter.number, family = family, 
                      smooth.dev = smooth.dev, AutoReflect = AutoReflect,...)
  if(binwidth>=TT){
    # i.e. binwidth is larger than data length
    binwidth=locits::AutoBestBW(x=x[(TT-2^{Jp-1}+1):TT],filter.number = filter.number, family = family, 
                        smooth.dev = smooth.dev, AutoReflect = AutoReflect,...)
  }
  
  if(binwidth<=h){stop(paste('Automatic Bandwidth Selection not compatible with h-step ahead forecast.  Choose a forecast horizon less than',binwidth,'.'))}
  
  Pwd <- locits::ewspec3(x = xa, filter.number = filter.number, family = family, 
                 smooth.dev = smooth.dev, AutoReflect = AutoReflect, WPsmooth.type = WPsmooth.type, 
                 binwidth = binwidth, ...)$S
  
  P<-matrix(Pwd$D,ncol=lxa,byrow=TRUE)
  
  K<-(2^(1:Jp)-1)*(Nh-1)+1
  Jt<-max(which(K<=TT))
  
  # modify left periodogram edge:
#   Smat <- matrix(NA, nrow = TT+h, ncol = Jt) # extra rows for the forecast points to be added below
#   for (j in 1:Jt){
#     Lj<-K[j]
#     Smat[Lj:TT,j]<-P[j,(1+add):(lxa-Lj+1)]
#     Smat[1:(Lj-1),j]<-rep(Smat[Lj,j],times=Lj-1)
#   }
  # keep central periodogram but get rid of the artefacts introduced by the non 2^J and put some blank spaces ready for the extrapolation
   Smat <- matrix(NA, nrow = TT+h, ncol = Jt) # extra rows for the forecast points to be added below
   for (j in 1:Jt){
     Lj<-K[j]
     Smat[1:TT,j]<-P[j,(1+add):lxa]
   }

  # forecast the spectrum at the end of the data
  if(h>0){
    for(htmp in 1:h){
      Smat[TT+htmp,]=apply(Smat[(TT-binwidth+htmp):(TT+htmp-1),],2,mean) 
      # does an extrapolated average that uses previous extrapolations
    }    
  }
  
  #  S <- EWS$S
  #  J <- Pwd$nlevels
  Psi <- PsiJmat(-Jt, filter.number = filter.number, family = family)
  nc <- ncol(Psi)
  L <- (nc - 1)/2
  dimnames(Psi) <- list(NULL, c(-L:0, 1:L))
  if (is.null(lag.max)) 
    the.lacv <- Smat %*% Psi[, (L + 1):ncol(Psi)]
  else {
    if (L + 1 + lag.max > ncol(Psi)) {
      warning(paste("lag.max too high. Have reset it to ", 
                    ncol(Psi) - L - 1, ". Higher lags are zero"))
      lag.max <- ncol(Psi) - L - 1
    }
    the.lacv <- Smat %*% Psi[, (L + 1):(L + 1 + lag.max)]
  }
  the.lacor <- sweep(the.lacv, 1, the.lacv[, 1], FUN = "/")
  out=list(lacv = the.lacv, lacr = the.lacor,S=Smat,binwidth=binwidth,filter.number=filter.number,family=family)
  class(out)='lacv'
  return(out)
}
