derivativeSimple<-function(Data, windowLength){
  LData<-length(Data)
  ResSlope<-rep(NA, LData)
  ResSlope2<-rep(NA, LData)
  ResFit<-rep(NA, LData)
  ResFit2<-rep(NA, LData)
  X<-c(1:windowLength)
  
  SecDerWindow<-10
  
  for(Index in 1:(LData - windowLength + 1)){
    ResIndex<-Index+windowLength -1
    Y<-Data[Index:ResIndex]
    LM<-lm(Y~X)
    Slope<-LM$coefficients[2]
    ResSlope[ResIndex]<-Slope
    Fit<-ave(LM$fitted.values)
    ResFit[ResIndex]<-tail(Fit, 1)
    
#     
#     LM2<-lm(ResSlope[Index:~X)
#     Slope2<-LM2$coefficients[2]
#     ResSlope2[ResIndex]<-Slope2
#     Fit2<-LM2$fitted.values
#     ResFit2[ResIndex]<-tail(Fit2, 1)
  }
  return(list(slope=ResSlope, fit=ResFit))#, slope2=ResSlope2, fit2=ResFit2))
}