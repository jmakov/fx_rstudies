linReg<-function(y, from, to, windowSize){
  X<-1:(windowSize+1)
  VLen<-to-from
  InitVals<-vector()#rep(NaN, VLen)
  Coef<-InitVals
  FittedVal<-InitVals
  
  Index<-windowSize
  for(i in from:(to-windowSize)){
    Index<-Index+1
    Model<-lm(y[i: (i+windowSize)]~X)
    Coef[Index]<-Model$coefficient[2]
    #FittedVal[Index]<-tail(Model$fitted.values, 1)
    FittedVal[Index]<-mean(Model$fitted.values)
  }
  return(list(Coef=Coef, FittedVal=FittedVal))
}
