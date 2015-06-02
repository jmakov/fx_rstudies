drawPrediction <- function(learnSeries, predictPeriods){
  len<-length(learnSeries) - predictPeriods
  etsfit<-ets(learnSeries[1:len])
  fcast<-forecast(etsfit, predictPeriods)
  
  intervalBefore<-len - (4*predictPeriods)
  intervalAfter<-len + (2*predictPeriods)
  
  plot(learnSeries[intervalBefore:intervalAfter], type="l", col="black")
  lines(fcast$fitted[intervalBefore: intervalAfter], col="orange")
  
  offset<-len - intervalBefore
  tmp<-offset: (offset+predictPeriods-1)
  
  lines(tmp, fcast$mean, col="red")
  lines(tmp,fcast$upper[,1],col="green")
  lines(tmp,fcast$upper[,2], col="blue")
  lines(tmp,fcast$lower[,1], col="green")
  lines(tmp,fcast$lower[,2], col="blue")
}

kalmanPredictDLM<-function(lGas, horizont){
  lmodel<-dlmModPoly()
  
  gasFilt <- dlmFilter(lGas, mod = lmodel)
  gasSmooth <- dlmSmooth(lGas, mod = lmodel)
  gasFore <- dlmForecast(gasFilt, nAhead = horizont)
  sqrtR <- sapply(gasFore$R, function(x) sqrt(x[1,2]))
  pl <- gasFore$a[,1] + qnorm(0.05, sd = sqrtR)
  pu <- gasFore$a[,1] + qnorm(0.95, sd = sqrtR)

  x <- ts.union(window(lGas, start=1), 
                window(gasSmooth$s[,1], start=1), 
                ts(gasFore$a[,1]))#, ts(pl), ts(pu))
  plot(x, plot.type = "single", type = 'o', pch = c(1, 0, 20, 3, 3),
       col = c("black", "darkgrey", "brown", "yellow", "yellow"),
       ylab = "Log gas consumption")
  legend("bottomright", legend = c("Observed",
                                   "Smoothed (deseasonalized)",
                                   "Forecasted level", "90% probability limit"),
         bty = 'n', pch = c(1, 0, 20, 3, 3), lty = 1,
         col = c("darkgrey", "darkgrey", "brown", "yellow", "yellow"))
}
kalmanPredictKFS<-function(series){
  smodel<-structSSM(series)
  kfsseries<-KFS(smodel, smoothing="state")
  predictNile<-predict(kfsseries)
  lows<-predictNile$y-qnorm( .95)*sqrt(c(predictNile$F))
  ups<-predictNile$y+qnorm( .95)*sqrt(c(predictNile$F))
  
  plot.ts(cbind(y,predictNile$y,lows,ups), plot.type="single", col=c(1:2,4,4),
          ylab="Predicted Annual flow", main="River Nile")
}