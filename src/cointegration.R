testForCointegration <- function(Synth, Frequency){
  require("quantmod")
  require("PerformanceAnalytics")
  require("fUnitRoots")
  require("tseries")
  parallelRegister()
  setGraphingDevice()
  
  Data<-Synth$pRes
  #since the first is originalAsk and the second originalBid
  pRes<-foreach(Index = 1:length(Data))%dopar%{
    #synthPairAsk, synthPairBid, Pair1Ask, Pair1Bid, Pair2Ask, Pair2Bid
    DF<-Data[[Index]]
    DFLen<-length(DF[, 1])
    DF<-DF[seq(1, DFLen, Frequency)]
    Names<-names(DF)
    NameFirst<-Names[[3]]
    NameSecond<-Names[[5]]
    ItemAsk1<-DF[, 3]
    ItemAsk2<-DF[, 5]
    Pair<-list(a=ItemAsk1, b=ItemAsk2, NameFirst=NameFirst, NameSecond=NameSecond)
    CointegrationTestRes<-cointegrationTest(Pair)
  }
  
  #plot results
  for(TestResult in pRes){
    cointegrationPlot(TestResult)
  }
}

#Pass in a pair of stocks and do the necessary checks to see if it is cointegrated
cointegrationTest<-function(Pair){
  #Data preparation
  # cut where they both have data
  Stock1FirstDataIndex<-which(!is.na(Pair$a))[1]
  Stock2FirstDataIndex<-which(!is.na(Pair$b))[1]
  WhereBothHaveData<-max(Stock1FirstDataIndex, Stock2FirstDataIndex)
  End<-length(Pair$a)
  Pair$a<-Pair$a[WhereBothHaveData : End]
  Pair$b<-Pair$b[WhereBothHaveData : End]
  #Step 1: Calculate the daily returns
  dailyRet.a <- na.omit((Delt(Pair$a,type="log")))
  dailyRet.b <- na.omit((Delt(Pair$b,type="log")))
  dailyRet.a <- dailyRet.a[is.finite(dailyRet.a)] #Strip out any Infs (first ret is Inf)
  dailyRet.b <- dailyRet.b[is.finite(dailyRet.b)]
  #print(length(dailyRet.a))
  #print(length(dailyRet.b))
  #Step 2: Regress the daily returns onto each other
  #Regression finds BETA and C in the linear regression retA = BETA * retB + C
  regression <- lm(dailyRet.a ~ dailyRet.b + 0)
  beta <- coef(regression)[1]
  #print(paste("The beta or Hedge Ratio is: ",beta,sep=""))
  #Step 3: Use the regression co-efficients to generate the spread
  spread <- Pair$a - beta*Pair$b #Could actually just use the residual form the regression its the same thing
  
  #spreadRet[!is.na(spreadRet)]
  #Step 4: Use the ADF to test if the spread is stationary
  #can use tSeries library
  adfResults <- adf.test((spread),k=0,alternative="stationary")
  
  #calculate other stuff we need for plotting
  spreadRet <- Delt(spread, type="arithmetic")
  spreadRet <- na.omit(spreadRet)
  return(list(TestResult=adfResults, Spread=spread, SpreadReturn=spreadRet, Pair=Pair, DailyRet1=dailyRet.a, DailyRet2=dailyRet.b, Beta=beta))
}

#Plot the pairs with additional data
cointegrationPlot<-function(TestResList){  
  #expose data
  CointegrationTestResult<-TestResList$TestResult
  Pair<-TestResList$Pair
  spread<-TestResList$Spread
  spreadRet<-TestResList$SpreadReturn
  dailyRet.a<-TestResList$DailyRet1
  dailyRet.b<-TestResList$DailyRet2
  beta<-TestResList$Beta
  PairName<-paste(Pair$NameFirst, Pair$NameSecond)
  
  #plot
  par(mfrow=c(2,2))
  #print(c((0.99*min(rbind(Pair$a,Pair$b))),(1.01*max(rbind(Pair$a,Pair$b)))))
  plot(Pair$a, type="l", col="red", main=PairName)#, ylim=c((0.99*min(rbind(Pair$a,Pair$b))),(1.01*max(rbind(Pair$a,Pair$b)))))
  lines(x=dailyRet.b,y=(dailyRet.b*beta),col="blue")#Plot in linear line we used in the regression
  par(new=TRUE)
  plot(Pair$b, type="l", col="blue")
  par(new=FALSE)
  plot((spreadRet), type="l",main="Spread Ret") #Plot the cumulative sum of the spread
  plot(spread, type="l",main="CumSum Spread") #Plot the cumulative sum of the spread
  #For a cointegrated spread the cumsum should not deviate very far from 0
  #For a none-cointegrated spread the cumsum will likely show some trending characteristics
#   plot(x=dailyRet.b,y=dailyRet.a,type="p",main="Regression of Delt for A & B") #Plot the daily returns
#   lines(x=dailyRet.b,y=(dailyRet.b*beta),col="blue")#Plot in linear line we used in the regression
  SpreadActual<-Pair$a - Pair$b
  plot(SpreadActual, type="l", main="Spread Actual")
  
  #print results
  print(PairName)
  print(CointegrationTestResult)
#   if(adfResults$p.value <= 0.05){
#     print(paste("The spread is likely Cointegrated with a pvalue of ",adfResults$p.value,sep=""))
#   } else {
#     print(paste("The spread is likely NOT Cointegrated with a pvalue of ",adfResults$p.value,sep=""))
#   }
  # disable grid for plots
  par(mfrow=c(1,1))
  par(new=F)
}
  

#R> eur.usd <- eur.usd[!is.weekend(time(eur.usd))]
