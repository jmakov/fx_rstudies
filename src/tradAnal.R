# strat debugging framework

# visualization
tradAnal<-function(DFList){
  require(playwith)
  playwith({
    # define margins
    par(mar=c(2,2,3,5)+.1)
    
    x<-as.numeric(DFList$pricesStats$date[,])
    ordersData=data.frame(x=DFList$orderStats$date[,], y=DFList$orderStats$price[,])
    
    # stuff on the left axis
    plot(x, y=DFList$pricesStats$usedLeverage[,], type="l", col="blue", xaxt="n",yaxt="n",xlab="",ylab="", lwd=1)
    axis(4, line=3, cex.axis=0.5)
    mtext("Margin used", side=4, col="blue", line=3)
    par(new=T)
      plot(x, y=DFList$pricesStats$equity[,], type="l", col="orange", xaxt="n",yaxt="n",xlab="",ylab="", lwd=2)
      axis(4, line=1, cex.axis=0.5)
      mtext("equity", side=4, col="orange", line=1)
    
    # stuff on the right axis
    par(new=T)
      plot(x, y=DFList$pricesStats$bid[,], type="l",yaxt="n",xlab="",ylab="")
      axis(2, cex.axis=0.5)
      mtext("pr", side=2, line=1)
      lines(x, DFList$pricesStats$ask[,])
      points(x=DFList$orderStats$date[,], y=DFList$orderStats$price[,], col=DFList$orderStats$position[,], lwd=0.5)
  }, main.function="points")
  
  playwith(plot(sort(DFList$orderStats$maxNegPL[,])), 
           new = TRUE, link.to = playDevCur())
  playwith(plot(sort(DFList$orderStats$age[,])), 
           new = TRUE, link.to = playDevCur())
  playwith(plot(DFList$orderStats$price[,], col=DFList$orderStats$position[,]), 
           new = TRUE, link.to = playDevCur())
}


#utils
tradStats<-function(DFList){
  hist(DFList$orderStats$maxNegPL[,])
  hist(DFList$orderStats$age[,])
}
loadTradStats<-function(directoryPath){
  PricesStats<-readPricesStats(directoryPath)
  OrderStats<-readOrderStats(directoryPath)
  DFList<-list(pricesStats=PricesStats, orderStats=OrderStats)
  tradStats(DFList)
  return(DFList)
}
readOrderStats<-function(directoryPath){
  require(LaF)
  fileName<-paste(directoryPath, "orderStats.csv", sep="")  
  myData<-laf_open_csv(filename=fileName,
                       column_types=c("string", "double", "string", "double", "double"),
                       column_names=c("date", "price", "position", "maxNegPL", "age"),
                       skip=1)
  return(myData)
}
readPricesStats<-function(directoryPath){
  require(LaF)
  fileName<-paste(directoryPath, "pricesStats.csv", sep="")  
  myData<-laf_open_csv(filename=fileName,
                       column_types=c("string", "double", "double", "double", "double"),
                       column_names=c("date", "bid", "ask", "equity", "usedLeverage"),
                       skip=1)
  return(myData)
}