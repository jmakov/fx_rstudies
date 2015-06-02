plotChunk<-function(lafObject, offset, chunkSize){
  chunk<-getChunk(lafObject, offset, chunkSize)
  plot(chunk$Ask, type="l", col="red")
  lines(chunk$Bid, col="green")
}


plotTurningPoints<-function(lafObject, offset, chunkSize){
  require(pastecs)
  chunk<-getChunk(lafObject, offset, chunkSize)
  plot(chunk$Ask, type="l")
  lines(chunk$Bid)
  TP<-turnpoints(chunk$Ask)
  lines(TP)
}


plotLabels<-function(lblsObject){
  a=lblsObject$Data$Ask
  b=lblsObject$Data$Bid
  plot(a, type="l", main=lblsObject$WindowSize, sub=lblsObject$MinDiff)
  lines(b)
  points(lblsObject$VisualB, col="green", cex=.4)
  points(lblsObject$VisualS, col="red", cex=.4)
}


plotMultiple<-function(...){
  Items<-list(...)
  Dots<-substitute(list(...))[-1]
  Names<-sapply(Dots, deparse)
  Colors<-rainbow(length(Items))
  Index<-0
  for(Stuff in Items){
    Index<-Index+1
    plot(Stuff, type="l", col=Colors[Index], lwd=2)
    par(new=TRUE)
  }
  legend("bottomright", legend=Names, col=Colors, lty=1, lwd=4, cex=.7)
  par(new=FALSE)
}


plotChunkMultiple<-function(LstLaf, Offset, To){
  Names<-sapply(LstLaf, deparse)
  Colors<-rainbow(length(LstLaf))
  Index<-0
  for(Stuff in LstLaf){
    Index<-Index+1
    plot(Stuff$Ask[Offset: To], type="l", col=Colors[Index], lwd=2)
    par(new=TRUE)
    plot(Stuff$Bid[Offset: To], type="l", col=Colors[Index], lwd=2)
    par(new=TRUE)
  }
  legend("bottomright", legend=Names, col=Colors, lty=1, lwd=4, cex=.7)
  par(new=FALSE)
}


#plot synth pairs vs original
plotSynthPairsChunk<-function(Synth, From, To){
#   setGraphingDevice()
  
  Original<-Synth$Original$Data[From:(To-1)]
  pRes<-Synth$pRes
  
  Names<-names(Synth)
  Pip<-0.0001
  
  Max<-max(Data[,1], na.rm=T)
  Min<-min(Data[,1], na.rm=T)
  plot(Original[, 1], type="l", ylim=c(Min-10*Pip, Max+10*Pip))
  lines(Original[, 2])
  Colors<-rainbow(length(Names))
  #for(Index in seq(3, length(Names), 2)){
  for(Index in 1:length(pRes)){
    Data<-pRes[[Index]]
    SynthAsk<-Data[,1]
    SynthBid<-Data[, 2]
    lines(SynthAsk, col=Colors[Index])
    lines(SynthBid, col=Colors[Index])
  }
}


#plot pairs that constitute synthetic pairs vs original
plotPairsChunk<-function(Synth){
  setGraphingDevice()
#   Original<-Synth$Original$Data[Sample]
  Data<-Synth$pRes
  
  Colors<-rainbow(length(Data))
  LC<-length(Colors)/2
  plotPair<-function(Colors, PlottingData){
    #rstudio bug
    Sys.sleep(0.01)
    
    Names<-names(PlottingData)
      #par(new=F)
      #plot original
#       plot(Original[, 1], type="l")
#       lines(Original[, 2])
      #plot pairs that constitute synthetic pair of the original
#       par(new=TRUE)
      spread<-PlottingData[, 3] - PlottingData[, 5]
      plot(PlottingData[, 3], col=Colors[1], type="l")
      lines(PlottingData[, 4], col=Colors[1])
      par(new=TRUE)
        plot(PlottingData[, 5], col=Colors[LC], type="l")
        lines(PlottingData[, 6], col=Colors[LC])
      par(new=TRUE)
        plot(spread, col="black", type="l")  
      legend("bottomright", legend=c(Names[[3]], Names[[5]], "spread"), col=c(Colors[[1]], Colors[[LC]], "black"), lty=1, lwd=4, cex=.7)
  }
  for(Index in 1:length(Data)){
    DF<-Data[[Index]]
    SampleRate<-getSampleRate(DF[, 1])
    PlottingData<-DF[SampleRate]
    plotPair(Colors, PlottingData)
  }
}


#UTILS
getSampleRate<-function(Vector){
  SampleRate<-NA
  VecLen<-length(Vector)
  From<-1
  To<-VecLen -1
  if(VecLen > 10^5) SampleRate<-seq(From, To, 1)
  else if(VecLen > 10^6) SampleRate<-seq(From, To, 10)
  else SampleRate<-seq(From, To, 1)
  return(SampleRate)
}

#clear all switches to graphing device X11cairo_2 [dev.cur()] but we want our graphs in Rstudio, so check if X11 set and correct
setGraphingDevice<-function(){
  CurrentGraphingDevice<-dev.cur()
  while(CurrentGraphingDevice != 1){
    dev.off()
    CurrentGraphingDevice<-dev.cur()
  }
}
