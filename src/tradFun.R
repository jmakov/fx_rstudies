#TODO renormalization every n ticks, win length?
# handle stationarity of the process or triggers?
# the goal is to have a stationary series with the same triggers all the time
#TODO optimiization:par normalization window length

# EXPERIMENTS
# VOLATILITY
# min dist to still + after fees
# saturate chan for B and A at t
# opt par: place only in (std dev (WinL1) - Par) from average WinL2
# - w M ord
# - w L ord -> how to simulatie?
#pairT
#1. set hedging factor based on some window. run tradeFun - 1 par opti
#2. rolling hedging factor: set hedgin factor every n ticks  based on some window. run tradeFun - 2 par opti
#testing orders:
# first order: when sigma apart, place for crossing, hedging factor + sigma average last hour, trigger when crossed

#under/overPriced
#1.  for mean of window triggers sigma - 2 par opti (wLen, sigma)

gridOptimization<-function(LaFObject, From, ChunkSize){
  # register the parallel backend
  require(TTR)
  parallelRegister()
  
  ResList<-list()
  Data<-LaFObject[From:(From+ChunkSize)]
  SlopeVec<-seq(1e-6, 1e-5, 1e-6)
  
  for(WLen in seq(10, 30, 10)){
    pRes<-foreach(Index=1:length(SlopeVec)) %dopar% {
        # select data to eval
        Slope<-SlopeVec[Index]
        # call funct of interest
        FunArgs<-list(LaFObject, Data, WLen, Slope)
        TmpRes<-do.call(tradFun, FunArgs)
        
        FeatureName<-paste(WLen, Slope, sep="_")  
        ReturnedRes<-list(FeatureName, TmpRes)
      }
      # construct named list from results
      for(Item in pRes){
        FeatName<-Item[[1]]
        Res<-Item[[2]]
        ResList[[FeatName]]<-Res
      }
    }
    return(ResList)
}
tradFun<-function(Data, WLen, Trigger){
  # we enter position with Trigger and wait till pos
  require(TTR)
  #TODO: stoploss
  BidSize<-1000
  Pip<-0.0001
  WLenMaSmall<-50
  WLenMaLarge<-1000
  
  WLenRegSmaLarge<-10

  Asks<-Data$Ask
  Bids<-Data$Bid
  RevVec<-rep(NA, length(Asks))
  EnterB<-rep(NA, length(Asks))
  EnterS<-rep(NA, length(Asks))
  #calc slopes on asks
  RegRes<-derivativeSimple(Asks, WLen)
  SlopeVec<-RegRes$slope
  FitVec<-RegRes$fit
  
  Ma<-ZLEMA(Asks, WLenMaSmall)
  Ma2<-ZLEMA(Asks, WLenMaLarge)
  #RegResSma<-derivativeSimple(Sma, WLenRegSmaLarge)
#   WLenReg2<-30
#   RegRes2<-derivativeSimple(SlopeVec[WLen:length(SlopeVec)], WLenReg2)
#   SlopeVec2<-c(rep(NA, WLen), RegRes2$slope)
  
  # trad rules
  #to jump over NAs from derivativeSimple
  Index<-WLen + WLenMaSmall + WLenMaLarge# + WLenReg2 #+WLenSmaLarge
  CurrRev<-0
  End<-length(Asks)
  while(Index < End){
#     Slope<-SlopeVec[Index]
#     Slope2<-SlopeVec2[Index]
    #find relevant
    #if(abs(Slope) >= Trigger){   #slope cond
    if((Ma2[Index] - Ma[Index]) > 0.000001){
    #   & Slope > 0){   #intersection of moving averages
    #  EnterB[Index]<-Asks[Index]
      #buy cond
#        if(Slope > 0 
#           & Slope2 > 0){
#         Entered<-Asks[Index]  #enter position at ask
         EnterB[Index]<-Asks[Index]
       
#         Interval<-Index:End
#         for(Ind2 in Interval){
#           Bid<-Bids[Ind2]
#           Ask<-Asks[Ind2]
#           SlopeInner<-SlopeVec[Ind2]
#           #exit conditions
#           #if((Entered+Pip) < Bid){  #take profit
#           if(SlopeInner < 0){
#           #if(Entered < Bid | SlopeInner < 0){
#             CurrRev<-CurrRev + (Bid-Entered)
#             RevVec[Ind2]<-CurrRev
#             break
#           }
        
          #stopLoss
#           Loss<-Entered-Bid
#           if(Loss > (10*Pip)){
#             CurrRev<-CurrRev + (Bid-Entered)
#             RevVec[Ind2]<-CurrRev
#             break
#           }
#           RevVec[Ind2]<-CurrRev
#           # and once found continue from that pos
#           Index<-Index+1
#         }
      }
     #sell cond
#       else if(Slope < 0){
#         Entered<-Bids[Index]
#         #search for first exit
#         Interval<-Index:End
#         for(Ind2 in Interval){
#           Ask<-Asks[Ind2]
#           SlopeInner<-SlopeVec2[Ind2]
#           if(Entered > Ask | SlopeInner > 0){
#           #if(SlopeVec2[Ind2] > 0){
#             CurrRev<-CurrRev + (Entered-Ask)*BidSize
#             RevVec[Ind2]<-CurrRev
#             break
#           }
#           RevVec[Ind2]<-CurrRev
#           # and once found continue from that pos
#           Index<-Index+1
#         }
#       }
#     }
    Index<-Index+1
  }
  # set margins for plot
  par(new=F)
  par(mar=c(5, 7, 1, 3))
  
  plot(Bids, type="l", cex.axis=0.7, ylab="")
  points(EnterB, col="green", lwd=1)
  lines(Asks)
  #lines(FitVec, col="green", lwd=2)
  lines(Ma, col="red", lwd=1)
  lines(Ma2, col="blue")
  
  
  # set grid
  abline(v=(seq(0,End,25)), col="black", lty="dotted")
  par(new=T)
  abline(h=(seq(0,2,0.0001)), col="black", lty="dotted")
  
  #indicators
#   par(new=T)
#   plot(SlopeVec, type="l", col="blue", axes=F, xlab="", ylab="")
#   axis(2, line=3, col="blue", cex.axis=0.7)
#   par(new=T)
#   plot(SlopeVec2, type="l", col="red", axes=F, xlab="", ylab="", lwd=1)
#   axis(2, line=5, col="red", cex.axis=0.7)
  
  #rev curve
#   par(new=T)
#   plot(RevVec, type="l", col="red", axes=F, xlab="", ylab="", lwd=3)
#   axis(4, col="red", cex.axis=0.7)
#   mtext("rev",side=4, col="red")
  
  return(RevVec)
}