getFeatures<-function(lbls){
  # compute features of interest
  
  require(TTR)
  require(quantmod)
  # register the parallel backend
  parallelRegister()
  #we'll time feat construction function
  print("basic:start")
  StartTime<-proc.time()
  # calculate time scales of interest
  timeScales<-list(2,16,32,64)#2,4,8,16,32,64,128)
  
  Ask<-lbls$Data$Ask
  Bid<-lbls$Data$Bid
  AskVol<-lbls$Data$AskVol
  BidVol<-lbls$Data$BidVol
  ItemsOfInterest<-list(
    #TODO work on Delt(Ask, k=2,8,16 etc)
    "Ask"=Ask
    #"Bid"=Bid,
    #"AskVol"=AskVol
    #"BidVol"=BidVol
    )
  FunSingleVar<-list(
    #TODO
    #Delt - percent change for period k
    #skewness
    "CMO"=CMO,
    "DPO"=DPO,
    "DVI"=DVI,
    #"EMA"=EMA,
    "DEMA"=DEMA,
    "deriv"=derivativeSimple,
    #"integrate"=diffinv #diffinv doesn't handle NaN's, so you must
    "GMMA"=GMMA,
    "KST"=KST,
    "Lag"=Lag,
    "MACD"=MACD,
    "PBands"=PBands,
    "ROC"=ROC,
    "runPercentRank"=runPercentRank,
    "runSum"=runSum,
    "runMin"=runMin,
    "runMax"=runMax,
    "runMean"=runMean,
    "runMedian"=runMedian,
    "runSD"=runSD,
    "runMad"=runMAD,
    "runVar"=runVar,
    "RSI"=RSI,
    #"SMA"=SMA,
    "TDI"=TDI,
    "TRIX"=TRIX,
    "ZLEMA"=ZLEMA,
    "VHF"=VHF,
    "wilderSum"=wilderSum,
    "WMA"=WMA
    )
FunDoubleVar<-list(
  "EVWMA"=EVWMA,
  "fDiff"=fDiff,
  "OBV"=OBV,
  "runCor"=runCor,
  "runCov"=runCov,
  "VWAP"=VWAP
  )  
  
  # calculate features
  Spread<-fDiff(Ask, Bid, 0)
  SpreadVol<-fDiff(AskVol, BidVol, 0)
  BasicFeatures<-list(
    "fSingle"=fFunFactorySingleParFun(ItemsOfInterest, FunSingleVar, timeScales),
    "OBV"=OBV(Ask, AskVol),
    "spread"=Spread,
    "spreadVol"=SpreadVol
    #"fDouble"=fFunFactoryDouble(lbls, ItemsOfInterest, FunDoubleVar, timeScales)
  )
  print((proc.time() - StartTime))
  
  # get dynamics of features
  print("dynamic:start")
  StartTime<-proc.time()
  Derivatives<-fDerivatives(BasicFeatures, timeScales)
  Derivatives2<-fDerivatives(Derivatives, timeScales)
  print((proc.time() - StartTime))
  
  Features<-list(
    #"BasicFeatures"=BasicFeatures,  #exclude since not space invariant
    #"fDouble"=BasicFeatures["fDouble"],
    "div"=Derivatives,
    "div2"=Derivatives2,
    "spread"=Spread,
    "spreadVol"=SpreadVol,
    "Labels"=lbls$Labels,
    "Data"=lbls$Data
  )
  return(cbindPad(Features))
}


# ops on one var
fDecompositionBasic<-function(lbls, ItemsOfInterest, timeScales){
  TS<-ts
}
fDerivatives<-function(Features, timeScales){
  DF<-cbindPad(Features)
  FunOfInterest<-list("deriv2"=derivativeSimple)
  
  ResList<-fFunFactorySingleParItems(DF, FunOfInterest, timeScales)
  return(ResList)
}
fFunFactorySingleParItems<-function(ItemsOfInterest, FunctionsOfInterest, timeScales){
  # returns named list  
  ItemNames<-names(ItemsOfInterest)
  ResList<-list()
  #for(ItemName in names(ItemsOfInterest)){
  for(Scale in timeScales){
    for(FuncName in names(FunctionsOfInterest)){
      pRes<-foreach(Index=1:length(ItemsOfInterest), .errorhandling="remove") %dopar% {
        # select data to eval
        Y<-ItemsOfInterest[[Index]]
        ItemName<-ItemNames[Index]
        # call funct of interest
        Fun<-FunctionsOfInterest[[FuncName]]
        FunArgs<-list(Y, Scale)
        TmpRes<-do.call(Fun, FunArgs)
        
        FeatureName<-paste(ItemName, FuncName, Scale, sep="_")  
        ReturnedRes<-list(FeatureName, TmpRes)
        }
      # construct named list from results
      for(Item in pRes){
        FeatName<-Item[[1]]
        Res<-Item[[2]]
        ResList[[FeatName]]<-Res
      }
    }
  }
  return(ResList)
}
fFunFactorySingleParFun<-function(ItemsOfInterest, FunctionsOfInterest, timeScales){
  # returns named list  
  FunNames<-names(FunctionsOfInterest)
  ResList<-list()
  
  for(Scale in timeScales){
    for(ItemName in names(ItemsOfInterest)){
      pRes<-foreach(Index=1:length(FunctionsOfInterest), .errorhandling="remove") %dopar% {
        # select data to eval
        Y<-ItemsOfInterest[[ItemName]]
        # call funct of interest
        FunName<-FunNames[[Index]]
        Fun<-FunctionsOfInterest[[FunName]]
        FunArgs<-list(Y, Scale)
        
        TmpRes<-do.call(Fun, FunArgs)
        
        FeatureName<-paste(ItemName, FunName, Scale, sep="_")
        ReturnedRes<-list(FeatureName, TmpRes)
      }
      # construct named list from results
      for(Item in pRes){
        FeatName<-Item[[1]]
        Res<-Item[[2]]
        ResList[[FeatName]]<-Res
      }
    }
  }
  return(ResList)
}

# ops on two vars
fDiff<-function(Y, X, lagging){
  Res<- Y - Lag(X, lagging) 
  return(Res)
}
fFunFactoryDouble<-function(lbls, ItemsOfInterest, FunctionsOfInterest, timeScales){
  # returns named list
  
  ResList<-list()
  From<-lbls$From
  To<-lbls$To
  FuncNames<-names(FunctionsOfInterest)
  for(ItemName in names(ItemsOfInterest)){
    #all permutations
    for(ItemName2 in names(ItemsOfInterest)){
      for(Scale in timeScales){
        pRes<-foreach(Index=1:length(FunctionsOfInterest)) %dopar% {
      #for(FuncName in names(FunctionsOfInterest)){
        #pRes<-foreach(Index=1:length(timeScales)) %dopar% {
          # select data to eval
          #Scale<-timeScales[[Index]]
          Y<-ItemsOfInterest[[ItemName]][From : To]
          X<-ItemsOfInterest[[ItemName2]][From : To]
          # call funct of interest
          #Fun<-FunctionsOfInterest[[FuncName]]
          Fun<-FunctionsOfInterest[[Index]]
          FuncName<-FuncNames[Index]
          if(FuncName=="OBV"){
            FunArgs<-list(Y, X)}
          else{FunArgs<-list(Y, X, Scale)}
          TmpRes<-do.call(Fun, FunArgs)
          FeatureName<-paste(ItemName, ItemName2, FuncName, Scale, sep="_")
          ReturnedRes<-list(FeatureName, TmpRes)
        }
        # construct named list from results
        for(Item in pRes){
          FeatName<-Item[[1]]
          Res<-Item[[2]]
          ResList[[FeatName]]<-Res
        }
      }
    }
  }
  return(ResList)
}

