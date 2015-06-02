# stuody  of synthetic pairs


# Construct synth pairs of the 0th order for the time interval [DateS, DateS] 
#return: list(CommonItem = SynthPair, [Original = pairInPairsDB])
#TODO what if original doesn't exist
 synth0<-function(SynthItem1, SynthItem2, DateS, DateE, PairsDB, Frequency){
   if(missing(SynthItem1) | missing(SynthItem2)
      | missing(DateS) | missing(DateE)
      | missing(PairsDB)
      | missing(Frequency)) stop("Missing arguments!")
 
   require(zoo)
   require(LaF)
   #funcs for handling big ASCI files are in io.R
   source("io.R")
   source("utils.R")
   source("plotting.R")
   parallelRegister()
   
   # To be able to construct our synth pair, our pairs must have a common item.
   CommonItems<-getCommonItems(SynthItem1, SynthItem2, PairsDB)
   #for every Item construct synth Pair
   print("calculating synthetic pairs")
    pRes<-foreach(Index=1:length(CommonItems)) %dopar% {
   #pRes<-foreach(Index=1:2) %dopar% {
     CommonItem<-CommonItems[[Index]]
     Pair1<-getPair(CommonItem,  SynthItem1, DateS, DateE, PairsDB, Frequency)
     Pair2<-getPair(CommonItem,  SynthItem2, DateS, DateE, PairsDB, Frequency) 
     m<-merge(Pair1$Data, Pair2$Data, fill=NA, all=TRUE)
     #‘last observation carried forward’
     m<-na.locf(m)
     
     BidColName1<-paste(Pair1$First, Pair1$Second, "Bid", sep="")
     BidColName2<-paste(Pair2$First, Pair2$Second, "Bid", sep="")
     AskColName1<-paste(Pair1$First, Pair1$Second, "Ask", sep="")
     AskColName2<-paste(Pair2$First, Pair2$Second, "Ask", sep="")
     
     B1<-m[, BidColName1]
     A1<-m[, AskColName1]
     B2<-m[, BidColName2]
     A2<-m[, AskColName2]
     #allocate space
     nrows<-nrow(m)
     Bid<-rep(NA, nrows)
     Ask<-rep(NA, nrows)
     EntryName<-CommonItem
     #we know how to construct synthetic pairs from the following pair types:
     #B(Synth1/Synth2)= B1*B2 [Synth1/X, X/Synth2] or B2/A1 [X/Synth1, X/Synth2] or B1/A2? [Synth1/X, Synth2/X]
     if(Pair1$First == SynthItem1
        & Pair2$Second == SynthItem2){
       Bid<-B1 * B2
       Ask<-A1 * A2
       EntryName<-paste(CommonItem, "1", sep="_")
     }
     else if(Pair1$Second == SynthItem1
             & Pair2$Second == SynthItem2){
       Bid<-B2 / A1
       Ask<-A2 / B1
       EntryName<-paste(CommonItem, "2", sep="_")
     }
     else if(Pair1$First == SynthItem1
             & Pair2$First == SynthItem2){
       Bid<-B1 / A2
       Ask<-A1 / B2
       EntryName<-paste(CommonItem, "3", sep="_")
     }
     else cat("Don't know how to calculate", paste(Pair1$First, Pair1$Second, sep=""), "and", paste(Pair2$First, Pair2$Second, sep=""))
     
     TmpRes<-merge(Ask, Bid, fill=NA, all=TRUE)
     ColNames<-c(paste(EntryName, "Ask", sep=""), paste(EntryName, "Bid", sep=""))
     TmpRes<-setNames(TmpRes, ColNames)
     m<-merge(TmpRes, m, fill=NA, all=TRUE)
   }
#    print("fetching original")
   Original<-NA
#    Original<-getPair(SynthItem1, SynthItem2, DateS, DateE, PairsDB, Frequency)
   return(list(Original=Original, pRes=pRes))
 }

 
#UTILS
is.weekend <- function(x) {
  x <- as.POSIXlt(x)
  x$wday > 5 | x$wday < 1
}
 
 
 dateToPosix<-function(DateStr){
   Date<- as.POSIXct(DateStr,  format="%Y.%m.%d %H:%M:%OS")
   return(Date)
 }
 
 
 filterSynthWindow<-function(DateStrS, DateStrE, Synth){
   require(zoo)
   
   ResLst<-list()
   DateS<-dateToPosix(DateStrS)
   DateE<-dateToPosix(DateStrE)
   Data<-Synth$pRes
   pRes<-foreach(Index=1:length(Data))%dopar%{
     DF<-Data[[Index]]
     Window<-window(DF, start=DateS, end=DateE)
   }
   return(list(pRes=pRes))
 }
 
 #returns data for pair on interval [DateS, DateE]
 # Date* args should have the same format as in Pair$Time
 getWindow<-function(Pair, DateSStr, DateEStr, Frequency){
   #since our data is in mse
   op<-options(digits.sec=3)
   DateS<- dateToPosix(DateSStr)
   DateE<- dateToPosix(DateEStr)
   #reducing the number of times dataframe is created and destroyed
   nrows<-10^5
   #set pointer to first byte
   begin(Pair)
   #init vals
   DF<-NA
   IndexS<-NA
   IndexE<-NA
   CurrIndex<-0
   #find our indices
   while(TRUE){
     DF<-next_block(Pair, columns=1, nrows)
     PDates<-as.POSIXct(DF$Time,  format="%Y.%m.%d %H:%M:%OS")
     #find the index of the first TRUE of the condition
     if(is.na(IndexS)) IndexS<-which(PDates >= DateS)[1] + CurrIndex
     if(is.na(IndexE)) IndexE<-which(PDates >= DateE)[1] + CurrIndex
     if(!is.na(IndexS) & !is.na(IndexE)) break
     #if we happen to come to the end of file without found indices, exit function and let us know
     if(nrow(DF) == 0) {
       if(is.na(IndexS)) stop("DateS not found")
       if(is.na(IndexE)) stop("DateE not found")
     }
     CurrIndex<-CurrIndex + nrows
   }
   
   #get sample data
   Sample<-seq(IndexS, IndexE, Frequency)
   DataWindow<-Pair[Sample]
   #column names
   PairName<-substring(Pair@filename, 1, 6)
   AskColName<-paste(PairName, "Ask", sep="")
   BidColName<-paste(PairName, "Bid", sep="")
   #convert to zoo object since we need indexed vectors for arithmetics  
   IndexedDF<-zoo(cbind(DataWindow$Ask, DataWindow$Bid), order.by=as.POSIXct(DataWindow$Time,  format="%Y.%m.%d %H:%M:%OS"))
   #exclude weekends
   Weekends<-is.weekend(time(IndexedDF))
   IndexedDF<-IndexedDF[!Weekends]
   #we can't just tell cbind to evaluate column names, it's a whole function for that...
   IndexedDF<-setNames(IndexedDF, c(AskColName, BidColName))
   return(IndexedDF)
 }
 
 
 #load all CSV files in dir and return named list
 loadPairsDB<-function(Dir){
   source("io.R")
   ResList<-list()
   #set curr working dir to Dir so we can easily parse file names
   oldWD<-getwd()
   filesInDir<-list.files(Dir)
   setwd(Dir)
   for(File in filesInDir){
     ItemName<-strsplit(File, "_")[[1]][1]
     ResList[ItemName]<-readCSV(File)
   }
   #reset working dir
   setwd(oldWD)
   return(ResList)
 }
 
 
 #return: list(Data=Window[DateS:DateE], First=FirstCurrStr, Second=SecondCurrStr)
 getPair<-function(Item1, Item2, DateS, DateE, PairsDB, Frequency){
   #init vals
   Pair<-NA
   Data<-NA
   First<-NA
   Second<-NA
   
   CurrPairStr<-paste(Item1,Item2, sep="")
   Pair<-PairsDB[[CurrPairStr]]
   if(!is.null(Pair)){
     First<-Item1
     Second<-Item2
   }
   else{
     CurrPairStr<-paste(Item2, Item1, sep="")
     Pair<-PairsDB[[CurrPairStr]]
     if(!is.null(Pair)){
       First<-Item2
       Second<-Item1
     }
     else return(NA)
   }
   
   Data<-getWindow(Pair, DateS, DateE, Frequency)
   if(is.null(Data)) {
     Data<-NA
     print("Data = Null!  You're doing sth wrong.")
   }
   
   return(list(Data=Data, First=First, Second=Second))
 }
 
 
 #Return currencies that are in pair with SynthItem1 and SynthItem2
 getCommonItems<-function(SynthItem1, SynthItem2, PairsDB){
   InPairWSItem1<-vector()
   InPairWSItem2<-vector()
   for(PairName in names(PairsDB)){
     Item1<-substring(PairName, 1, 3)
     Item2<-substring(PairName, 4, 6)
     
     #check affiliations
     # for SItem1
     if(SynthItem1 == Item1) InPairWSItem1<-c(InPairWSItem1, Item2)
     else if(SynthItem1 == Item2) InPairWSItem1<-c(InPairWSItem1, Item1)
     # and SItem2
     if(SynthItem2 == Item1) InPairWSItem2<-c(InPairWSItem2, Item2)
     else if(SynthItem2 == Item2) InPairWSItem2<-c(InPairWSItem2, Item1)
   }
   #  Intersection of lists provides us with wished candidates.
   CommonItems<-intersect(InPairWSItem1, InPairWSItem2)
   return(CommonItems)
 }
 
 
 mergingFun<-function(pRes, Original){
   PairsMerge<-Original
   ResMerge<-Original
   #TODO merge only pairs
   for(ListItem in  pRes){
     TmpRes<-ListItem[[1]]
     ResMerge<-merge(ResMerge, TmpRes, fill=NA, all=TRUE)
     
     Pairs<-ListItem[[2]]
     PairsMerge<-merge(PairsMerge, Pairs, fill=NA, all=TRUE)
   }
   
   print("handling NAs")
   #   TmpLst<-list(ResMerge, PairsMerge)
   #   pRes<-foreach(Index=1:2)%dopar%{
   #     Item<-TmpLst[[Index]]
   #     #‘last observation carried forward’
   #     m<-na.locf(Item)
   #   }
   #TODO papply(data, na.locf)
   ResMerge<-na.locf(ResMerge)
   PairsMerge<-na.locf(PairsMerge)
   return(list(Synth=ResMerge, Pairs=PairsMerge))
 }
 