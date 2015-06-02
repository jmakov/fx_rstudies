getChunk<-function(lafObject, offset, chunkSize){
  begin(lafObject)
  goto(lafObject, offset)
  chunk<-next_block(lafObject, nrows=chunkSize)
  return(chunk)
}
readCSV<-function(localFile){
  require(LaF)
  # reads Dukascopy's CSV file
  myData<-laf_open_csv(filename=localFile,
            column_types=c("string", "double", "double", "double", "double"),
            column_names=c("Time", "Ask", "Bid", "AskVol", "BidVol"),
            skip=1)
  return(myData)
}
writeLabeledCSV<-function(lbls, Features){
  csvUsed<-strsplit(lbls$Filename, "/")
  removedFileType<-strsplit(csvUsed[[1]][length(csvUsed[[1]])], ".csv")
  
  fileName<-paste("labeled", removedFileType, lbls$From, lbls$WindowSize, lbls$MinDiff, sep="_")
  
  print("Formatting data frame to nsmall=5")
  #DF<-format(Features, nsmall=5) #$Data will be messed up in 6+th digit, but other vals are ok
  
  print("Writing file")
  write.csv(Features, file=paste(fileName, "csv", sep="."), row.names=TRUE) 
  print("Fin ^l ^")
}
readTime<-function(Filename){
  Res<-read.zoo(Filename, regular=F, format="%Y.%m.%d %H:%M:%OS", header=TRUE, aggregate = function(x) tail(x, 1), colClasses=c("character", "NULL", "NULL", "NULL", "NULL"), sep=",", tz="")
  return(Res)
}