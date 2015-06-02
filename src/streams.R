LabelFeaturesWrite<-function(laFObject){
  TimeStart<-proc.time()
  Ask<-laFObject$Ask
  
  # label stuff
  print("Starting labelling")
  TimeLabel<-proc.time()
  
  From<-10^1
  RowsToLabel<-500
  MinDiff<-0.0001
  WindowSize<-100
  Labels<-getLabelsSimpleTillExit(laFObject, From, RowsToLabel, MinDiff, WindowSize)

  print((proc.time() - TimeLabel))
  print("Labelling FIN")
  
  # calc features
  Features<-getFeatures(Labels)
  
  FilterRes<-filterFeaturesWMissingVals(Features, 0.2)
  
  CleanedFeatures<-cutNas(FilterRes$FilteredFeatures)
  
  # serialize
  writeLabeledCSV(Labels, CleanedFeatures)
  
  print((proc.time() - TimeStart))
  print("Stream FIN")
  return(list("Features"=CleanedFeatures, "Labels"=Labels))
}