getLabelsSimple<-function (lafObject, from, rowsToLabel, minDiff, windowSize){
  # looks for exits in windowSize else hold
  
  To<-from + rowsToLabel - 1
  exportedData<-lafObject[from:To]
  Window<-lafObject[from:(To +  windowSize)]
  a<-Window$Ask
  b<-Window$Bid
  
  lbls<-vector(length=rowsToLabel)
  visualB<-vector(length=rowsToLabel)
  visualS<-vector(length=rowsToLabel)
  
  for(i in 1:(rowsToLabel)){
    # if nothing in horizon, hold
    action<-0
    visualLblsB<-NaN
    visualLblsS<-NaN
    for(j in 1:windowSize){
      if((b[i] - a[(i+j)]) > minDiff){ 
        action<-1
        visualLblsS<-b[i]
        break
      }
      else if((b[(i+j)] - a[i]) > minDiff){ 
        action<-2
        visualLblsB<-a[i]
        break
      }
    }
    
    lbls[i]<- action
    visualB[i]<-visualLblsB
    visualS[i]<-visualLblsS
  }
  
  return(list(Labels=lbls, VisualB=visualB, VisualS=visualS, WindowSize=windowSize, Data=exportedData,
              MinDiff=minDiff, From=from, To=To, Filename=lafObject@filename))
}
getLabelsWindowPercent<-function (lafObject, from, rowsToLabel, minDiff, windowSize, minExitPercent){
  # generates labels: 0 - hold, 1 - down, 2 - up
  
  Window<-lafObject[from:(from + rowsToLabel + windowSize)]
  a<-Window$Ask
  b<-Window$Bid
  
  lbls<-vector(length=rowsToLabel)
  visualB<-vector(length=rowsToLabel)
  visualS<-vector(length=rowsToLabel)
  
  for(i in 1:(rowsToLabel)){
    # if nothing in horizon, hold
    action<-0
    visualLblsB<-NaN
    visualLblsS<-NaN
    exitsa<-0
    exitsb<-0
    for(j in 1:windowSize){
      if((b[i] - a[(i+j)]) > minDiff){ 
        exitsb<-exitsb+1
      }
      else if((b[(i+j)] - a[i]) > minDiff){ 
        exitsa<-exitsa+1 
      }
    }
    ebPercent<-exitsb/windowSize
    eaPercent<-exitsa/windowSize
    if(ebPercent > minExitPercent){
      action<-1
      visualLblsS<-b[i]
    }
    else if(eaPercent > minExitPercent){
      action<-2
      visualLblsB<-a[i]
    }
    lbls[i]<- action
    visualS[i]<-visualLblsS
    visualB[i]<-visualLblsB
  }
  
  return(list(Labels=lbls, VisualB=visualB, VisualS=visualS, WindowSize=windowSize, Data=Window, MinDiff=minDiff))
}

