#these are the general functions we use for the gender assignment project
library(binom)

calculateBinomialProportions<-function(noFirst,noSecond,expectedProportion,noTests=1,confidenceLevel=0.95)
{
  
  vector("numeric",8)->rr
  result.binom<-binom.test(noFirst,noSecond+noFirst, expectedProportion,conf.level=confidenceLevel)
  rr[1]<-noFirst
  rr[2]<-noSecond
  #simple aka conservative multitest correction
  rr[3]<-result.binom$p.value*noTests
  if(rr[3]>1) {rr[3]<- 1}
  rr[4]<-result.binom$p.value
  rr[5]<-result.binom$estimate
  rr[6]<-result.binom$null.value
  #we use AC as we are assuming this is high number count data
  result.confidence<-binom.confint(noFirst,noSecond+noFirst,method="agresti-coull",conf.level = confidenceLevel)
  rr[7]<-result.confidence$lower
  rr[8]<-result.confidence$upper
  
 
  names(rr)<-c("NoFirst","NoSecond","AdjustedPValue","PValue","ActualProportion","ExpectedProportion","LowerCI","UpperCI")
  
  rr
}

calculateBinomials<-function(inTable,expectedProportion,confidenceLevel=0.95)
{
  for(i in 1:nrow(inTable))
  {
    rr_row<-calculateBinomialProportions(inTable[i,1],inTable[i,2],expectedProportion,nrow(ukProportions),confidenceLevel)
    if(i==1)
      rr<-data.table(t(rr_row))
    else rr<-rbind(rr,t(rr_row))
  }
  
  
  rr[,CountLowerCI:=NoFirst*LowerCI/ActualProportion]
  rr[,CountUpperCI:=NoFirst*UpperCI/ActualProportion]
  
  if(!is.null(colnames(inTable)))
  {
    colnames(rr)[1]<-colnames(inTable)[1]
    colnames(rr)[2]<-colnames(inTable)[2]
  }
  if(!is.null(rownames(inTable)))
  {
    rr$Items<-rownames(inTable)
  }
  
  
  
  
  #flag to say if pval is less than significance threshold- which is presumed to be the same as the confidence level
  rr$Signficant<- rr$AdjustedPValue<(1-confidenceLevel)
  
  rr
}

#utility to convert binimal calcs table into something easier to plot
transformBinomialsForPlotting<-function(inTable)
{
  firstVar<-colnames(inTable)[1]
  secondVar<-colnames(inTable)[2]
  rr<-melt(inTable,measure.vars=colnames(inTable)[1:2])
  rr[variable==secondVar,ActualProportion:=1-ActualProportion]
  rr[variable==secondVar,ExpectedProportion:=1-ExpectedProportion]
  rr[variable==secondVar,TempLowerCI:=1-UpperCI]
  rr[variable==secondVar,UpperCI:=1-LowerCI]
  rr[variable==secondVar,LowerCI:=TempLowerCI]
  rr$TempLowerCI<-NULL
  colnames(rr)[ncol(rr)]<-"Number"
  colnames(rr)[ncol(rr)-1]<-"Criteria"
  rr[,CountLowerCI:=Number*LowerCI/ActualProportion]
  rr[,CountUpperCI:=Number*UpperCI/ActualProportion]

  rr
  
}


