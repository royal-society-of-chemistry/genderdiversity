library(ggplot2)
library(forcats)
library(reshape2)
library(plyr)
library(knitr)
library(data.table)
library(dplyr)


mround <- function(number, multiple) {
  multiple * round(number/multiple)
}

bt <- function(inputx, inputn, inputp, i) {
  btoutput<-binom.test(x=inputx, n=inputn,p=inputp,alternative = "two.sided",conf.level = 0.95)
  if (i==1) { 
    100*btoutput$conf.int[i]
  } else {
    if(i==2) {
      100*btoutput$conf.int[i]
    } else {
      if (i==3) {
        if ((inputp > btoutput$conf.int[2]) || (inputp< btoutput$conf.int[1])) {
          "Significant"
        } else { 
          ""
        }
      }
    }
    
  }
}

getquarterfromdate <- function(inputDate) {
  output=""
  if (inputDate =="") {
    output=""
  } else {
    bitsofinputdate <- unlist(strsplit(as.character(inputDate), "-"))
    year<-as.numeric(bitsofinputdate[1])
    month<-as.numeric(bitsofinputdate[2])
    if (month<4) {
      quarter <-" Q1"
    } else {
      if (month<7) {
        quarter <-" Q2" 
      } else {
        if (month<10) {
          quarter <-" Q3" 
        } else {
          quarter <-" Q4" 
        }
      }
    }
    if (year < 2013) {
      output<-"-"
    }else {
      output<-paste0(year,quarter)
    }
  }
  output
}


addTotalAndPercentageColumnsToDF <- function(inputdf)
{
  outdf <- inputdf
  outdf[,c("Female","Male","Unknown")]<- sapply(outdf[,c("Female","Male","Unknown")], as.numeric)
  outdf$TotalForLevel<-rowSums(outdf[,c("Female","Male","Unknown")])
  outdf$TotalGenderDeduced<-rowSums(outdf[,c("Female","Male")])
  #bit of a hack to make it not fall over when dividing by 0
  outdf$TotalGenderDeduced[outdf$TotalGenderDeduced == 0] <- -1
  outdf$FemalePercentage<-100*outdf$Female / outdf$TotalGenderDeduced
  outdf$MalePercentage<-100*outdf$Male / outdf$TotalGenderDeduced
  #and unhack it
  outdf$TotalGenderDeduced[outdf$TotalGenderDeduced ==-1] <- 0
  outdf$TotalForLevel <-  round(outdf$TotalForLevel, 0)
  outdf$TotalGenderDeduced <-  round(outdf$TotalGenderDeduced, 0)
  outdf$FemalePercentage<-  round(outdf$FemalePercentage, 1)
  outdf$MalePercentage<-  round(outdf$MalePercentage, 1)
  outdf
}

calculateBinomialsDF<- function(inputdf, baseFemalePercentage, confidenceLevel=0.95) {
  source('./binomCalc.R')
  outdf <- inputdf
  outdf$LCI<-0
  outdf$UCI<-0
  outdf$LCICount<-0
  outdf$UCICount<-0
  outdf$Significance<-""
  outdf$Level  <- as.factor(outdf$Level)
  
  
  for(i in 1:nrow(inputdf))
  {
    if (outdf[i,"Female"]+outdf[i,"Male"] ==0) {
      outdf[i,"LCI"]=0
      outdf[i,"LCICount"]=0
      outdf[i,"UCI"]=0
      outdf[i,"UCICount"]=0
      outdf[i,"AdjustedPValue"]=0
      outdf[i,"Significance"]=""
    } else {
      rr_row<-calculateBinomialProportions(outdf[i,"Female"],outdf[i,"Male"],baseFemalePercentage/100,outdf[i,"TotalGenderDeduced"],0.95)
      outdf[i,"LCI"]=round(100*rr_row["LowerCI"],2)
      outdf[i,"LCICount"]=round(outdf[i,"Female"]*rr_row["LowerCI"]/rr_row["ActualProportion"],2)
      outdf[i,"UCI"]=round(100*rr_row["UpperCI"],2)
      outdf[i,"UCICount"]=round(outdf[i,"Female"]*rr_row["UpperCI"]/rr_row["ActualProportion"],2)
      outdf[i,"AdjustedPValue"]=rr_row["AdjustedPValue"]
      if (rr_row["AdjustedPValue"]<(1-confidenceLevel)) {
        outdf[i,"Significance"]="Significant"
      } else {
        outdf[i,"Significance"]=""
      }
    }
    #flag to say if pval is less than significance threshold- which is presumed to be the same as the confidence level
  }
  
  outdf
}

addCIandSigColumnsToDF<- function(inputdf, baseFemalePercentage)
{
  outdf <- inputdf
  outdf$Level  <- as.factor(outdf$Level)
  outdf$LCI <- mapply(bt, outdf$Female, outdf$TotalGenderDeduced, baseFemalePercentage/100, 1)
  outdf$UCI <- mapply(bt, outdf$Female, outdf$TotalGenderDeduced, baseFemalePercentage/100, 2)
  outdf$Significance <- mapply(bt, outdf$Female, outdf$TotalGenderDeduced, baseFemalePercentage/100, 3)
  outdf$LCI <-  round(outdf$LCI, 2)
  outdf$UCI <-  round(outdf$UCI, 2)
  outdf$LCI[outdf$Gender == "Male"] <- outdf$MalePercentage
  outdf$UCI[outdf$Gender == "Male"] <- outdf$MalePercentage
  outdf
}

maketotalplotdf <- function(dataDF) {
  totalplotdf<- melt(dataDF[,c("Level","TotalGenderDeduced","Male","Female","Unknown")], id=c("Level","TotalGenderDeduced"))
  names(totalplotdf)[names(totalplotdf) == 'Level'] <- 'XValues'
  names(totalplotdf)[names(totalplotdf) == 'TotalGenderDeduced'] <- 'TotalGenderDeduced'
  names(totalplotdf)[names(totalplotdf) == 'value'] <- 'YValues'
  names(totalplotdf)[names(totalplotdf) == 'variable'] <- 'Gender'
  totalplotdf$Gender <- factor(totalplotdf$Gender, levels = c("Unknown","Male", "Female"))
  totalplotdf
}

plotTotalGraph <- function(dataDF, baseFemalePercentage, xtitle, ytitle) {
  dataDF$Gender <- factor(dataDF$Gender, levels = c("Unknown","Male", "Female"))
  plot<- ggplot() +
    geom_bar(aes(x=XValues, y =YValues, fill = Gender), data=dataDF, stat = "identity") +scale_fill_manual(values=c("#DCDCDC", "#6699cc", "#ff6666"))
  plot <- plot +
    scale_x_discrete(limits = rev(levels(droplevels(dataDF$XValues)))) +
    theme(axis.title.x = element_text( size=15), 
          axis.text.x  = element_text(size=8), 
          axis.title.y = element_text( size=15), 
          axis.text.y  = element_text(size=8))
  plot <- plot +
    geom_text(data=dataDF, aes(x = XValues, y = TotalGenderDeduced, label = TotalGenderDeduced), size=3) +
    theme(legend.position="bottom", legend.direction="horizontal")
  plot <- plot + xlab(xtitle) + ylab(ytitle)
  plot
}


makepercentplotdf <- function(dataDF) {
  percentplotdf<- melt(dataDF[,c("Level","LCI", "UCI","Significance", "MalePercentage","FemalePercentage")], id=c("Level","LCI", "UCI", "Significance"))
  
  names(percentplotdf)[names(percentplotdf) == 'Level'] <- 'XValues'
  names(percentplotdf)[names(percentplotdf) == 'value'] <- 'YValues'
  names(percentplotdf)[names(percentplotdf) == 'variable'] <- 'Gender'
  percentplotdf[percentplotdf$Gender != "Female",c("Labels")] <- ""
  
  percentplotdf
}

plotPercentageGraph <- function(dataDF, baseFemalePercentage, xtitle, ytitle, baselinelabel) {
  
  dataDF$Gender <- gsub('Percentage', '', dataDF$Gender)
  dataDF$Gender <- factor(dataDF$Gender, levels = c("Male", "Female"))
  #dataDF <- ddply(dataDF, .(XValues),                     transform, pos = cumsum(YValues) - (0.5 * YValues))
  dataDF$pos <-dataDF$YValues
  dataDF$pos[dataDF$Gender == "Male"] <- 90
  dataDF$Labels<-paste(dataDF$YValues,"%")
  #percentplotdf$Labels<-d$A=="B"
  dataDF[which(dataDF$Gender == "Male"),c("Labels")] <- as.character(dataDF[which(dataDF$Gender == "Male"),c("Significance")])

  plot<- ggplot() +
    geom_bar(aes(x=XValues, y =YValues, fill = Gender), data=dataDF, stat = "identity")+scale_fill_manual(values=c("#6699cc", "#ff6666"))
  plot <- plot +
    scale_x_discrete(limits = rev(levels(droplevels(dataDF$XValues)))) +
    theme(axis.title.x = element_text( size=15), 
          axis.text.x  = element_text(size=8), 
          axis.title.y = element_text( size=15), 
          axis.text.y  = element_text(size=8))
  plot <- plot +
    geom_text(data=dataDF, aes(x = XValues, y = pos, label = Labels), size=3) +
    theme(legend.position="bottom", legend.direction="horizontal") +
    geom_line() + geom_hline(yintercept = baseFemalePercentage, colour="white")  +
    geom_text(aes(x =  sum(dataDF$Gender == "Female"), y = baseFemalePercentage+10, fontface=2, label =paste( baselinelabel,"\n",baseFemalePercentage,"%")), colour="white", size=3) +
    geom_errorbar(data=dataDF, aes(x=XValues, ymin=LCI, ymax=UCI), width=0.3)
  plot <- plot + xlab(xtitle) + ylab(ytitle)
  plot
}

displaytable <- function(inputdf,levelLabel="") {
  levels(inputdf$Level)<- sub("\n", "  ", levels(inputdf$Level))
  columnNames <-colnames(inputdf)
  #columnNames <-c("Level", "Male","Female","TotalGenderDeduced","Unknown","Population", "MalePercentage","FemalePercentage","LCI", "UCI","Significance")
  if (levelLabel !="") {
    names(inputdf)[names(inputdf) == "Level"] <- levelLabel
    originalColumnNames <- unlist(columnNames)
    columnNames<-relist(replace(originalColumnNames, originalColumnNames=="Level", levelLabel), skeleton=columnNames)
  }
  row.names(inputdf) <- NULL
  inputdf$AdjustedPValue<-format(inputdf$AdjustedPValue, digits = 3, scientific = 5)
  kable(inputdf[,columnNames])
  
}

concatcolumns<- function(OneRowPerManuscriptDF,columnstoconcat,columnstokeeplastoneseparate) {
  origcolumns <- colnames(OneRowPerManuscriptDF)
  
  for (thiscol in columnstoconcat) {

    if (paste0(thiscol,".x") %in% origcolumns) {
      names(OneRowPerManuscriptDF)[names(OneRowPerManuscriptDF)==paste0(thiscol,".x") ] <- thiscol
    }
    if (paste0(thiscol,".y") %in% origcolumns) {
      OneRowPerManuscriptDF[,thiscol] <- paste(OneRowPerManuscriptDF[,thiscol],OneRowPerManuscriptDF[,paste0(thiscol,".y")],sep = ";")
      OneRowPerManuscriptDF[,paste0(thiscol,".y")] <- NULL
    }
    

  }
  OneRowPerManuscriptDF
}

getoutcome <- function(Prescreened,Status){
  output=""
  if (Prescreened=="TRUE") {
    if (Status=="reject") {
      output<-paste("Prescreened",Status,sep = ": ")
    } else{
      output<-paste("No referee",Status,sep = ": ")
    }
  } else {
    output<-paste("Reviewed",Status,sep = ": ")
  }
  output  
}


getvaluefromdf<-function(filename,matchcolumnname,matchcolumnvalue,outcolumnname) {
  filenamedf <-read.csv(paste0("data/",filename,".csv"))
  baseFemalePercentage <-filenamedf[filenamedf[,c(matchcolumnname)]==matchcolumnvalue,c(outcolumnname)]
  baseFemalePercentage
}

getoverallbaseline<-function() {
  getvaluefromdf("s1_baselines","Level","All authors","FemalePercentage")
}


GetSignificanceFromPValue<-function(PValueForModel) {
  SignificanceForModel<-""
  if (PValueForModel<0.001){
    SignificanceForModel<-"*** Significant"
  } else {
    if (PValueForModel<0.01){
      SignificanceForModel<-"** Significant"
    } else {
      if (PValueForModel<0.05){
        SignificanceForModel<-"* Significant"
      } else {
        SignificanceForModel<-"Not significant"
      }
    }
  } 
  SignificanceForModel
}


addPopulationSizeToDataFrame<-function(effectTableDF,modelToUse,Variable1Name,Variable2Name) {
  modelframe<-model.frame(modelToUse)
  if (Variable2Name=="") {
    populationsizesdf<-data.frame(table(modelframe[,c(Variable1Name)]))
  } else {
    populationsizesdf<-data.frame(table(modelframe[,c(Variable1Name,Variable2Name)]))
  }
  colnames(populationsizesdf)[colnames(populationsizesdf) == 'Freq'] <- 'PopulationSize'
  if("Revision" %in% colnames(populationsizesdf))
  {
    populationsizesdf$Revision<-as.character(populationsizesdf$Revision)
  }
  if("Revision" %in% names(effectTableDF))
  {
    effectTableDF$Revision<-as.character(effectTableDF$Revision)
  }
  if (Variable2Name=="") {
    colnames(populationsizesdf)[colnames(populationsizesdf) == 'Var1'] <- Variable1Name
    effectTableDF <- merge(x=effectTableDF,y=populationsizesdf,by=Variable1Name,all.x = TRUE)
  } else {
    effectTableDF <- merge(x=effectTableDF,y=populationsizesdf,by=c(Variable1Name,Variable2Name),all.x = TRUE)
  }
  if("Revision" %in% names(effectTableDF))
  {
    effectTableDF$Revision<-as.factor(effectTableDF$Revision)
  }
  effectTableDF
}

addPValuesAndSignificanceOneVariable<-function(inputDF,PValues) {
  outputDF <- inputDF
  outputDF$PValue <-PValues
  outputDF$Significant<-""
  outputDF[which(outputDF$PValue<0.05),"Significant"] <-"* Significant"
  outputDF[which(outputDF$PValue<0.01),"Significant"] <-"** Significant"
  outputDF[which(outputDF$PValue<0.001),"Significant"] <-"*** Significant"
  outputDF
}

addVariableOneModelResultsToEffectDataFrame <-function(effectTable,effectTable2,Variable1Name,columnname) {
  resultsDF<- effectTable
  colnames(resultsDF)[colnames(resultsDF) == 'fit'] <- columnname
  #colnames(resultsDF)[colnames(resultsDF) == 'upper'] <- 'ref_upper'
  #colnames(resultsDF)[colnames(resultsDF) == 'lower'] <- 'ref_lower'
  resultsDF$se <- NULL
  resultsDF$upper <- NULL
  resultsDF$lower <- NULL
  resultsDF <- merge(x=effectTable2,y=resultsDF,by=Variable1Name,all.x = TRUE)
  resultsDF
}

addPValuesAndSignificanceTwoVariables<-function(resultsDF,PvaluesVector,Variable1Name,Variable2Name,Variable1FirstLevel,Variable2FirstLevel) {
  PvaluesDF<-data.frame(name=names(PvaluesVector),PValue=PvaluesVector)
  row.names(PvaluesDF) <- NULL
  PvaluesDF$name <- as.character(PvaluesDF$name)
  PvaluesDF[which(PvaluesDF$name=="(Intercept)"),"name"]<-paste0(Variable1Name,Variable1FirstLevel,":",Variable2Name,Variable2FirstLevel)
  PvaluesDF[-grep(Variable1Name, PvaluesDF$name),"name"]<-paste0(Variable1Name,Variable1FirstLevel,":",PvaluesDF[-grep(Variable1Name, PvaluesDF$name),"name"])
  PvaluesDF[-grep(Variable2Name, PvaluesDF$name),"name"]<-paste0(PvaluesDF[-grep(Variable2Name, PvaluesDF$name),"name"],":",Variable2Name,Variable2FirstLevel)
  
  resultsDF$name<-paste0(Variable1Name,resultsDF[[Variable1Name]])
  resultsDF$name<-paste0(resultsDF$name,":",Variable2Name,resultsDF[[Variable2Name]])
  
  resultsDF <- merge(x=resultsDF,y=PvaluesDF,by="name",all.x = TRUE)
  resultsDF$Significant<-""
  resultsDF[which(resultsDF$PValue<0.05),"Significant"] <-"* Significant"
  resultsDF[which(resultsDF$PValue<0.01),"Significant"] <-"** Significant"
  resultsDF[which(resultsDF$PValue<0.001),"Significant"] <-"*** Significant"
  resultsDF$name<-NULL
  resultsDF
  }