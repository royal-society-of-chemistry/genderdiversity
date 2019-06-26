library(rjson)
library(data.table)
library(ggplot2)
library(dplyr)
json_data <- fromJSON(file="data/diversityPapers.json")

sapply(json_data, '[[', 6)->yearsList
sapply(json_data, '[[', "usukGenderProfile")->authorGenderList
sapply(json_data, '[[', "articleType")->articleTypeList


yearsList>2015 ->rowsWithLaterYears
articleTypeList=="Paper"->rowsWithPapers
noAuthorsPerPaper<-unlist(lapply(authorGenderList,length))
#json_data[rowsWithLaterYears&rowsWithPapers]->modernPapersList
authorGenderList[rowsWithLaterYears&rowsWithPapers]->paperAuthorGenderList

matrix(0,10,10)->authorPositionMatrixMale
matrix(0,10,10)->authorPositionMatrixFemale
for(i in 1:length(paperAuthorGenderList))
{
  authorGenders<-paperAuthorGenderList[[i]]
  rowNumber<-length(authorGenders)
  if(rowNumber<11)
  {for(j in 1:length(authorGenders))
  {
    
    if(authorGenders[j]=="M")
      authorPositionMatrixMale[rowNumber,j]<-authorPositionMatrixMale[rowNumber,j]+1
    if(authorGenders[j]=="F")
      authorPositionMatrixFemale[rowNumber,j]<-authorPositionMatrixFemale[rowNumber,j]+1
    
  }
  }
}

backgroundProbs<-sum(authorPositionMatrixFemale)/(sum(authorPositionMatrixMale)+sum(authorPositionMatrixFemale))


matrix(0,10,10)->scoreMatrix
for(i in 1:(nrow(authorPositionMatrixMale)))
{
  
  {for(j in 1:i)
  {
    
 
    mcount<-  authorPositionMatrixMale[i,j]
    fcount<-authorPositionMatrixFemale[i,j]
    binom.result<-binom.test(fcount,fcount+mcount,backgroundProbs)
    
    scoreMatrix[i,j]<- -log10(binom.result$p.value)
    if(binom.result$estimate>binom.result$null.value) scoreMatrix[i,j]<- -scoreMatrix[i,j]
  }
  }
}


melt(scoreMatrix)->plotableTable
colnames(plotableTable)<-c("NumberAuthors","AuthorPosition","Score")

# we want cut off at 0.05 (we don't adjust for number of tests just because dividing by 55 is a bit harsh)

cuttoffScore<- -log10(0.05)
plotableTable %>% mutate(Score=ifelse (Score>0 & Score<cuttoffScore,0,Score))->plotableTable
plotableTable %>% mutate(Score=ifelse (Score<0 & Score> -cuttoffScore,0,Score))->plotableTable

#we also want to floor and ceiling reuslts for display -use 1e10...
plotableTable %>% mutate(Score=ifelse (Score>5,5,Score))->plotableTable
plotableTable %>% mutate(Score=ifelse (Score< -5,-5,Score))->plotableTable

plotableTable$AuthorPosition<-factor(plotableTable$AuthorPosition,levels=1:10)
plotableTable$NumberAuthors<-factor(plotableTable$NumberAuthors,levels=10:1)
ggplot(plotableTable,aes(x=AuthorPosition,y=NumberAuthors,fill=Score))+geom_tile()+scale_fill_gradient2(low="#ff6666",mid="white",high="#6699cc")


