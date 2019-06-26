#script to assign gender by name using the combined US/UK censor data

library(data.table)
library(dplyr)
assignGenderToVector<-function(inVector)
{
  fread("data/genderNames.csv",header=TRUE,sep=",")->nameGenderMappingTable

  femaleNames<-nameGenderMappingTable[UKUS_Gender=="Female"]$Name
  maleNames<-nameGenderMappingTable[UKUS_Gender=="Male"]$Name


  inVector<-tolower(inVector)
  rr<-data.table(inVector)
  colnames(rr)<-c("FNAME")
  rr$GENDER<-"U"
  rr[FNAME %in% femaleNames,GENDER:="F"]
  rr[FNAME %in% maleNames,GENDER:="M"]
  sapply(rr$FNAME,strsplit,fixed=TRUE,split=" ")->splitNames
  rr$FNAME<-sapply(splitNames,function(x) x[1])

  rr[FNAME %in% femaleNames & GENDER=="U",GENDER:="F"]
  rr[FNAME %in% maleNames& GENDER=="U",GENDER:="M"]
  
  rr
}
