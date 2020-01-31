import pandas as pd
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("input")
parser.add_argument("output")
parser.add_argument("field")
args = parser.parse_args()
print(args.input)
print(args.output)
print(args.field)

#Example Python code For assigning Gender From First Names

def assignGenderToVector(inputDF,firstnamecolumnname):
	nameGenderMappingDf = pd.read_csv("data/genderNames.csv")
	femaleNames=nameGenderMappingDf.loc[(nameGenderMappingDf["UKUS_Gender"] == "Female"),"Name"]
	maleNames=nameGenderMappingDf.loc[(nameGenderMappingDf["UKUS_Gender"] == "Male"),"Name"]
	
	rr=inputDF
	rr["CHECKFNAME"]=rr[firstnamecolumnname].str.lower()
	rr["GENDER"]="U"

	rr['AreTheyFemale'] = rr.CHECKFNAME.isin(femaleNames).astype(int)
	rr.loc[rr["AreTheyFemale"] == 1,"GENDER"]="F"
	rr=rr.drop("AreTheyFemale", axis=1)
	rr['AreTheyMale'] = rr.CHECKFNAME.isin(maleNames).astype(int)
	rr.loc[rr["AreTheyMale"] == 1,"GENDER"]="M"
	rr=rr.drop("AreTheyMale", axis=1)
	rr=rr.drop("CHECKFNAME", axis=1)
	return rr

inputWithNamedf=pd.read_csv(args.input)
rr = assignGenderToVector(inputWithNamedf,args.field)
rr.to_csv(args.output)
