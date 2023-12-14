#Put the location of your local git repository here
setwd("/Users/dennyba/workspace/Courses/2023Fall_MATH571/STAT_571A_Project")

houses = read.csv("recs2020_public_v5.csv")

#Switch coded numerically coded AC Type to strings
acTypeCol = names(houses)=="ACEQUIPM_PUB"
houses[which(houses$ACEQUIPM_PUB<=0),acTypeCol] ="DoNotKnow"
houses[which(houses$ACEQUIPM_PUB==1),acTypeCol] ="CentralAC"
houses[which(houses$ACEQUIPM_PUB==2),acTypeCol] ="CentralHeatPump"
houses[which(houses$ACEQUIPM_PUB==3),acTypeCol] ="MiniSplit"
houses[which(houses$ACEQUIPM_PUB==4),acTypeCol] ="Window"
houses[which(houses$ACEQUIPM_PUB==5),acTypeCol] ="Portable"
houses[which(houses$ACEQUIPM_PUB==6),acTypeCol] ="SwampCooler"

#Code thermostat type as string
thermTypeCol = names(houses)=="TYPETHERM"
houses[which(houses$TYPETHERM==-2), thermTypeCol] ="DoNotKnow"
houses[which(houses$TYPETHERM==0), thermTypeCol] ="None"
houses[which(houses$TYPETHERM==1), thermTypeCol] ="Manual"
houses[which(houses$TYPETHERM==2), thermTypeCol] ="Programmable"
houses[which(houses$TYPETHERM==3), thermTypeCol] ="Smart"

#Code type of home as string
homeTypeCol = names(houses)=="TYPEHUQ"
houses[which(houses$TYPEHUQ==1), homeTypeCol] ="Mobile"
houses[which(houses$TYPEHUQ==2), homeTypeCol] ="SingleFamilyDetached"
houses[which(houses$TYPEHUQ==3), homeTypeCol] ="SingleFamilyAttached"
houses[which(houses$TYPEHUQ==4), homeTypeCol] ="Multiplex"
houses[which(houses$TYPEHUQ==5), homeTypeCol] ="LargeCommericalApts"
