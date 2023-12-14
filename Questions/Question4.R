## The exploration code is almost based on 'AZ_Data_Exploration'

houses = read.csv("recs2020_public_v5.csv")

isArizona = houses[,6]=="Arizona"
daz = houses[isArizona,]
daz2 = daz
acTypeCol = names(daz)=="ACEQUIPM_PUB"
daz2[which(daz$ACEQUIPM_PUB<=0),acTypeCol] ="DoNotKnow"
daz2[which(daz$ACEQUIPM_PUB==1),acTypeCol] ="CentralAC"
daz2[which(daz$ACEQUIPM_PUB==2),acTypeCol] ="CentralHeatPump"
daz2[which(daz$ACEQUIPM_PUB==3),acTypeCol] ="MiniSplit"
daz2[which(daz$ACEQUIPM_PUB==4),acTypeCol] ="Window"
daz2[which(daz$ACEQUIPM_PUB==5),acTypeCol] ="Portable"
daz2[which(daz$ACEQUIPM_PUB==6),acTypeCol] ="SwampCooler"

require(MASS)
houses_fit_all = lm(KWH ~ SQFTEST + YEARMADERANGE + ACEQUIPM_PUB, data = daz2)
houses_fit_size = lm(KWH ~ SQFTEST, data = daz2)
plot(daz2$SQFTEST, daz2$KWH, xlab="Home Size (square feet)", ylab="Energy consumed (kWH)")
abline(houses_fit_size)

AZ_size <- summary(houses_fit_size)

plot(daz2$SQFTEST, houses_fit_size$residuals, 
     xlab="Home Size (square feet)", ylab="Residuals")
abline(h=0)

reallyBigHouseIndex = which(daz2$SQFTEST > 6000)
daz2 = daz2[-reallyBigHouseIndex,]

plot(daz2$SQFTEST, daz2$KWH)
abline(houses_fit_size)

bc = boxcox(houses_fit_size)
lambdaMax = bc$x[which.max(bc$y)]
#lambdaMax = .42, so let's use .5
houses_fit_sqrt_kwh = lm(sqrt(KWH) ~ SQFTEST, data = daz2)

AZ_size_adjust <- summary(houses_fit_sqrt_kwh)

plot(daz2$SQFTEST, sqrt(daz2$KWH), xlab="Home Size (square feet)", ylab="Sqrt Energy consumed")
abline(houses_fit_sqrt_kwh)

plot(daz2$SQFTEST, houses_fit_sqrt_kwh$residuals, 
     xlab="Home Size (square feet)", ylab="Residuals")
abline(h=0)






CA_data = houses[,6]=="California"
another_state = houses[CA_data,]
another_state2 = another_state
acTypeCol = names(another_state)=="ACEQUIPM_PUB"
another_state2[which(another_state$ACEQUIPM_PUB<=0),acTypeCol] ="DoNotKnow"
another_state2[which(another_state$ACEQUIPM_PUB==1),acTypeCol] ="CentralAC"
another_state2[which(another_state$ACEQUIPM_PUB==2),acTypeCol] ="CentralHeatPump"
another_state2[which(another_state$ACEQUIPM_PUB==3),acTypeCol] ="MiniSplit"
another_state2[which(another_state$ACEQUIPM_PUB==4),acTypeCol] ="Window"
another_state2[which(another_state$ACEQUIPM_PUB==5),acTypeCol] ="Portable"
another_state2[which(another_state$ACEQUIPM_PUB==6),acTypeCol] ="SwampCooler"

require(MASS)
houses_fit_size_CA = lm(KWH ~ SQFTEST + YEARMADERANGE + ACEQUIPM_PUB, data = another_state2)
houses_fit_size_CA = lm(KWH ~ SQFTEST, data = another_state2)
plot(another_state2$SQFTEST, another_state2$KWH, xlab="Home Size (square feet)", ylab="Energy consumed (kWH)")
abline(houses_fit_size_CA)

CA_size <- summary(houses_fit_size_CA)

plot(another_state2$SQFTEST, houses_fit_size_CA$residuals, 
     xlab="Home Size (square feet)", ylab="Residuals")
abline(h=0)

reallyBigHouseIndex = which(another_state2$SQFTEST > 6000)
another_state2 = another_state2[-reallyBigHouseIndex,]

plot(another_state2$SQFTEST, another_state2$KWH)
abline(houses_fit_size_CA)

bc = boxcox(houses_fit_size_CA)
lambdaMax_CA = bc$x[which.max(bc$y)]
houses_fit_size_adjust_CA = lm((KWH^lambdaMax_CA) ~ SQFTEST, data = another_state2)
plot(another_state2$SQFTEST, ((another_state2$KWH)^lambdaMax_CA), xlab="Home Size (square feet)", ylab="Sqrt Energy consumed")
abline(houses_fit_size_adjust_CA)

CA_size_adjust <- summary(houses_fit_size_adjust_CA)

plot(another_state2$SQFTEST, houses_fit_sqrt_kwh$residuals, 
     xlab="Home Size (third root feet)", ylab="Residuals")
abline(h=0)









NY_data = houses[,6]=="New York"
another_state = houses[NY_data,]
another_state2 = another_state
acTypeCol = names(another_state)=="ACEQUIPM_PUB"
another_state2[which(another_state$ACEQUIPM_PUB<=0),acTypeCol] ="DoNotKnow"
another_state2[which(another_state$ACEQUIPM_PUB==1),acTypeCol] ="CentralAC"
another_state2[which(another_state$ACEQUIPM_PUB==2),acTypeCol] ="CentralHeatPump"
another_state2[which(another_state$ACEQUIPM_PUB==3),acTypeCol] ="MiniSplit"
another_state2[which(another_state$ACEQUIPM_PUB==4),acTypeCol] ="Window"
another_state2[which(another_state$ACEQUIPM_PUB==5),acTypeCol] ="Portable"
another_state2[which(another_state$ACEQUIPM_PUB==6),acTypeCol] ="SwampCooler"

require(MASS)
houses_fit_size_NY = lm(KWH ~ SQFTEST + YEARMADERANGE + ACEQUIPM_PUB, data = another_state2)
houses_fit_size_NY = lm(KWH ~ SQFTEST, data = another_state2)
plot(another_state2$SQFTEST, another_state2$KWH, xlab="Home Size (square feet)", ylab="Energy consumed (kWH)")
abline(houses_fit_size_NY)

NY_size <- summary(houses_fit_size_NY)

plot(another_state2$SQFTEST, houses_fit_size_NY$residuals, 
     xlab="Home Size (square feet)", ylab="Residuals")
abline(h=0)

reallyBigHouseIndex = which(another_state2$SQFTEST > 6000)
another_state2 = another_state2[-reallyBigHouseIndex,]

plot(another_state2$SQFTEST, another_state2$KWH)
abline(houses_fit_size_NY)

bc = boxcox(houses_fit_size_NY)
lambdaMax_NY = bc$x[which.max(bc$y)]
houses_fit_size_adjust_NY = lm((KWH^lambdaMax_NY) ~ SQFTEST, data = another_state2)
plot(another_state2$SQFTEST, ((another_state2$KWH)^lambdaMax_NY), xlab="Home Size (square feet)", ylab="Sqrt Energy consumed")
abline(houses_fit_size_adjust_NY)

NY_size_adjust <- summary(houses_fit_size_adjust_NY)

plot(another_state2$SQFTEST, houses_fit_sqrt_kwh$residuals, 
     xlab="Home Size (third root feet)", ylab="Residuals")
abline(h=0)






AZ_size
AZ_size_adjust
CA_size
CA_size_adjust
NY_size
NY_size_adjust

## We can get some information from the comparison. For example, the slope for AZ is 6.08, for CA is 2.58, for NY is 3.32. Roughly speaking, extreme temperature would increase the relationship between energy consumed and home size, and heat is more influencial and cold in this aspect.
