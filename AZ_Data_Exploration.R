setwd("/Users/dennyba/workspace/Courses/2023Fall_MATH571/STAT_571A_Project")

houses = read.csv("recs2020_public_v5.csv")

#Filter data for homes in AZ
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
plot(daz2$SQFTEST, sqrt(daz2$KWH), xlab="Home Size (square feet)", ylab="Sqrt Energy consumed")
abline(houses_fit_sqrt_kwh)

plot(daz2$SQFTEST, houses_fit_sqrt_kwh$residuals, 
     xlab="Home Size (square feet)", ylab="Residuals")
abline(h=0)

houses_fit_size_ac = lm(sqrt(KWH) ~ SQFTEST+ ACEQUIPM_PUB, data = daz2)
ac_colors = unclass(as.factor(daz2$ACEQUIPM_PUB))
ac_vector = attr(ac_colors,"levels")

plot(daz2$SQFTEST, daz2$KWH, col=ac_colors,pch=16, 
     xlab="Home Size (square feet)", ylab="Energy consumed (kWH)")
legend("topleft", ac_vector, col=1:5, pch=16)

sizes = seq(min(daz2$SQFTEST), max(daz2$SQFTEST), length.out=100)
for(j in 1:length(ac_vector)){
  ac_str = ac_vector[j]
  predictor_df = data.frame( "ACEQUIPM_PUB"= ac_str,
                            "SQFTEST" = sizes
  )
  predicted_kwh = (predict(houses_fit_size_ac, predictor_df))^2
  points(sizes, predicted_kwh, col=j, type='l', lwd=2)
}
