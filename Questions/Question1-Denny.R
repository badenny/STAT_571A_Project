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

predColInd = names(houses)=="COOLCNTL"
predCol = houses[,predColInd]
houses[which(predCol==1), predColInd] ="OneTemp"
houses[which(predCol==2), predColInd] ="ManualAdjust"
houses[which(predCol==3), predColInd] ="ProgramOrSmart"
houses[which(predCol==4), predColInd] ="OnWhenNeeded"
houses[which(predCol==5), predColInd] ="NoControl"
houses[which(predCol==99), predColInd] ="Other"
houses[which(predCol==-2), predColInd] ="NoAC"

predColInd = names(houses)=="FUELHEAT"
predCol = houses[,predColInd]
houses[which(predCol==1), predColInd] ="NaturalGas"
houses[which(predCol==2), predColInd] ="Propane"
houses[which(predCol==3), predColInd] ="FuelOil"
houses[which(predCol==5), predColInd] ="Electricity"
houses[which(predCol==7), predColInd] ="Wood"
houses[which(predCol==99), predColInd] ="Other"
houses[which(predCol==-2), predColInd] ="NoHeat"


#desert_houses = houses[houses$BA_climate=='Hot-Dry',]
#desert_houses = houses[houses$state_name=="New Mexico",]
desert_houses = houses[houses$state_name=="Arizona",]

sqft_fit = lm(KWH ~ SQFTEST, data=desert_houses)
#remove very large homes from analysis
desert_houses = desert_houses[desert_houses$SQFTEST<6000, ]
desert_houses = desert_houses[desert_houses$TYPETHERM!="DoNotKnow",]
desert_houses = desert_houses[desert_houses$ACEQUIPM_PUB!="DoNotKnow",]

#desert_houses$FUELHEAT = as.factor(desert_houses$FUELHEAT)



sqft_fit = lm(KWH ~ SQFTEST, data=desert_houses)
require(MASS) #needed for Box-Cox transformation
desert_box_cox = boxcox(sqft_fit)
bc_lambda_max = desert_box_cox$x[which.max(desert_box_cox$y)]
sqft_fit_quartic = lm(KWH^(1/2) ~ SQFTEST, data=desert_houses)

size_ac_fit = lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + FUELHEAT, data=desert_houses)
summary(size_ac_fit)


#now include type of thermostat, type of dwelling, and quality of insulation
house_fit = lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPETHERM + TYPEHUQ +
                 ADQINSUL + FUELHEAT, data=desert_houses)
house_fit_interaction = lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPETHERM +
                             TYPEHUQ + ADQINSUL + FUELHEAT + SQFTEST:TYPEHUQ + 
                             SQFTEST:ACEQUIPM_PUB + SQFTEST:TYPETHERM +
                             SQFTEST:ADQINSUL + SQFTEST:FUELHEAT,
                           data=desert_houses)
summary(house_fit_interaction)

anova(house_fit, house_fit_interaction)
#anova shows that we should drop all interaction coefficients with F p-value .8

#Residual plot
plot(house_fit$residuals, col='blue', main="Residual Plot with KWH^(1/2)",
     xlab="Index", ylab="Residual", cex.lab=1.5,
     cex.axis=1.5, cex.main=1.5, pch=1)
abline(h=0,col='black',lwd=2.5)

house_fit_no_therm_insul = lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPEHUQ +
                          FUELHEAT, data=desert_houses)
house_fit_no_therm = lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPEHUQ + ADQINSUL +
                                FUELHEAT, data=desert_houses)
#anova F-test indicates that we should drop the thermometer coefficient
anova(house_fit_no_therm_insul, house_fit)
anova(house_fit_no_therm, house_fit_interaction)

#This is the model to use
house_fit= lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPEHUQ + ADQINSUL +
                          FUELHEAT, data=desert_houses)

pred_WH_MLR <- function(object, newdata, level = 0.95){
  n <- nrow(object$model)
  p <- object$rank
  fit <- predict(object, newdata, interval = "confidence")
  ME_confidence <- fit[, 'fit'] - fit[, 'lwr']
  alpha <- 1 - level
  t_alpha <- qt(1 - alpha / 2, df = n - p)
  W_alpha <- sqrt(p * qf(level, p, n - p))
  ME <- ME_confidence * W_alpha / t_alpha
  upr <- fit[, 'fit'] + ME
  lwr <- fit[, 'fit'] - ME
  return(cbind(fit = fit[, 'fit'], lwr, upr))
}



#sf_range = seq(min(desert_houses$SQFTEST), max(desert_houses$SQFTEST, length.out=100))
huq_colors = unclass(as.factor(desert_houses$TYPEHUQ))
huq_vector = attr(huq_colors,"levels")

#plot white at first so that the points are not covered
plot(desert_houses$SQFTEST, desert_houses$KWH, col="white", main="Comparison of Energy Usage by House Type",
     xlab="House Size (SQ FT)", ylab="Annual Electricity Usage (KWH)", cex.lab=1.5,
     cex.axis=1.5, cex.main=1.5,xlim=c(100,5000), ylim=c(100,60000))
pch_offset = 13
points(desert_houses$SQFTEST, desert_houses$KWH, col=huq_colors,pch=huq_colors+pch_offset, cex=1.)
for(j in 1:length(huq_vector)){
  huq_str = huq_vector[j]
  sf_which_huq = desert_houses$SQFTEST[which(desert_houses$TYPEHUQ==huq_str)]
  sf_range = seq(min(sf_which_huq), max(sf_which_huq), length.out=100)
  predictor_df = data.frame(SQFTEST=sf_range, ACEQUIPM_PUB="CentralAC",
                            TYPEHUQ=huq_str,
                            FUELHEAT="Electricity",
                            ADQINSUL=mean(desert_houses$ADQINSUL))
  predicted_dens = (pred_WH_MLR(house_fit, predictor_df))^2
  huq_fit = predicted_dens[,1]
  huq_lwr = predicted_dens[,2]
  huq_upr = predicted_dens[,3]
  points(sf_range, huq_fit, col=j, type='l', lwd=4)
  #polygon(c(depths,rev(depths), depths[1]), c(huq_lwr, rev(huq_upr), huq_lwr[1]),
  #        density = 15, col=j, angle = 45/j, lty=1, lwd=.5)
}
legend("topleft", legend=attr(huq_colors,"levels"), col=1:5, pch=1:5+pch_offset,cex=1.1)

house_fit = lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPETHERM + TYPEHUQ +
                 ADQINSUL + FUELHEAT + COOLCNTL, data=desert_houses)

house_fit_interaction = lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPETHERM +
                             TYPEHUQ + ADQINSUL + FUELHEAT + COOLCNTL +
                             SQFTEST:COOLCNTL +
                             SQFTEST:TYPEHUQ + SQFTEST:ACEQUIPM_PUB +
                             SQFTEST:TYPETHERM + SQFTEST:ADQINSUL + 
                             SQFTEST:FUELHEAT + SQFTEST:COOLCNTL,
                             data=desert_houses)
house_fit= lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPEHUQ +
                ADQINSUL + FUELHEAT + COOLCNTL, data=desert_houses)
anova(house_fit, house_fit_interaction)

house_fit_no_therm = lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPEHUQ +
                          ADQINSUL + FUELHEAT + COOLCNTL, data=desert_houses)

house_fit= lm(KWH^(1/2) ~ SQFTEST + ACEQUIPM_PUB + TYPEHUQ +
                          ADQINSUL + FUELHEAT + COOLCNTL, data=desert_houses)


predStr = "COOLCNTL"
predColInd = names(houses)==predStr
predCol = desert_houses[,predColInd]
huq_colors = unclass(as.factor(predCol))
huq_vector = attr(huq_colors,"levels")
#plot white at first so that the points are not covered
plot(desert_houses$SQFTEST, desert_houses$KWH, col="white", main="Energy Use by Thermostat Behavior",
     xlab="House Size (SQ FT)", ylab="Annual Electricity Usage (KWH)", cex.lab=1.5,
     cex.axis=1.5, cex.main=1.5,xlim=c(100,5000), ylim=c(100,60000))
pch_offset = 13
points(desert_houses$SQFTEST, desert_houses$KWH, col=huq_colors,pch=huq_colors+pch_offset, cex=1.)
for(j in 1:length(huq_vector)){
  huq_str = huq_vector[j]
  print(huq_str)
  sf_which_huq = desert_houses$SQFTEST[which(predCol==huq_str)]
  sf_range = seq(min(sf_which_huq), max(sf_which_huq), length.out=100)
  predictor_df = data.frame(SQFTEST=sf_range, ACEQUIPM_PUB="CentralAC",
                            TYPEHUQ="SingleFamilyDetached",
                            FUELHEAT="Electricity",
                            COOLCNTL=huq_str,
                            ADQINSUL=mean(desert_houses$ADQINSUL))
  predicted_dens = (pred_WH_MLR(house_fit, predictor_df))^2
  huq_fit = predicted_dens[,1]
  huq_lwr = predicted_dens[,2]
  huq_upr = predicted_dens[,3]
  points(sf_range, huq_fit, col=j, type='l', lwd=4)
  #polygon(c(depths,rev(depths), depths[1]), c(huq_lwr, rev(huq_upr), huq_lwr[1]),
  #        density = 15, col=j, angle = 45/j, lty=1, lwd=.5)
}
legend("topleft", legend=attr(huq_colors,"levels"), col=1:5, pch=1:5+pch_offset,cex=1.1)

#make function that produces those prediction plotss
 
#make plot to show difference of 
#require(glmnet)
#cv_house_lasso = cv.glmnet(model.matrix(house_fit)[,-1], desert_houses$KWH^.25, alpha=0)
#house_lasso = glmnet(model.matrix(house_fit)[,-1], desert_houses$KWH^.25, alpha=1, lambda=.085)
#coefficients(house_lasso)

