setwd(paste0(getwd(),"/STAT_571A_Project")) ## Set a proper directory

## Author: Sungmin Ji    ## Date; 11-28-2023

## Packages

if("tidyverse" %in% rownames(installed.packages()) == FALSE)
{install.packages("tidyverse")
}
library(tidyverse)

if("glmnet" %in% rownames(installed.packages()) == FALSE)
{install.packages("glmnet")
}
library(glmnet)


if("car" %in% rownames(installed.packages()) == FALSE)
{install.packages("car")
}
library(car)

houses = read.csv("recs2020_public_v5.csv")

#Filter data for homes in AZ
isArizona = houses[,6]=="Arizona"
daz = houses[isArizona,]
daz2 = daz

annot <- list()
annot[["ACEQUIPM_PUB"]] = c("DoNotKnow", "CentralAC", "CentralHeatPump", "MiniSplit", "Window", "Portable", "SwampCooler")
annot[["TYPEHUQ"]] = c("Moblie home", "Single-family house detached from any other house",
                       "Single-family house attached to one or more other houses",
                       "Apartment in a building with 2 to 4 units",
                       "Apartment in a building with 5 or more units")

annot[["ADQINSUL"]] = c("Well insulated", "dequately insulated", "Poorly insulated", "Not insulated")
annot[["KWHPLPMP"]] = c("No", "Yes")
annot[["FUELHEAT"]] = c("Electricity", "Natural gas from underground pipes", "Propane", "Wood or pellets", "Others") #include "Fuel oil" in "Others"

cat_columns <- c("ACEQUIPM_PUB", "TYPEHUQ", "ADQINSUL", "KWHPLPMP", "FUELHEAT")

select_col <- cat_columns[1] # ACEQUIPM_PUB

for(i in 1:length(annot[[select_col]])){
  if(i==1){
    daz2[[select_col]][which(daz[[select_col]]<=(i-1))] = i
  }else{
    daz2[[select_col]][which(daz[[select_col]]==(i-1))] = i
  }
}

select_col <- cat_columns[2] # TYPEHUQ

for(i in 1:length(annot[[select_col]])){
    daz2[[select_col]][which(daz[[select_col]]==i)] = i
}

select_col <- cat_columns[3] # ADQINSUL

for(i in 1:length(annot[[select_col]])){
  daz2[[select_col]][which(daz[[select_col]]==i)] = i
}

select_col <- cat_columns[4]  # KWHPLPMP
daz2[[select_col]][which(daz[[select_col]]==0)] = "1"
daz2[[select_col]][which(daz[[select_col]]>0)] = "2"


for(i in 1:length(annot[[select_col]])){
  daz2[[select_col]][which(daz[[select_col]]==i)] = i
}

select_col <- cat_columns[5]  # FUELHEAT

daz2[[select_col]][which(daz[[select_col]]==5)] = "1"
daz2[[select_col]][which(daz[[select_col]]==1)] = "2"
daz2[[select_col]][which(daz[[select_col]]==2)] = "3"
daz2[[select_col]][which(daz[[select_col]]==3)] = "5"
daz2[[select_col]][which(daz[[select_col]]==7)] = "4"
daz2[[select_col]][which(daz[[select_col]]<=0|daz[[select_col]]==99)] = "5"


df_cat <- daz2 %>% select(ACEQUIPM_PUB, TYPEHUQ, ADQINSUL, KWHPLPMP, FUELHEAT)

df_cat2 <- sapply(names(annot), FUN=function(x) factor(df_cat[[x]], levels=1:length(annot[[x]]), labels=paste0(".",annot[[x]])),  simplify = F) %>% as.data.frame()

selected_df <- daz2 %>% select(KWH, SQFTEST, LGTINLED, LGTINCFL, LGTINCAN, LGTIN1TO4, LGTIN4TO8, LGTINMORE8) %>% cbind(df_cat2)
str(selected_df)

colnames(selected_df)
lmfit1 <- lm(KWH ~., data=selected_df)

par(mfrow=c(2,2))
plot(lmfit1)
summary(lmfit1)


lmfit2 <- lm(sqrt(KWH) ~., data=selected_df)
plot(lmfit2)
summary(lmfit2)

vif(lmfit2) ## No obvious evidence of multicollinearity


#### Variable Selection
n <- nrow(selected_df) ## sample size=495
p <- n - lmfit2$df.residual ## p=20

## 1. Backward Elimination

step(lmfit2, direction="backward", k=log(n)) ## Use BIC formula for AIC

## 2. Stepwise selection
step(lmfit2, direction="both", k=log(n)) 

## Both methods give consistent results for "sqrt(KWH) ~ SQFTEST + LGTIN4TO8 + ACEQUIPM_PUB + KWHPLPMP + FUELHEAT
lmfit3 <- lm(sqrt(KWH) ~ SQFTEST + LGTIN4TO8 + KWHPLPMP + FUELHEAT, data=selected_df)

## 3. LASSO

X <- model.matrix(sqrt(KWH)~., data=selected_df)[,-1]
Y <- sqrt(selected_df$KWH)

### LOOCV
set.seed(1000)
lasso_cvfit <- cv.glmnet(x=X, y=Y, alpha=1, nfolds = n)



## Minimum CV rule ##
lambda <- lasso_cvfit$lambda.min; cvm <- min(lasso_cvfit$cvm)
lambda;cvm

bestlasso_fit <- glmnet(x=X, y=Y, alpha=1, lambda=lambda)

coef.glmnet(bestlasso_fit)
selected_pred1 <- rownames(coef.glmnet(bestlasso_fit))[as.vector(coef.glmnet(bestlasso_fit))!=0]
cat(selected_pred1, sep=", ")

## OnSE CV rule ##
ind_cv <- which.min(lasso_cvfit$cvm)
cvm_onesd <- lasso_cvfit$cvm[ind_cv]  + lasso_cvfit$cvsd[ind_cv]

lasso_cvfit$cvm < cvm_onesd
lambda_ind <- min(which(lasso_cvfit$cvm < cvm_onesd))
bestlasso_fit <- glmnet(x=X, y=Y, alpha=1, lambda=lasso_cvfit$lambda[lambda_ind])

lasso_cvfit$cvm[lambda_ind] 

coef.glmnet(bestlasso_fit)
selected_pred2 <- rownames(coef.glmnet(bestlasso_fit))[as.vector(coef.glmnet(bestlasso_fit))!=0]
selected_pred2

par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(lasso_cvfit$lambda, lasso_cvfit$cvm, type="b", col="blue", 
     xlim = c(min(lasso_cvfit$lambda), max(lasso_cvfit$lambda)), ylim=c(500, 1000), 
     xlab=bquote(lambda), ylab="Mean CV error", main="Minimum LOOCV rule")
abline(v=lasso_cvfit$lambda[ind_cv], col="green")
text(x=2.5+lasso_cvfit$lambda[ind_cv], y=950, labels=expression("" %<-% "Optimal model"))
text(x=7+lasso_cvfit$lambda[ind_cv], y=900, labels=
       paste0("sqrt(KWH)~", paste0(selected_pred1[2:5], sep="+", collapse="")), cex=0.75)
text(x=7+lasso_cvfit$lambda[ind_cv], y=850, labels=
       paste0(paste0(selected_pred1[6:7], sep="+", collapse=""),"..."), cex=0.75)
text(x=4+lasso_cvfit$lambda[ind_cv], y=800, labels=paste0("Number of parameters=", length(selected_pred1)))

par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(lasso_cvfit$lambda, lasso_cvfit$cvm, type="b", col="blue", 
     xlim = c(min(lasso_cvfit$lambda), max(lasso_cvfit$lambda)), ylim=c(500, 1000), 
     xlab=bquote(lambda), ylab="Mean CV error", main="One standard-error rule with LOOCV")

## One standard-error bar
abline(h=cvm_onesd, col="red")
text(x=15, y=cvm_onesd, labels=paste0("One SE rule thresholds=", round(cvm_onesd)))
arrows(lasso_cvfit$lambda, lasso_cvfit$cvup, lasso_cvfit$lambda, 
       lasso_cvfit$cvlo, angle = 90, code = 3, 
       length = 0.05, col = "black")
abline(v=lasso_cvfit$lambda[lambda_ind], col="green")
text(x=4+lasso_cvfit$lambda[lambda_ind], y=850, labels=paste0("Number of parameters=", length(selected_pred2)))
text(x=4.5+lasso_cvfit$lambda[lambda_ind], y=900, labels=
       paste0("sqrt(KWH)~", paste0(selected_pred2[2:3], sep="+", collapse=""),"..."), cex=0.75)
text(x=2.5+lasso_cvfit$lambda[lambda_ind], y=950, labels=expression("" %<-% "Optimal model"))




## LOOCV of selection methods
formula3 <- sqrt(KWH) ~ SQFTEST + LGTIN4TO8 + KWHPLPMP + FUELHEAT
lmfit3 <- lm(formula3, data=selected_df)
nullformula <- sqrt(KWH) ~ 1


lmcv <- function(D, formula., folds=5, seeds=2023){
  
  nD = nrow(D)
  if(nD < folds) stop("sample size should be larger than a given fold size")
  
  dim_p = ncol(D)-1
  
  Dpar = floor(nD/folds)
  set.seed(seeds)
  random_par = c(sample(rep(1:folds, each = Dpar), Dpar*folds, replace=F),
                 sample(1:folds, nD - Dpar*folds, replace=F))
  
  cv_error_vec = vector("numeric", length=folds)
  for(i in 1:folds){
    train = D[random_par!=i, ]
    cv = D[random_par==i, ]
    n_cv = nrow(cv)
    
    ## calculate estimates based on partitions for training
    trlm = lm(formula., data=train)
    
    ## calculate CV error based on the estimates and the CV partition
    precv = predict(trlm, newdata=cv)
    cv_error_vec[i] = sum((sqrt(cv$KWH) - precv)^2)/n_cv 
  }
  
  temp = NULL
  temp$cvm = mean(cv_error_vec)
  temp$folds = folds
  temp$n_sample = nD
  temp$n_partitions = table(random_par)
  
  return(temp)
}

lmcv_loocv <- lmcv(selected_df, formula3, folds=n)
null_loocv <- lmcv(selected_df, nullformula, seeds=3000)
summary(lmfit3)


## Comparing prediction performance in terms of LOOCV

null_cv <- null_loocv$cvm

cvm / null_cv
cvm_onesd / null_cv
lmcv_loocv$cvm / null_cv



### Ten folds
set.seed(1000)
lasso_cvfit <- cv.glmnet(x=X, y=Y, alpha=1, nfolds = 10)


## Minimum CV rule ##
lambda <- lasso_cvfit$lambda.min; cvm <- min(lasso_cvfit$cvm)
lambda;cvm

bestlasso_fit <- glmnet(x=X, y=Y, alpha=1, lambda=lambda)

coef.glmnet(bestlasso_fit)
selected_pred1 <- rownames(coef.glmnet(bestlasso_fit))[as.vector(coef.glmnet(bestlasso_fit))!=0]
selected_pred1

## OnSE CV rule ##
ind_cv <- which.min(lasso_cvfit$cvm)
cvm_onesd <- lasso_cvfit$cvm[ind_cv]  + lasso_cvfit$cvsd[ind_cv]

lasso_cvfit$cvm < cvm_onesd
lambda_ind <- min(which(lasso_cvfit$cvm < cvm_onesd))
bestlasso_fit <- glmnet(x=X, y=Y, alpha=1, lambda=lasso_cvfit$lambda[lambda_ind])

lasso_cvfit$cvm[lambda_ind] 

coef.glmnet(bestlasso_fit)
selected_pred2 <- rownames(coef.glmnet(bestlasso_fit))[as.vector(coef.glmnet(bestlasso_fit))!=0]
selected_pred2

par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(lasso_cvfit$lambda, lasso_cvfit$cvm, type="b", col="blue", 
     xlim = c(min(lasso_cvfit$lambda), max(lasso_cvfit$lambda)), ylim=c(500, 1000), 
     xlab=bquote(lambda), ylab="Mean CV error", main="Minimum ten-folds CV rule")
abline(v=lasso_cvfit$lambda[ind_cv], col="green")
text(x=2.5+lasso_cvfit$lambda[ind_cv], y=950, labels=expression("" %<-% "Optimal model"))

par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(lasso_cvfit$lambda, lasso_cvfit$cvm, type="b", col="blue", 
     xlim = c(min(lasso_cvfit$lambda), max(lasso_cvfit$lambda)), ylim=c(500, 1000), 
     xlab=bquote(lambda), ylab="Mean CV error", main="One standard-error rule with ten-folds CV")

## One standard-error bar
abline(h=cvm_onesd, col="red")
text(x=15, y=cvm_onesd, labels=paste0("One SE rule thresholds=", round(cvm_onesd)))
arrows(lasso_cvfit$lambda, lasso_cvfit$cvup, lasso_cvfit$lambda, 
       lasso_cvfit$cvlo, angle = 90, code = 3, 
       length = 0.05, col = "black")
abline(v=lasso_cvfit$lambda[lambda_ind], col="green")
text(x=2.5+lasso_cvfit$lambda[lambda_ind], y=950, labels=expression("" %<-% "Optimal model"))



