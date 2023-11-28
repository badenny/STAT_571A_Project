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

cat_columns <- c("ACEQUIPM_PUB", "TYPEHUQ", "ADQINSUL", "KWHPLPMP")

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
daz2[[select_col]][which(daz[[select_col]]==0)] = 0
daz2[[select_col]][which(daz[[select_col]]>0)] = 1


daz2 %>% select(ACEQUIPM_PUB, TYPEHUQ, ADQINSUL, KWHPLPMP) %>% apply(2, as.factor)



selected_df <- daz2 %>% select(KWH, SQFTEST, LGTINLED, LGTINCFL, LGTINCAN, LGTIN1TO4, LGTIN4TO8, LGTINMORE8) %>% 
  cbind(daz2 %>% select(ACEQUIPM_PUB, TYPEHUQ, ADQINSUL, KWHPLPMP) %>% apply(2, as.factor))


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

## Both methods give consistent results for "sqrt(KWH) ~ SQFTEST + LGTIN4TO8 + KWHPLPMP"
lmfit3 <- lm(sqrt(KWH) ~ SQFTEST + LGTIN4TO8 + KWHPLPMP, data=selected_df)

## 3. LASSO

X <- model.matrix(KWH~., data=selected_df)[,-1]
Y <- sqrt(selected_df$KWH)

### Five-fold CV
set.seed(1000)
lasso_cvfit <- cv.glmnet(x=X, y=Y, alpha=1, nfolds = 5)

lambda <- lasso_cvfit$lambda.min; cv_error <- min(lasso_cvfit$cvsd)
lambda

bestlasso_fit <- glmnet(x=X, y=Y, alpha=1, lambda=lambda)
coef.glmnet(bestlasso_fit)
selected_pred <- rownames(coef.glmnet(bestlasso_fit))[as.vector(coef.glmnet(bestlasso_fit))!=0]
selected_pred

### LOOCV
set.seed(1001)
lasso_cvfit <- cv.glmnet(x=X, y=Y, alpha=1, nfolds = n)

lambda <- lasso_cvfit$lambda.min; cv_error <- min(lasso_cvfit$cvsd)
lambda

bestlasso_fit <- glmnet(x=X, y=Y, alpha=1, lambda=lambda)
coef.glmnet(bestlasso_fit)
selected_pred <- rownames(coef.glmnet(bestlasso_fit))[as.vector(coef.glmnet(bestlasso_fit))!=0]
selected_pred

## 4. Elastic net
alp <- seq(0, 1, length=11) 
cv_alpha <- sapply(alp, simplify = FALSE, FUN=function(a){
  set.seed(1002)
  enet_cvfit = cv.glmnet(X, Y, nfolds = 5, alpha = a)
  list(alpha=a, lambda=enet_cvfit$lambda.min, cv_error=min(enet_cvfit$cvsd))
})
matlambda <- sapply(cv_alpha, FUN=function(a){
  c(a$alpha, a$lambda, a$cv_error)
})
matcoeff <- do.call(cbind, lapply(cv_alpha, FUN=function(a){
  enet_fit = glmnet(X, Y, alpha=a$alpha, lambda=a$lambda)
  coef.glmnet(enet_fit)
}))

colnames(matlambda) <- colnames(matcoeff) <- paste0("alpha=",alp)
rownames(matlambda) <- c("alpha","lambda.min", "min.cv")

## the best lambda and its CV error.
matlambda
  

