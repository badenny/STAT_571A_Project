setwd(paste0(getwd(),"/STAT_571A_Project")) ## read csv with a proper path
recs2020_AZ <- read.csv("recs2020_AZ.csv")

## Packages
if("tidyverse" %in% rownames(installed.packages()) == FALSE)
{install.packages("tidyverse")
}
library(tidyverse)

if("moments" %in% rownames(installed.packages()) == FALSE)
{install.packages("moments")
}
library(moments)

# Type of data
sapply(recs2020_AZ, class)

# Summary statistics
sapply(recs2020_AZ, summary)

# Variance Skewness Kurtosis 
sapply(recs2020_AZ, FUN = function(x){ 
        stat = c(var(x), skewness(x), kurtosis(x)-3)
        names(stat) = c("var","skewness", "kurtosis")
        return(stat)}
       )

## Too large variance compared to others may cause unbalanced contribution for each predictor on response variable, although it will not affect its p-value.
## Some variables with small variance may have a negligible effect on response variable. It can be omitted for a better interpretation on the model.
## Skewness and Kurtosis may indicate the shape of distribution of each predictor. a bell-shaped (normal) distribution of predictor implies the both value will be close to zero.
## The distribution of predictor may not be important in inference, but too skewed distribution can provide lots of leverage points, which causes a high variance in model estimation (Instable slopes depending on observations).


## Explantory data analysis
attach(recs2020_AZ)

## Columns for "~PMP" <- ## I found these columns contains lots of zero-value samples, while some of samples had a quite large value. This may cause a worse fit of regression model.
par(mfrow=c(1,3))
recs2020_AZ %>% select(matches("PMP")) %>% sapply(FUN=function(x) plot(x, KWH, ylab="KWH"))
## The relationship between predictor and response looks not clear, so I suggest a binary transformation (0=zero, 1=others).

recs2020_AZ %>% select(matches("PMP")) %>% sapply(FUN=function(x) plot(factor(1*(x>0), levels=c(0,1)), KWH, ylab="KWH"))

binaried_PMP <- recs2020_AZ %>% select(matches("PMP")) %>% sapply(FUN=function(x) factor(1*(x>0)+1*(x>1000), levels=c(0,1,2)))

regKWH <- lm((KWH)^(1/2)~., data=data.frame(KWH=KWH, binaried_PMP[,1]))
summary(regKWH)

regKWH <- lm((KWH)^(1/2)~., data=data.frame(KWH=KWH, recs2020_AZ$KWHPLPMP))
summary(regKWH)


## By inspecting the linear relationship of any pair for the two type of "PMP", they possess almost same information.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.3/strwidth(txt)
  text(0.5, 0.5, paste("r =",txt), cex = cex.cor * r)
}
recs2020_AZ %>% select(matches("PMP")) %>% pairs(upper.panel=panel.cor)
## I guess we can select one predictor from "PMP" predictors, or apply the dimension reduction, like PCA.


## Columns for discrete variables

integer_col <- sapply(recs2020_AZ, is.integer)
recs2020_AZ[, integer_col]

discrete_col <- c("BEDROOMS", "NCOMBATH", "TOTROOMS", "MONPOOL", "FUELPOOL", "NUMFRIG", "TVCOLOR", "SQFTRANGE", "ZFUELPOOL", "ZMONPOOL", "ZPOOLPUMP")

df_recs_discrete <- recs2020_AZ %>% select(discrete_col) 
pairs(df_recs_discrete)


## What does "-2" mean in these columns?
## If the number of levels is not large (less than five), transform it as a factor, or ordered factor may provide the better inference of model parameters.
