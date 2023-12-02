## ----setup, echo=F-------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(cache = F)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# read the CSV
energy_data <- read.csv("/Users/udaytalwar/Documents/STAT571A/FinalProjectData/recs2020_public_v5.csv")


## ------------------------------------------------------------------------------------------------------------------------------------------------------

t1 <- aggregate(energy_data$KWH, by = list(energy_data$state_name), FUN = median)
t1 <- t1[order(t1$x, decreasing = TRUE),]

barplot(t1$x, names.arg = t1$Group.1, cex.names = 0.7, cex.axis = 0.75, 
        las = 2,
        col = 'blue', main = 'Median Energy Consumption by State',
        ylab = 'Median Energy Consumption in KwH')

t2 <- aggregate(energy_data$KWH, by = list(energy_data$DIVISION), FUN = median)
t2 <- t2[order(t2$x, decreasing = TRUE),]

barplot(t2$x, names.arg = t2$Group.1, cex.names = 0.6, cex.axis = 0.75, 
        las = 2,
        col = 'blue', main = 'Median Energy Consumption by Census Division',
        ylab = 'Median Energy Consumption in KwH')

t3 <- aggregate(energy_data$KWH, by = list(energy_data$BA_climate), FUN = median)
t3 <- t3[order(t3$x, decreasing = TRUE),]

barplot(t3$x, names.arg = t3$Group.1, cex.names = 0.75, cex.axis = 0.75, 
        las = 2,
        col = 'blue', main = 'Median Energy Consumption by Climate',
        ylab = 'Median Energy Consumption in KwH')



## ------------------------------------------------------------------------------------------------------------------------------------------------------
library(plyr)
var_of_int <- c('SQFTEST', 'LGTINLED','LGTINCFL','LGTINCAN', 'LGTIN1TO4', 
                'LGTIN4TO8', 'LGTINMORE8', 'LGTOUTNITE')

resp <- 'KWH'

#subset BA_climate = Hot-Dry

  #quick check of most commong climate type in AZ
az_df <- subset(energy_data, state_name %in% 'Arizona')

count(az_df['BA_climate'])

#subsetted data for Hot-Dry areas 

hd_df <- subset(energy_data, BA_climate %in% 'Hot-Dry')

df_of_interest <- hd_df[, c(resp, var_of_int)]

#drop rows where homes do not have indoor lights
df_of_interest <- subset(df_of_interest, LGTINLED != 0 & LGTINCFL != 0 & LGTINCAN != 0)



fromvals <- c('1','2','3','4')
tovals <- c('All', 'Most', 'AboutHalf', 'Some')

df_of_interest$LGTINLED <- as.character(df_of_interest$LGTINLED)
df_of_interest$LGTINCFL <- as.character(df_of_interest$LGTINCFL)
df_of_interest$LGTINCAN <- as.character(df_of_interest$LGTINCAN)

df_of_interest$LGTINLED <- mapvalues(df_of_interest$LGTINLED, from = fromvals, to = tovals)
df_of_interest$LGTINCFL <- mapvalues(df_of_interest$LGTINCFL, from = fromvals, to = tovals)
df_of_interest$LGTINCAN <- mapvalues(df_of_interest$LGTINCAN, from = fromvals, to = tovals)
df_of_interest$LGTOUTNITE <- mapvalues(df_of_interest$LGTOUTNITE, from = -2, to = 0)

fromvals2 <- c('All', 'Most', 'AboutHalf', 'Some')
tovals2 <- c(1.00, 0.75, 0.5, 0.25)

df_of_interest$PropLED <- mapvalues(df_of_interest$LGTINLED, from = fromvals2, to = tovals2)
df_of_interest$PropOther <- 1 - as.numeric(df_of_interest$PropLED)


contins <- c('SQFTEST', 'LGTIN1TO4', 'LGTIN4TO8', 'LGTINMORE8')

# df_of_interest[, contins] <- scale(df_of_interest[, contins])


testdf <- df_of_interest[-58, ]

# + BA_climate
baselm <- lm(KWH ~ SQFTEST + LGTINLED + LGTINCFL + LGTINCAN  
             + LGTIN1TO4 + LGTIN4TO8 + LGTINMORE8, data = df_of_interest)

summary(baselm)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(baselm)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
library(car)
bc <- boxCox(baselm)

bc_optimal <-bc$x[which.max(bc$y)]
bc_optimal


## ------------------------------------------------------------------------------------------------------------------------------------------------------
extendlm <- lm(I(KWH^(1/4)) ~ SQFTEST + LGTINLED + LGTINCFL + LGTINCAN  
             + LGTIN1TO4 + LGTIN4TO8 + LGTINMORE8, data = df_of_interest)

summary(extendlm)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(extendlm)

## ------------------------------------------------------------------------------------------------------------------------------------------------------
df_of_interest[58, ]


## ------------------------------------------------------------------------------------------------------------------------------------------------------
plot(KWH ~ SQFTEST, data = df_of_interest, pch = 16,
     col = as.factor(df_of_interest$LGTINMORE8), ylab = 'KWH',
     xlab = 'SQFTEST',
     main = 'Energy Consumption (KWH) vs House Size (SQFTEST)')
legend("topright", pch = 16, col = 1:4,
       legend = levels(as.factor(df_of_interest$LGTINMORE8)),
       title = 'Tree ID', cex = 0.5)

df1 <-  subset(df_of_interest, df_of_interest$LGTINCAN == 'AboutHalf')
df2 <-  subset(df_of_interest, df_of_interest$LGTINCAN == 'All')
df3 <-  subset(df_of_interest, df_of_interest$LGTINCAN == 'Most')
df4 <-  subset(df_of_interest, df_of_interest$LGTINCAN == 'Some')


sqft_seq1 <- seq(min(df1$SQFTEST), max(df1$SQFTEST), 100)
sqft_seq2 <- seq(min(df2$SQFTEST), max(df2$SQFTEST), 100)
sqft_seq3 <- seq(min(df3$SQFTEST), max(df3$SQFTEST), 100)
sqft_seq4 <- seq(min(df4$SQFTEST), max(df4$SQFTEST), 100)

lm1 <- lm(KWH ~ SQFTEST, data = subset(df_of_interest, df_of_interest$LGTINCAN == 'AboutHalf'))
preds1 <- predict(lm1, newdata = data.frame(SQFTEST = sqft_seq1))
lm2 <- lm(KWH ~ SQFTEST, data = subset(df_of_interest, df_of_interest$LGTINCAN == 'All'))
preds2 <- predict(lm2, newdata = data.frame(SQFTEST = sqft_seq2))
lm3 <- lm(KWH ~ SQFTEST, data = subset(df_of_interest, df_of_interest$LGTINCAN == 'Most'))
preds3 <- predict(lm3, newdata = data.frame(SQFTEST = sqft_seq3))
lm4 <- lm(KWH ~ SQFTEST, data = subset(df_of_interest, df_of_interest$LGTINCAN == 'Some'))
preds4 <- predict(lm4, newdata = data.frame(SQFTEST = sqft_seq4))




lines(sqft_seq1, preds1, col = 1, lwd = 2)
lines(sqft_seq2, preds2, col = 2, lwd = 2)
lines(sqft_seq3, preds3, col = 3, lwd = 2)
lines(sqft_seq4, preds4, col = 4, lwd = 2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
x = 1:dim(df_of_interest)[1]
difoffits <- dffits(baselm)
plot(x, difoffits, ylim = c(-2,2),
     col = ifelse(x == 58, 'red','black'),
     pch = ifelse(x == 58, 19, 1))


a <- head(df_of_interest, 60)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
png(filename = '/Users/udaytalwar/Documents/STAT571A/FinalProjectData/ReportPics/DFBetas.png',
    width = 1200, height = 720)
difofbeta <- dfbetas(extendlm)
difobetasplit <- split(difofbeta, rep(1:13, c(rep(494, 13))))

maxabs <- function(x){
  return(max(abs(x)))
}
lapply(difobetasplit, maxabs)

plot(x ,difobetasplit$`1`, ylab = 'DFBETAS', xlab = 'Observation #', col = 1, 
     pch = ifelse(x == 58, 19, 1), ylim = c(-2,2), cex.lab = 1.5, cex.axis = 1.5)
legend("topright", pch = 16, col = 1:13, horiz = T,
       legend = seq(1,13),
       title = expression(beta[i]), cex = 1.25)

points(x, difobetasplit$`2`, col = 2, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`3`, col = 3, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`4`, col = 4, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`5`, col = 5, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`6`, col = 6, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`7`, col = 7, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`8`, col = 8, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`9`, col = 9, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`10`, col = 10, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`11`, col = 11, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`12`, col = 12, pch = ifelse(x == 58, 19, 1))
points(x, difobetasplit$`13`, col = 13, pch = ifelse(x == 58, 19, 1))
dev.off()


## ------------------------------------------------------------------------------------------------------------------------------------------------------
plot.ecdf(energy_data$SQFTEST, xlab = 'Housing Unit Size (SqFt)')

## ------------------------------------------------------------------------------------------------------------------------------------------------------
#standardized regression 

X_scl <- data.frame(df_of_interest)
X_scl[, contins] <- scale(X_scl[, contins])


extendlm_std <- lm(I(KWH^(1/4)) ~ SQFTEST + LGTINLED + LGTINCFL + LGTINCAN  
             + LGTIN1TO4 + LGTIN4TO8 + LGTINMORE8, data = X_scl)

summary(extendlm_std)



## ------------------------------------------------------------------------------------------------------------------------------------------------------
#standardized reg plot 
png(filename = '/Users/udaytalwar/Documents/STAT571A/FinalProjectData/ReportPics/StandardizedRegPlot.png',
    width = 1200, height = 720)
plot(KWH ~ SQFTEST, data = df_of_interest, pch = 16,
     col = as.factor(df_of_interest$LGTINLED), ylab = 'KWH^(1/4)',
     xlab = 'SQFTEST',
     main = 'Energy Consumption (KWH^(1/4)) vs House Size (SQFTEST)', 
     cex.main = 2, cex.lab = 1.25)
legend("topright", pch = 16, col = 1:4,
       legend = levels(as.factor(df_of_interest$LGTINLED)),
       title = 'ProportionOfLED', cex = 1.25)

df1 <-  subset(df_of_interest, df_of_interest$LGTINLED == 'AboutHalf')
df2 <-  subset(df_of_interest, df_of_interest$LGTINLED == 'All')
df3 <-  subset(df_of_interest, df_of_interest$LGTINLED == 'Most')
df4 <-  subset(df_of_interest, df_of_interest$LGTINLED == 'Some')


sqft_seq1 <- seq(min(df1$SQFTEST), max(df1$SQFTEST), 100)
sqft_seq2 <- seq(min(df2$SQFTEST), max(df2$SQFTEST), 100)
sqft_seq3 <- seq(min(df3$SQFTEST), max(df3$SQFTEST), 100)
sqft_seq4 <- seq(min(df4$SQFTEST), max(df4$SQFTEST), 100)

lm1 <- lm(I(KWH^(1/4)) ~ SQFTEST, data = subset(df_of_interest, df_of_interest$LGTINLED == 'AboutHalf'))
preds1 <- predict(lm1, newdata = data.frame(SQFTEST = sqft_seq1))
lm2 <- lm(I(KWH^(1/4)) ~ SQFTEST, data = subset(df_of_interest, df_of_interest$LGTINLED == 'All'))
preds2 <- predict(lm2, newdata = data.frame(SQFTEST = sqft_seq2))
lm3 <- lm(I(KWH^(1/4)) ~ SQFTEST, data = subset(df_of_interest, df_of_interest$LGTINLED == 'Most'))
preds3 <- predict(lm3, newdata = data.frame(SQFTEST = sqft_seq3))
lm4 <- lm(I(KWH^(1/4)) ~ SQFTEST, data = subset(df_of_interest, df_of_interest$LGTINLED == 'Some'))
preds4 <- predict(lm4, newdata = data.frame(SQFTEST = sqft_seq4))




lines(sqft_seq1, preds1^4, col = 1, lwd = 2)
lines(sqft_seq2, preds2^4, col = 2, lwd = 2)
lines(sqft_seq3, preds3^4, col = 3, lwd = 2)
lines(sqft_seq4, preds4^4, col = 4, lwd = 2)
dev.off()


## ------------------------------------------------------------------------------------------------------------------------------------------------------
light_form_ms <- I((KWH)^(1/4)) ~ SQFTEST
light_form <- I(KWH^(1/4)) ~ SQFTEST + LGTINLED + LGTINCFL + LGTINCAN + LGTIN1TO4 + LGTIN4TO8 + LGTINMORE8
  
stepwise_both <- step(lm(light_form_ms, data = df_of_interest), k = 2,
                      scope = list(upper = light_form, lower =  I((KWH)^(1/4)) ~ 1), 
                      direction = "both")
stepwise_both


## ------------------------------------------------------------------------------------------------------------------------------------------------------
lmsimple <- lm(I(KWH^(1/4)) ~ SQFTEST , data = df_of_interest)

lm1 <- lm(I(KWH^(1/4)) ~ SQFTEST + LGTINLED + LGTINMORE8, data = df_of_interest)

lm2 <- lm(I(KWH^(1/4)) ~ SQFTEST + LGTINLED + LGTINCFL + LGTINCAN  
             + LGTIN1TO4 + LGTIN4TO8 + LGTINMORE8 + LGTINMORE8 * LGTINLED + 
               LGTINMORE8 * LGTINCAN + LGTINMORE8 * LGTINCFL, data = df_of_interest)

anova(lm1, lm2, test = 'LRT')


## ------------------------------------------------------------------------------------------------------------------------------------------------------
#final model results
final_lm <- lm(I(KWH^(1/4)) ~ SQFTEST + LGTINLED + LGTINMORE8, data = X_scl)

summary(final_lm)

