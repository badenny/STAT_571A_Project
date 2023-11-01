d = read.csv('recs2020_public_v5.csv')

#Filter data for homes in AZ
isArizona = d[,6]=="Arizona"
daz = d[isArizona,]

#Remove data which do not have changing predictors
removeList = c()
for(j in 1:ncol(daz)){
  uniqueElems = unique(daz[,j])
  numUnique = length(uniqueElems)
  if(numUnique == 1){
    removeList = c(removeList, j)
  }
}

#find data which are basically the same (i.e. highly corrleated with) as KWH
daz = daz[,-removeList]
rSquared = c()
for(j in 1:ncol(daz)){
  myFit = lm(KWH ~ daz[,j], data=daz)
  sf = summary(myFit)
  rSquared = c(rSquared,sf$r.squared)
}
daz = daz[,-which(rSquared>.5 & rSquared < 1)]

rSquared = c()
for(j in 1:ncol(daz)){
  myFit = lm(KWH ~ daz[,j], data=daz)
  sf = summary(myFit)
  rSquared = c(rSquared,sf$r.squared)
}

#Keep the rest of the data which have correlations >.2
daz2 = daz[,which(rSquared>.2)]
write.csv(daz2, file="recs2020_AZ.csv")


