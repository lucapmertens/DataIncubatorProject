## load packages ####
library(randomForest)
library(data.table)

## load data ####
load("machinedata.RData")

mdata <- trainset$mdata

factor.col <- c("type0","type1","type2","type3","type4","type5","side0","side1","side2","side3","side4","side5")
mdata[,(factor.col):=lapply(.SD,as.numeric),.SDcols=factor.col]
mdata[,(factor.col):=lapply(.SD,'-',1),.SDcols=factor.col]

side.col <- c("side0","side1","side2","side3","side4","side5")
mdata[,(side.col):=lapply(.SD,function(x){(2*x-1)}),.SDcols=side.col]
mdata[,(side.col):=lapply(.SD,'*',-1),.SDcols=side.col]


crossdata <- valset$mdata
factor.col <- c("type0","type1","type2","type3","type4","type5","side0","side1","side2","side3","side4","side5")
crossdata[,(factor.col):=lapply(.SD,as.numeric),.SDcols=factor.col]
crossdata[,(factor.col):=lapply(.SD,'-',1),.SDcols=factor.col]

side.col <- c("side0","side1","side2","side3","side4","side5")
crossdata[,(side.col):=lapply(.SD,function(x){(2*x-1)}),.SDcols=side.col]
crossdata[,(side.col):=lapply(.SD,'*',-1),.SDcols=side.col]

mdata <- data.frame(mdata)
mdata[,'ord_id'] <- NULL
mdata$dp <- trainset$y[,dp60]

crossdata <- data.frame(crossdata)
crossdata[,'ord_id'] <- NULL
crossdata$dp <- valset$y[,dp60]

mdata[,"side1"] <- NULL
mdata[,"side2"] <- NULL
mdata[,"side3"] <- NULL
mdata[,"side4"] <- NULL
mdata[,"side5"] <- NULL

crossdata[,"side1"] <- NULL
crossdata[,"side2"] <- NULL
crossdata[,"side3"] <- NULL
crossdata[,"side4"] <- NULL
crossdata[,"side5"] <- NULL




names(mdata)
names(mdata[19])



dp.rf <- readRDS("random_forests/rf_dp1.RDS")
print(dp.rf)
importance(dp.rf)

varImpPlot(dp.rf,main="dp120")

plot(dp.rf)

## predict values
dp.pr <- predict(dp.rf,crossdata)

plot(crossdata$dp[crossdata$side0==1],col="red")

table(crossdata$type0)

plot(dp.pr[crossdata$type0==0],crossdata$dp[crossdata$type0==0])

pmodel <- lm(crossdata$dp~dp.pr-1)
summary(pmodel)

range(dp.pr)



