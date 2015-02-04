## load packages ####
library(data.table)
library(randomForest)

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


# find optimal numbers of variables to try splitting on at each node
#N <- nrow(trainset$mdata)
#someguys <- sample(N,4000)
#bestmtry <- tuneRF(mdata[someguys,-19],
#                    mdata[someguys,19], 
#                    ntreeTry=100, 
#                    stepFactor=1.5,
#                    improve=0.01, 
#                    trace=TRUE, 
#                    plot=TRUE, 
#                    dobest=FALSE)

## calibration algorithm
# mdata$dp <- trainset$y[,dp1]
# system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
# saveRDS(dp.rf,"rf_dp1.RDS")
# 
# mdata$dp <- trainset$y[,dp10]
# system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
# saveRDS(dp.rf,"rf_dp10.RDS")
# 
# mdata$dp <- trainset$y[,dp20]
# system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
# saveRDS(dp.rf,"rf_dp20.RDS")
# 
# mdata$dp <- trainset$y[,dp30]
# system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
# saveRDS(dp.rf,"rf_dp30.RDS")
# 
# mdata$dp <- trainset$y[,dp60]
# system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
# saveRDS(dp.rf,"rf_dp60.RDS")
# 
# mdata$dp <- trainset$y[,dp90]
# system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
# saveRDS(dp.rf,"rf_dp90.RDS")
# 
# mdata$dp <- trainset$y[,dp120]
# system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
# saveRDS(dp.rf,"rf_dp120.RDS")

mdata$dp <- trainset$y[,tr1]
system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
saveRDS(dp.rf,"rf_tr1.RDS")

mdata$dp <- trainset$y[,tr10]
system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
saveRDS(dp.rf,"rf_tr10.RDS")

mdata$dp <- trainset$y[,tr20]
system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
saveRDS(dp.rf,"rf_tr20.RDS")

mdata$dp <- trainset$y[,tr30]
system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
saveRDS(dp.rf,"rf_tr30.RDS")

mdata$dp <- trainset$y[,tr60]
system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
saveRDS(dp.rf,"rf_tr60.RDS")

mdata$dp <- trainset$y[,tr90]
system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
saveRDS(dp.rf,"rf_tr90.RDS")

mdata$dp <- trainset$y[,tr120]
system.time(dp.rf <- randomForest(dp ~ ., data=mdata, ntree=200, mtry=2,importance=TRUE, na.action=na.omit))
saveRDS(dp.rf,"rf_tr120.RDS")




print(dp.rf)




plot(dp.rf,main="randomForest MSE")

## Show "importance" of variables: higher value mean more important:
round(importance(dp.rf), 2)

varImpPlot(dp.rf)

test <- rfcv(trainx=mdata[1:500,-19],trainy=mdata[1:500,19],cv.fold=5)

table()

## predict values
dp.pr <- predict(dp.rf,crossdata)
plot(dp.pr[crossdata$side0==1])#,crossdata$dp[crossdata$side0==5])

pmodel <- lm(crossdata$dp~dp.pr-1)
summary(pmodel)

range(dp.pr)





