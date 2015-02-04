## load packages ####
library(data.table)
library(randomForest)

## load data ####
rf.dp <- readRDS("rf_dp60.RDS")

mdata <- readRDS("features.RDS")
target <- readRDS("y.RDS")



remove <- mdata[which(is.na(target$dp10))]$ord_id

mdata <- mdata[!ord_id%in%remove,]
target <- target[!ord_id%in%remove,]



mdata <- data.frame(mdata)
target <- data.frame(target)



names(mdata[-1])
names(target[6])

nrow(mdata[-1])
nrow(target[3])

mdata[1] <- target[6]

head(mdata)

head(target[3])

# find optimal numbers of variables to try splitting on at each node
bestmtry <- tuneRF(mdata[-1],
                   target$dp10, 
                   ntreeTry=100, 
                   stepFactor=1.5,
                   improve=0.01, 
                   trace=TRUE, 
                   plot=TRUE, 
                   dobest=FALSE)

# calibration algorithm
system.time(adult.rf <-randomForest(ord_id~.,data=mdata, mtry=2, ntree=100, 
                        keep.forest=TRUE, importance=TRUE,test=data$val))


# Importance Analysis
importance(adult.rf)
varImpPlot(adult.rf)


