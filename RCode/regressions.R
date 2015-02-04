library(data.table)
library(ggplot2)

load("machinedata.RData")

# Explanatoy Variables
data.reg <- trainset[[1]]

# Transform Factors
## Agressive=0, passive=1
## Buy=1, Sell=-1

factor.col <- c("type0","type1","type2","type3","type4","type5","side0","side1","side2","side3","side4","side5")
data.reg[,(factor.col):=lapply(.SD,function(x) {as.numeric(x)-1} ),.SDcols=factor.col]
## Variable Side is -1 or 1
side.col <- c("side0","side1","side2","side3","side4","side5")
data.reg[,(side.col):=lapply(.SD,function(x){-(2*x-1)}),.SDcols=side.col]

# Add response variable
y <- trainset[[2]]
data.reg <- merge(data.reg,y,b='ord_id')
data.reg

rm(side.col,factor.col,y,trainset,valset)

# Regressions

reg_dp <- list()

reg_dp[[1]] <- lm(dp1*side0~tau1+tau2+tau3+tau4+tau5,data=data.reg[type0==0&size0==1&side0==1])
reg_dp[[2]] <- lm(dp1*side0~log(size1)+log(size2)+log(size3)+log(size4)+log(size5),data=data.reg[type0==0&size0==1&side0==1])
reg_dp[[3]] <- lm(dp1*side0~tau1*log(size1)+tau2*log(size2)+tau3*log(size3)+tau4*log(size4)+tau5*log(size5),data=data.reg[type0==0&size0==1&side0==1])

lapply(reg_dp,summary)

reg_tr <- list()
reg_tr[[1]] <- lm(log(1+tr10/100)~tau1+tau2+tau3+tau4+tau5,data=data.reg[type0==0&size0==1&side0==1])
reg_tr[[2]] <- lm(tr60*side0~log(size0)+log(size1)+log(size2)+log(size3)+log(size4)+log(size5),data=data.reg[type0==0])
lapply(reg_tr,summary)

ggplot(data.reg[tau1<1000&type0==0], aes(x=tau1, y=dp60))+geom_point(shape=1)+geom_smooth(method=lm)
ggplot(data.reg[tau5<250&type0==0], aes(x=tau5, y=tr60))+geom_point(shape=1)+geom_smooth(method=lm)
