# Load packages

source("c:/r/qserver.r")
library("ggplot2")
library("data.table")
library("zoom")

# c <- open_connection()

# Load data
load("CLdata.RData")
data.frame("Field"=names(cldords),"Class"=as.character(sapply(cldords,class)))
cldords

# Data filtering
data <- cldords[k!=0]
# compute signed price ps (BUY/SELL sign)
data[,ps:=p]
data[side=='SELL',ps:=-p]
data[,cumps:=cumsum(ps),by=list(ord_id)]

data <- data[,list(avg=mean(cumps),sigma=sd(cumps),.N),by=list(sym,k)]

data[,ci.l90:=avg-qnorm(0.95)*(sigma/sqrt(N))]
data[,ci.r90:=avg+qnorm(0.95)*(sigma/sqrt(N))]

data[,ci.l99:=avg-qnorm(0.995)*(sigma/sqrt(N))]
data[,ci.r99:=avg+qnorm(0.995)*(sigma/sqrt(N))]

data[,unique(sym)]

ggplot(data[sym=='CLF5'],aes(x=k))+
  geom_line(aes(y=avg))+
  xlab("Time (in sec)") +
  ylab("Price change (in USD)") +
  ggtitle("Price impact for CLF5")+
  geom_ribbon(aes(ymin=ci.l90, ymax=ci.r90), alpha=0.2, fill="blue")+
  geom_ribbon(aes(ymin=ci.l99, ymax=ci.r99), alpha=0.2, fill="blue")

































