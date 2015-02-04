require(data.table)
require(ggplot2)
load("C:/Users/Lira/Desktop/RCode/CLdata.RData")

head(cldords)


data <- cldords[k!=0]
head(data)
data[,ps:=p]
data[side=='SELL',ps:=-p]
data[,cumps:=cumsum(ps),by=list(ord_id)]

data <- data[,list(avg=mean(cumps),sigma=sd(cumps),.N),by=list(sym,k)]

data[,ci.l:=avg-qnorm(0.95)*(sigma/sqrt(N))]
data[,ci.r:=avg+qnorm(0.95)*(sigma/sqrt(N))]

ggplot(data[sym=='CLF5'],aes(x=k))+geom_line(aes(y=avg))+
  geom_line(aes(y=ci.l),linetype=3)+geom_line(aes(y=ci.r),linetype=3)

  
data[,unique(sym)]
