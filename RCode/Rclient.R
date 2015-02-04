library("zoom")
library("ggplot2")
library("data.table")
library("reshape2")
source("c:/r/qserver.r")
c <- open_connection("atlas",12012)

setwd("C:/Users/LucaPhilippe/Dropbox/projects/Data Incubator/project")

returns  <- execute(c, "returns")

returns <- data.table(returns)
returns[,.N,by=list(sym,siz)]

tickers <- unique(returns[,sym]) # 6 tickers
nrow(returns)                    # 297 trajectories

nrow(returns[sym==tickers[1],])  # 18+12+2+45+65+155 = 297

oldnames <- names(returns)
newnames <- oldnames
newnames[4:63] <- 1:60
setnames(returns,oldnames,newnames)

mret <- melt(returns,id.vars=c("sym","side","siz"))
setnames(mret,c("variable","siz"),c("Time","Size"))

mret[[4]] <-as.numeric(mret[[4]])

mret[[3]] <- as.factor(mret[[3]])

p1 <- ggplot(mret[sym==tickers[5]&side=="SELL"], 
             aes(x=Time, y=cumsum(value), colour=Size)
             ) + geom_line()
p1
#abline(h=0,col='blue')

zm()
  





lines(cumsum(SELLret),col="red")
title(ticker)

zm()
close_connection(c)
