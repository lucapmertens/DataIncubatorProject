## ## Init

## load packages ####

#source("c:/r/qserver.r")
packs <- c("ggplot2","data.table","zoom","png","grid")
 lapply(packs, require, character.only = TRUE); 

## local functions ####

myrep <- function(vec,ntimes){
  return(as.vector(sapply(vec, function(x) rep(x,ntimes))))
}

jN <- function(N){
  return(sum(N))
}
javg <- function(N,avg){
  return(sum(N*avg/sum(N))) 
}
jsd <- function(n,avg,sigma){
  N <- sum(n)
  if(N==1){return(0)}  
  EXX <- sum((n-1)/N*sigma^2 + n/N*avg^2)
  EX2 <- sum(n/N*avg)^2
  return(sqrt(N/(N-1)*(EXX-EX2)))
} 

ci.u <- function(y,sigma,p){
  return(y+qnorm((p+1)/2)*sigma)
} 
ci.d <- function(y,sigma,p){
  return(y-qnorm((p+1)/2)*sigma)
} 

## load data ####
orders <- readRDS("orders.RDS")
load("filemanager.RData")

## compute features ####

goodsym <- unique(orders$sym)[c(1:12,14)]

## Classifypassive/aggressive
orders <- orders[sym%in%goodsym,]


algopar <- unique(orders$algoparams)
algopar
table(orders$algoparams)

passive <- algopar[1]
aggressive <- algopar[c(2,3,4,5,6,7,8,10,11)]
passiverisk <- algopar[9]

orders[algoparams%in%passive,balgo:="passive"]
orders[algoparams%in%aggressive,balgo:="aggressive"]
orders[algoparams%in%passiverisk,balgo:="passiverisk"]
orders[,balgo:=factor(balgo)]

## classify size
size_bins <- c(0,1,10,100,100000) # see exploraotry data analysis
orders[,bsize:=.bincode(size,size_bins)]
orders[,month:=format(orders$date,"%Y-%m")]

# orders[order(bsize),unique(.N),by=bsize]
# table(orders[,list(bsize,balgo)])



## compute average trajectories ####
features1 <- c("sym","month") 
features2 <- c("side")
features3 <- c("bsize")
features4 <- c("balgo")
features5 <- c("balgo","side")
features6 <- c("balgo","bsize")

features <- features5
allret <- data.table()
system.time(
  for(f in 1:8){
    ## compute returns from pricedata 
    pricedata <- readRDS(filenames[f])
    pricedata <- data.table(pricedata)
    p0 <- myrep(pricedata[k==0,p],180)
    ret <- pricedata[k>0,]
    ret[,p:=p/p0]
    ret[,p:=cumsum(p),by=list(ord_id)]
    setkey(ret,ord_id)
    rm(pricedata)
    if(!"side"%in%features){                    # if side is not a featute then
      SELL_ords <- orders[side=="SELL",ord_id]  # returns of sell orders are changed 
      ret[ord_id%in%SELL_ords,p:=-p]            # in sign to be averaged with buy orders
    }
    
    ## merge with metadata
    cols <- c("p","k",features)
    fret <- orders[ret,j=cols,with=F]  
    cols <- c(features,"k")
    fret <- fret[,list(avg=mean(p),sigma=sd(p),.N),by=cols]
    
    allret <- rbind(allret,fret)
    rm(ret,fret)
  })
cols <- c(features,"k")
fret <- allret[,list(N=jN(N),avg=javg(N,avg),sigma=jsd(N,avg,sigma)),by=cols]

colnames(fret)
nrow(fret)/180
table(sfret$balgo,sfret$bsize)


# features = balgo,bsize
saveRDS(fret,"size+algo.RDS")

setkey(fret,bsize)
setkey(fret,balgo)

sfret <- fret[!(balgo=="passiverisk"|bsize%in%c(4))]
sfret <- sfret[!(bsize==3),]
sfret[,class:=factor(paste(balgo,bsize))]

unique(sfret$class)
table(sfret$class)

## Plot results
img <- readPNG("gradient.png") 
g <- rasterGrob(img, interpolate=TRUE)
g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))
theme_set(theme_gray(base_size = 15))
market_impact <- 
  ggplot(sfret[k<=120&bsize<3],aes(x=k,colour=class))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  scale_x_continuous( breaks=seq(0,120,by=30))+
  scale_y_continuous( breaks=seq(-2e-4,3e-4,by=5e-5))+
  theme(plot.background = element_rect(fill = "#C0C0C0", colour = "#111111"),
        axis.text = element_text(colour="black",size=18),
        legend.background = element_rect(fill = "#A0A0A0", colour = "#333333"),
        panel.grid=element_line(linetype="dotted",colour="#00FFFF"))+
  geom_vline(xintercept=seq(0, 120, by=30),linetype="dotted",colour="#00FFFF")+
  geom_hline(yintercept=seq(0, 1.2*10^-4, by=5e-05),linetype="dotted",colour="#0088D8")+
  
  xlab("Time (in sec)") +
  ylab("Relative price change") +
  ggtitle("Price impact for CL contracts in 2014")+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.9), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.9),
                  fill=class), alpha=0.3, colour=NA)+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.99), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.99),
                  fill=class), alpha=0.3, colour=NA)+
  geom_line(aes(y=avg),size=0.8)+
  scale_fill_manual(values=c("#00FF00","#FF3333","#FFFF00","#0055FF"))+
  scale_color_manual(values=c("#00FF00","#FF3333","#FFFF00","#0055FF"))

market_impact
ggsave(market_impact,file="MIplots/algo+size.jpg",height=10,width=15)




# features = balgo,side
# saveRDS(fret,"side+algo.RDS")
# saveRDS(fret,"side+algo+clean.RDS")

#fret <- readRDS("side+algo.RDS")

sfret <- fret[balgo!="passiverisk"]

unique(sfret$balgo)
names(sfret)

sfret <- sfret[,list(balgo,side,k,avg)]

nalgo <-  (sfret$balgo=="aggressive")-(sfret$balgo=="passive")
nside <- (sfret$side=="BUY")-(sfret$side=="SELL")

sfret$side <- nside
sfret$balgo <- nalgo

class(sfret$k)


test <- melt(sfret,id.vars="avg")

unique(sfret$balgo)
unique(sfret$side)

LB <- sfret[sfret$balgo=="passive"&side=="BUY",avg]
LS <- sfret[sfret$balgo=="passive"&side=="SELL",avg]
MB <- sfret[sfret$balgo=="aggressive"&side=="BUY",avg]
MS <- sfret[sfret$balgo=="aggressive"&side=="SELL",avg]

mean(mean((LB/MB)),mean((LS/MS)))

setkey(fret,side)
setkey(fret,balgo)

sfret <- fret[balgo!="passiverisk"]
sfret[,class:=factor(paste(side,balgo))]

## Plot results
img <- readPNG("gradient.png") 
g <- rasterGrob(img, interpolate=TRUE)
g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))
theme_set(theme_gray(base_size = 20))
market_impact <- 
  ggplot(sfret[k<=120],aes(x=k,colour=class))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  scale_x_continuous( breaks=seq(0,120,by=30))+
  scale_y_continuous( breaks=seq(-2e-4,3e-4,by=5e-5))+
  theme(plot.background = element_rect(fill = "#C0C0C0", colour = "#111111"),
        axis.text = element_text(colour="black",size=18),
        legend.background = element_rect(fill = "#A0A0A0", colour = "#333333"),
        panel.grid=element_line(linetype="dotted",colour="#00FFFF"))+
  geom_vline(xintercept=seq(0, 120, by=30),linetype="dotted",colour="#00FFFF")+
  geom_hline(yintercept=seq(-1e-4, 1e-4, by=5e-05),linetype="dotted",colour="#0088D8")+
 
  xlab("Time (in sec)") +
  ylab("Relative price change") +
  ggtitle("Price impact for CL contracts in 2014")+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.9), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.9),
                  fill=class), alpha=0.3, colour=NA)+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.99), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.99),
                  fill=class), alpha=0.3, colour=NA)+
  geom_line(aes(y=avg),size=0.8)+
  scale_fill_manual(values=c("#00FF00","#0055FF","#FF3333","#FFFF00"))+
  scale_color_manual(values=c("#00FF00","#0055FF","#FF3333","#FFFF00"))

market_impact
ggsave(market_impact,file="MIplots/algo+side.jpg",height=10,width=15)



















market_impact <- 
  ggplot(fret[bsize%in%c(1,2,3,4,5)],aes(x=k,colour=bsize))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  theme(panel.background = element_rect(fill = "#333333", colour = "#111111"),panel.grid=element_line(linetype="dotted",colour="#00FFFF"))+
  geom_vline(xintercept=seq(0, 180, by=30),linetype="dotted",colour="#2794F3")+
  geom_hline(yintercept=seq(-1*10^-5, 2*10^-4, by=1e-05),linetype="dotted",colour="#2794F3")+
  geom_line(aes(y=avg),size=0.8)+
  xlab("Time (in sec)") +
  ylab("Relative price change (Pt%P0)") +
  ggtitle("Price impact for CL contracts in 2014")+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.9), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.9),
                  fill=bsize), alpha=0.3, colour=NA)+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.99), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.99),
                  fill=bsize), alpha=0.3, colour=NA)
market_impact
ggsave(market_impact,file="MIplots/market_impact3.bmp",height=8,width=15)

market_impact <- ggplot(fret,aes(x=k,colour=side))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  theme(panel.background = element_rect(fill = "#333333", colour = "#111111"),panel.grid=element_line(linetype="dotted",colour="#00FFFF"))+
  geom_vline(xintercept=seq(0, 180, by=30),linetype="dotted",colour="#00FFFF")+
  geom_hline(yintercept=seq(-6*10^-5, 6*10^-5, by=1e-05),linetype="dotted",colour="#0088D8")+
  geom_line(aes(y=avg),size=0.8)+
  xlab("Time (in sec)") +
  ylab("Relative price change (Pt%P0)") +
  ggtitle("Price impact for CL contracts in 2014")+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.9), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.9),
                  fill=side), alpha=0.3, colour=NA)+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.99), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.99),
                  fill=side), alpha=0.3, colour=NA)+
  scale_fill_manual(values=c("#5FAFDF","#5FAFDF"))+
  scale_color_manual(values=c("green","red"))
  
ggsave(market_impact,file="MIplots/market_impact1.bmp",height=8,width=15)
#############################################
data[,ps:=p]
data[side=='SELL',ps:=-p]
data[,cumps:=cumsum(ps),by=list(ord_id)]

data <- data[,list(avg=mean(cumps),sigma=sd(cumps),.N),by=list(sym,k)]





































