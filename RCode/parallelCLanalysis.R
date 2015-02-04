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

load("orders.RData")
load("filemanager.RData")
orders <- data.table(orders)
setkey(orders,ord_id)

## wiki data structure #### 

# Orders (Relevant) Metadata: colnames(orders)[c(2,3,4,5,6,7,8)]
#
# c("date","sym","time","size","limitpx","side","pricetype")

## ## compute market impact curves

## compute features ####

size_bins <- c(0,1,2,5,10,50,100,1000) # see exploraotry data analysis
orders[,bsize:=.bincode(size,size_bins)]
orders[,month:=format(orders$date,"%Y-%m")]

## compute average trajectories ####
features1 <- c("sym","month","side","bsize") 
features2 <- c("side")

features <- features1
allret <- data.table()
f <- 9

system.time(
  for(f in 1:9){
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

## save results
#saveRDS(allret,"allret1.RDS")
#saveRDS(allret,"allret2.RDS")

allret1 <- readRDS("allret1.RDS")
allret2 <- readRDS("allret2.RDS")

## Debug
nrow(allret2)
colnames(allret2)
allret2[k==1&side=="BUY"]



#fret[,side:=as.factor(side)]

# Plot results
img <- readPNG("gradient.png") 
g <- rasterGrob(img, interpolate=TRUE)
g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))
           
ggplot(fret[sym=='CLH4'],aes(x=k,colour=side))+
  #annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  #theme(panel.background = element_rect(fill = "#333333", colour = "#111111"),panel.grid=element_line(linetype="dotted",colour="#00FFFF"))+
  #geom_vline(xintercept=seq(0, 180, by=30),linetype="dotted",colour="#00FFFF")+
  #geom_hline(yintercept=seq(-6*10^-5, 6*10^-5, by=1e-05),linetype="dotted",colour="#0088D8")+
  geom_line(aes(y=avg),size=0.8)+
  xlab("Time (in sec)") +
  ylab("Price change (in USD)") +
  ggtitle("Price impact for CLH4")+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.9), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.9),
                  fill=side), alpha=0.3, colour=NA)+
  geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.99), 
                  ymin=ci.d(avg,sigma/sqrt(N),0.99),
                  fill=side), alpha=0.3, colour=NA)+
  scale_fill_manual(values=c("#5FAFDF","#5FAFDF"))+
  scale_color_manual(values=c("green","red"))
  

#############################################à
data[,ps:=p]
data[side=='SELL',ps:=-p]
data[,cumps:=cumsum(ps),by=list(ord_id)]

data <- data[,list(avg=mean(cumps),sigma=sd(cumps),.N),by=list(sym,k)]





































