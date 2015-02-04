## load packages ####
require(data.table)
require(reshape2)

## global options
options(digits.secs=3)

## load data ####
orders <- readRDS("orders.RDS")
load("filemanager.RData")

orders[order(time)]
min(diff(orders$time))

# bug in the data!
pricedata <- readRDS(filenames[9])

corrupted <- list()
corrupted$lines <- which(is.na(pricedata$p))
corrupted$id <- unique(pricedata[corrupted$lines,]$ord_id)
# corrupted data are all in the last chunk, and in last 3 dates 
corrupted$dates <- as.Date(c("2015-01-14","2015-01-15","2015-01-16"))

## local functions ####

myrep <- function(vec,ntimes){
  # repeat each value of vec ntimes
  return(as.vector(sapply(vec, function(x) rep(x,ntimes))))
}

## classify aggressive/passive
algopar <- unique(orders$algoparams)
algopar
passive <- algopar[c(1,2)]
aggressive <- algopar[c(3,4,5,6,7,8,10,11)]
passiverisk <- algopar[9]

orders <- orders[!ord_id%in%corrupted$id,]
orders <- orders[!algoparams%in%passiverisk,]

orders[algoparams%in%passive,balgo:="passive"]
orders[algoparams%in%aggressive,balgo:="aggressive"]
orders[,balgo:=factor(balgo)]

## classify size 
size_bins <- c(0,1,2,20,100000) # see exploraotry data analysis
orders[,bsize:=.bincode(size,size_bins)]
orders <- orders[bsize%in%c(1,2,3),]

## create recent history 



# parameters
par <- list()
par$Nlags <- 5

# event counting /day
orders <- orders[order(date,sym,time)]
orders[,dcount:=(1:.N),by=list(date,sym)]
orders <- orders[order(date,sym,time)]

events_pt <- which(orders$dcount>par$Nlags)

ord_id <- orders$ord_id[events_pt]

tau1 <- as.numeric(orders$time[events_pt]-orders$time[events_pt-1])  
tau2 <- as.numeric(orders$time[events_pt]-orders$time[events_pt-2])
tau3 <- as.numeric(orders$time[events_pt]-orders$time[events_pt-3])
tau4 <- as.numeric(orders$time[events_pt]-orders$time[events_pt-4])
tau5 <- as.numeric(orders$time[events_pt]-orders$time[events_pt-5])



orders$time[events_pt][3674]-orders$time[events_pt-1][3674]
events_pt[3674]  # 3709

test <- orders[3709+(-2:2),]
test

orders[3709+-2:2,]


as.numeric(orders[3799,time]-orders[3799-1,time])

type0=orders$balgo[events_pt]
type1=orders$balgo[events_pt-1]
type2=orders$balgo[events_pt-2]
type3=orders$balgo[events_pt-3]
type4=orders$balgo[events_pt-4]
type5=orders$balgo[events_pt-5]
  
size0=orders$bsize[events_pt]
size1=orders$bsize[events_pt-1]
size2=orders$bsize[events_pt-2]
size3=orders$bsize[events_pt-3]
size4=orders$bsize[events_pt-4]
size5=orders$bsize[events_pt-5]
  

mdata <- data.table(ord_id=ord_id,
                    tau1=tau1,tau2=tau2,tau3=tau3,tau4=tau4,tau5=tau5,
                    type0=type0,type1=type1,type2=type2,type3=type3,type4=type4,type5=type5,
                    size0=size0,size1=size1,size2=size2,size3=size3,size4=size4,size5=size5
                    )

rm(type0,type1,type2,type3,type4,type5,size0,size1,size2,size3,size4,size5,tau1,tau2,tau3,tau4,tau5)


y <- data.table(ord_id=ord_id)

fret <- data.table()
for(f in 1:9){
  ## compute returns from pricedata 
  pricedata <- readRDS(filenames[f])
  pricedata <- data.table(pricedata)
  p0 <- pricedata[k==0,p]
  ret <- pricedata[k>0&k<121,]
  #ret[,p:=p/p0]
  ret[,p:=cumsum(p),by=list(ord_id)]
  rm(pricedata)
  dret <- dcast.data.table(ret[k%in%c(1,10,20,30,60,90,120)],ord_id~k,value.var="p")
  setnames(dret,c("1","10","20","30","60","90","120"),c("dp1","dp10","dp20","dp30","dp60","dp90","dp120"))
  dret[,p0:=p0]
  dret[,c("tr1","tr10","tr20","tr30","tr60","tr90","tr120"):=list(dp1/p0*10000,dp10/p0*10000,dp20/p0*10000,dp30/p0*10000,dp60/p0*10000,dp90/p0*10000,dp120/p0*10000)]
  fret <- rbind(fret,dret)
  print(paste("chunk",as.character(f),"processed"))
  rm(ret,dret)
}
nrow(fret)
setkey(fret,ord_id)
#saveRDS(fret,"fret_raw.RDS") #fret with no order selection
fret <- fret[y]

writeRDS(mdata,"features.RDS")
writeRDS(fret,"y.RDS")


###################################################################### look into this later
mdata <- readRDS("features.RDS")
target <- readRDS("y.RDS")

sample = rbinom(10,2,.3) 
trainset = data[sample==0,] 
valset = data[sample==1,] 


mdata

names(mdata)





