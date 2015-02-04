#source("c:/r/qserver.r")
packs <- c("ggplot2","data.table","zoom","png","grid")
lapply(packs, require, character.only = TRUE);
rm(packs)

# Load Data
orders <- readRDS("orders.RDS")

## classify aggressive/passive
algopar <- unique(orders$algoparams)
algopar
passive <- algopar[c(1,2)]
aggressive <- algopar[c(3,4,5,6,7,8,10,11)]
passiverisk <- algopar[9]
rm(algopar)

orders[algoparams%in%passive,type:='passive']
orders[algoparams%in%aggressive,type:='aggressive']

rm(passive,aggressive,passiverisk)


#Set time
orders[,hour:=as.POSIXlt(time)$hour]
orders[,minute:=as.POSIXlt(time)$min]

#Make time blocks: create variable minutes rounded
b <- 5
for(t in c(1:(60/b))){
  orders[minute<(b*t)&minute>=((b*t)-5),minr:=((b*t)-b)]  
}

#Make Positct

orders[,temp:=as.character(minr)]
orders[minute<10,temp:=paste0('0',minr)]
orders[,hm:=paste0('1970-01-01 ',hour,':',temp,':00')]
orders[,temp:=NULL]

orders[,hm:=as.POSIXct(hm)]
head(orders)


# Plot Setups:

Sys.setlocale("LC_TIME", "English")
orders$hr = format(orders$time, format="%H")
vol.total <- sum(orders$size)

img <- readPNG("gradient.png") 
g <- rasterGrob(img, interpolate=TRUE)
g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))


## Time of the day

data <- orders[!is.na(type),list(vol=sum(size)),by=list(hour,type)]
theme_set(theme_gray(base_size = 15))

vol_hour <- 
  ggplot(data, aes(x=factor(hour),y=vol/vol.total,fill=as.factor(type)))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  scale_x_discrete( breaks=seq(0,23,by=1))+
  scale_y_continuous( breaks=seq(0, .17, by=.03))+
  theme(plot.background = element_rect(fill = "#C0C0C0", colour = "#111111"),
        axis.text = element_text(colour="black",size=18),
        legend.background = element_rect(fill = "#A0A0A0", colour = "#333333"),
        panel.grid=element_line(linetype="dotted",colour="#00FFFF"))+
  geom_vline(xintercept=seq(0, 24, by=1),linetype="dotted",colour="#00FFFF")+
  geom_hline(yintercept=seq(0, .17, by=.03),linetype="dotted",colour="#0088D8")+  
  scale_fill_manual("class",values=c("#CC0000","#009900"))+
  xlab('Hour')+ylab('Volume')+
  geom_bar(position="dodge",stat ="identity")
  
  #geom_line(size=0.8)
vol_hour


ggsave(vol_hour,file="MIplots/volhour.jpg",height=10,width=15)

nrow(orders$)

rm(data)


## Day of Weak

data <- orders[!is.na(type),list(vol=sum(size)),by=list(type,date)]
data$dow = as.factor(format(data$date, format="%a"))

data

ggplot(data[type=='passive'&vol<10000],aes(dow,vol/vol.total)) +
  geom_boxplot()+
  geom_smooth(aes(group = 1)) +
  scale_x_discrete(limits=c('Mon','Tue','Wed','Thu','Fri'))+ # provide explicit factor ordering
  xlab('day of week') + ylab('volume')

ggplot(data[type=='aggressive'&vol<10000],aes(dow,vol/vol.total)) +
  geom_boxplot()+
  geom_smooth(aes(group = 1)) +
  scale_x_discrete(limits=c('Mon','Tue','Wed','Thu','Fri'))+ # provide explicit factor ordering
  xlab('day of week') + ylab('volume')

rm(data)



## Plot results
img <- readPNG("gradient.png") 
g <- rasterGrob(img, interpolate=TRUE)
g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))

market_impact <- 
  ggplot(sfret,aes(x=k,colour=class))+
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
                  fill=class), alpha=0.3, colour=NA)+
  
  #geom_ribbon(aes(ymax=ci.u(avg,sigma/sqrt(N),0.99), 
  #                ymin=ci.d(avg,sigma/sqrt(N),0.99),
  #                fill=class), alpha=0.3, colour=NA)
  
  scale_fill_manual(values=c("#0000FF","#0088FF","#00FF88","#00FFFF"))+
  scale_color_manual(values=c("#0000FF","#0088FF","#00FF88","#00FFFF"))
