test <- .bincode(orders$size,c(0,1,2,5,10,50,100,1000))

table(test)/length(test)*100
                 
#1
sum(test==1)/length(test)*100 
sum(test==1)/sum(test)*100 
# 93.3% orders 82.6% shares
#2
sum(test==2)/length(test)*100 
sum(test*(test==2))/sum(test)*100 
# 4.6% orders 8.1% shares
#3
mask <- which(test>=3&test<=5) 
length(mask)/length(test)*100 
sum(test[mask])/sum(test)*100
# 1.7% orders 5.2% shares
#4
mask <- which(test>=6&test<=10) 
length(mask)/length(test)*100
sum(test[mask])/sum(test)*100
# 0.2% orders 1.4% shares
#5
mask <- which(test>=11&test<=50) 
length(mask)/length(test)*100
sum(test[mask])/sum(test)*100
# 0.1% orders 2% shares
#6
mask <- which(test>=51&test<=100) 
length(mask)/length(test)*100
sum(test[mask])/sum(test)*100
# 0.01% orders 0.6% shares
#7
mask <- which(test>100) 
length(mask)/length(test)*100
sum(test[mask])/sum(test)*100
# 0.002% orders 0.3% shares




