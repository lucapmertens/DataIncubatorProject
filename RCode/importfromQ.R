source("c:/r/qserver.r")
require(data.table)

# Import orders
c <- open_connection("localhost",5000)
orders <- execute(c,"cldords")
orders <- data.table(orders)

orders$algoparams <- sapply(orders$algoparams,function(x) return(x[[1]]))
orders$algoparams <- substr(orders$algoparams,7,100000)

setkey(orders,ord_id)
table(orders$algoparams)

saveRDS(orders,"orders.RDS")


#Import book data
bookdata <- execute(c,"mbookdata")
nrow(bookdata)

saveRDS(bookdata,"bookdata.RDS")
saveRDS(orders,"orders_new.RDS")
close_connection(c)


# Import pricedata

pricedata <- execute(c,"pricedata")

# saveRDS(pricedata,"pricedata1.RDS")
# saveRDS(pricedata,"pricedata2.RDS")
# saveRDS(pricedata,"pricedata3.RDS")
# saveRDS(pricedata,"pricedata4.RDS")
# saveRDS(pricedata,"pricedata5.RDS")
# saveRDS(pricedata,"pricedata6.RDS")
# saveRDS(pricedata,"pricedata7.RDS")
# saveRDS(pricedata,"pricedata8.RDS")
# saveRDS(pricedata,"pricedata9.RDS")

testdata <- readRDS("pricedata9.RDS")
sum(is.na(pricedata$p))
