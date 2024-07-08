
#To reduce the volatility in our data, the hourly measured day-ahead electricity prices are averaged on a daily
#basis. In energy literature, the average of the 24 hourly prices is called the daily price, the daily
#spot price, or the baseload price.

#This Code was provided by Dr. Rebekka Buse

rm(list=ls())
library(xts)
setwd("...")

# load data: 
entsoe <- read.csv("Data.scv")

entsoe<-entsoe[,c(1,3,4)] # select date, Country, Day_ahead_price
entsoe[,1]<- as.POSIXct(as.character(entsoe[,1]), format="%Y-%m-%d %H:%M", tz="GMT") #transform date to POSIXct format

colnames(entsoe)[1]<-"DateTime"

#tzone(index(entsoe)) # no time zone attributed

# construct dataframe with prices for each country
entsoe_price_l<-split(entsoe[,1:3], entsoe$Country) # list of all dataframes, 1 dataframe for each country containing date, Country, Price
entsoe_price_l<-lapply(entsoe_price_l, function(x){colnames(x)[3]<-x[2,2]; return(x)}) #rename price column by country name
entsoe_price_lx<-lapply(entsoe_price_l, function(x){y<-xts(x, order.by = x$DateTime); return(y[,3])}) #transform date to xts

entsoe_price<-Reduce(function(x, y) merge.xts(x, y, all=TRUE),entsoe_price_lx) # merge country-wise lists to one dataframe

# ---> check if NAs are correct --> they are!
# sum(is.na(entsoe[,3]))
# sum(is.na(entsoe_price))
# na_l<-sapply(entsoe_price_l, function(x){ sum(is.na(x[,3]))} )
# sum(na_l)
# nrow_l<-sapply(entsoe_price_l, nrow)
# round(na_l/nrow_l*100, digits=2) # same values as Chris' missing data matrix
# sum(nrow(entsoe_price)-nrow_l)+sum(na_l)

# compute daily means
ep<-endpoints(entsoe_price, on = "days")
entsoe_prc_dly<-period.apply(entsoe_price, INDEX=ep, FUN=mean, na.rm=TRUE)
tclass(entsoe_prc_dly) <- "Date" # remove time, so that only date is shown

# Save the data in a CSV file
csv_file_path <- "entsoe_prc_dly_updated.csv"
write.csv(entsoe_prc_dly, file = csv_file_path, row.names = FALSE)


