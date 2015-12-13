# We will use the data from finvoz.com to get our data and also Yahoo Finance for daily prices.

# Installing the libraries.

install.packages("zoo")
install.packages("plyr")
install.packages("reshape2")

# Load the libraries into the R workspace.

library(XML)
library(ggplot2)
library(plyr)
library(reshape2)
library(zoo) # Allow use to calculate moving averages.

setwd("~/R_projects/Practical_DataScienceTools_Cookbook_recipes/charts")

getwd()

# Since the website turned private, we now have a static data
# that was pulled by the author in 2014.

finviz <- read.csv("finviz.csv", header=T, stringsAsFactors = F)

head(finviz)
finviz[finviz$Ticker=='AAPL',]

summary(finviz$Price)

# Lot of the times the numeric data has % signs which make the engine think
# it is string, so we will create a generic clean_numeric function
# and using gsub will perform a cleanup exercise.

clean_numeric <- function(s) {
  s <- gsub("%|\\$|,|\\(","",s)
  s <- as.numeric(s)
}

# Next we will apply this function to our field columns.

finviz <- cbind(finviz[,1:6],apply(finviz[,7:68],2,clean_numeric))

head(finviz[,7])

# Let's now inspect the data and get a visual sense of what is a high
# stock price, what is low price and where the most stock falls.

hist(finviz$Price[finviz$Price <= 200], breaks = 100, main="Price Distribution", xlab="Price")

# Let us now get the average price per sector and see how they compare.

sector_avg_price <- aggregate(Price~Sector, data=finviz, FUN="mean")
colnames(sector_avg_price)[2] <- "Sector_Average_Price"

ggplot(sector_avg_price, aes(x=Sector, y=Sector_Average_Price, fill=Sector)) +
  geom_bar(stat="identity") + ggtitle("Sector Average Price")

# Looks like there is some outlier in the Finance sector, that is causing 
# the chart to look a little higher for that sector. Let's dig deeper.

industry_avg_price <- aggregate(Price~Sector+Industry, data=finviz, FUN="mean")
industry_avg_price <- industry_avg_price[order(industry_avg_price$Sector, industry_avg_price$Industry),]
colnames(industry_avg_price)[3] <- "Industry_Avg_Price"
head(industry_avg_price)


# Isolate the industries in the Financial sector

industry_chart <- subset(industry_avg_price, Sector=="Financial")
head(industry_chart)

# Plot this out on a ggplot

ggplot(industry_chart, aes(x=Industry, y=Industry_Avg_Price, fill=Industry)) +
  geom_bar(stat="identity") + ggtitle("Financial Industry average price") + theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

# So we see the outlier, we then drill further into it.

company_chart <- subset(finviz, Industry=="Property & Casualty Insurance")
head(company_chart)
ncol(company_chart)

ggplot(company_chart, aes(x=Company, y=Price, fill=Company)) + geom_bar(stat="identity") + theme(legend.position="none")
+ theme(axis.text.x = element_text(angle=90, hjust = 1)) + ggtitle("Company Average Prices")


# It's the berkshire hathway where the stock price is 172,000 per share.
# Let's remove this outlier from our dataset, since it is such a big outlier.

finviz <- subset(finviz, Ticker!="BRK-A")

# And now we plot again the same metrics we did before.

sector_avg_price <- aggregate(Price~Sector, data=finviz, FUN="mean")
colnames(sector_avg_price)[2] <- "Sector_Average_Price"

ggplot(sector_avg_price, aes(x=Sector, y=Sector_Average_Price, fill=Sector)) +
  geom_bar(stat="identity") + ggtitle("Sector Average Price")

# Now we have harmonized the data to get rid of their outliers.

# Now let's do something more interesting of coming up with a valuation model.

# Ultimate goal is to arrive at a decision if the stock is overvalued or undervalued.

# We do relative valuation based on the other stock prices in its vicinity
# Creates a skinny table.
sector_avg <- melt(finviz, id="Sector")
head(sector_avg)
tail(sector_avg)

sector_avg <- subset(sector_avg, variable%in%c("Price", "P.E", "PEG", "P.S","P.B"))
head(sector_avg)
sector_avg <- (na.omit(sector_avg))
head(sector_avg)
sector_avg$value <- as.numeric(sector_avg$value)

# Next step is to cast the data to make it wide again.

sector_avg <- dcast(sector_avg, Sector~variable, mean)
head(sector_avg)
colnames(sector_avg)[2:6] <- c("SAvgPE","SAvgPEG","SAvgPS","SAvgPB","SAvgPrice")

# We will do now the exact same thing, but this time on the industry level.

industry_avg <- melt(finviz, id=c("Sector","Industry"))
head(industry_avg)
industry_avg <- subset(industry_avg, variable%in%c("Price", "P.E", "PEG", "P.S","P.B"))
industry_avg <- (na.omit(industry_avg))
industry_avg$value <- as.numeric(industry_avg$value)
industry_avg <- dcast(industry_avg, Sector+Industry~variable, mean)
industry_avg <- (na.omit(industry_avg))
colnames(industry_avg)[3:7] <- c("IAvgPE","IAvgPEG","IAvgPS","IAvgPB","IAvgPrice")

# Now we will add the sector and Industry average columns to our original finviz dataset.

finviz <- merge(finviz, sector_avg, by.x="Sector", by.y="Sector")
finviz <- merge(finviz, industry_avg, by.x=c("Sector","Industry"), by.y=c("Sector","Industry"))

head(finviz)

# It removed those stocks whose industry or sector averages was not observed.

# We will put 10 placeholder fields that contain all zeros. They willl 
# we used to track whether the stock is undervalued or overvalued, based on the 
# industry or sector average observed.

finviz$SPEUnder <- 0
finviz$SPEGUnder <- 0
finviz$SPSUnder <- 0
finviz$SPBUnder <- 0
finviz$SPriceUnder <- 0
finviz$IPEUnder <- 0
finviz$IPEGUnder <- 0
finviz$IPSUnder <- 0
finviz$IPBUnder <- 0
finviz$IPriceUnder <- 0

# Next we will create columns in finviz dataset and add 1's wheeeve their prices are undervalued.

finviz$SPEUnder[finviz$P.E<finviz$SAvgPE] <- 1
finviz$SPEGUnder[finviz$PEG<finviz$SAvgPEG] <- 1
finviz$SPSUnder[finviz$P.S<finviz$SAvgPS] <- 1
finviz$SPBUnder[finviz$P.B<finviz$SAvgPB] <- 1
finviz$SPriceUnder[finviz$Price<finviz$SAvgPrice] <- 1
finviz$IPEUnder[finviz$P.E<finviz$IAvgPE] <- 1
finviz$IPEGUnder[finviz$PEG<finviz$IAvgPEG] <- 1
finviz$IPSUnder[finviz$P.S<finviz$IAvgPS] <- 1
finviz$IPBUnder[finviz$P.B<finviz$IAvgPB] <- 1
finviz$IPriceUnder[finviz$Price<finviz$IAvgPrice] <- 1

# Finally we will sum these columns to create a new column, with the index value
# telling you on a scale of 0 to 10, how undervalued the stock is based on different imensions 
# we considered in this dataset.

finviz$RealValIndex <- apply(finviz[79:88],1,sum)

potentially_undervalued_stocks <- subset(finviz, RealValIndex>=8)

colnames(potentially_undervalued_stocks)

tail(potentially_undervalued_stocks[,c(4,89)])

potentially_undervalued_stocks[potentially_undervalued_stocks$RealValIndex==10,c(1,2,4,66)]

# Analyze the shortlisted stocks to see their past performance to decide whether to invest in them or not.

colnames(finviz)

target_stocks <- subset(finviz, Price>20 & Price<100 & Volume>10000 &
                          Country=="USA" &
                          EPS..ttm.>0 &
                          EPS.growth.next.year>0 &
                          EPS.growth.next.5.years>0 &
                          Total.Debt.Equity<1 & Beta<1.5 &
                          Institutional.Ownership<30 &
                          RealValIndex>8)

nrow(target_stocks)

# Now let's get the history of our stocks.

counter <- 0

for(symbol in target_stocks$Ticker){
  url <- paste0("http://ichart.finance.yahoo.com/table.csv?s=",symbol,"&a=08&b=7&c=1984&d=01&e=23&f=2014&g=d&ignore=.csv")
  stock <- read.csv(url)
  stock <- na.omit(stock)
  colnames(stock)[7] <- "AdjClose"
  stock[,1] <- as.Date(stock[,1])
  stock <- cbind(Symbol=symbol,stock)

  maxrow <- nrow(stock)-49
  ma50 <- cbind(stock[1:maxrow,1:2], rollmean(stock$AdjClose,50,align="right"))
  maxrow <- nrow(stock)-199
  ma200 <- cbind(stock[1:maxrow,1:2], rollmean(stock$AdjClose,200,align="right"))

  # Combine this with the original dataset.

  stock <- merge(stock, ma50, by.x=c("Symbol","Date"), by.y=c("Symbol","Date"), all.x=TRUE)
  colnames(stock)[9] <- "MovAvg50"
  stock <- merge(stock, ma200, by.x=c("Symbol","Date"), by.y=c("Symbol","Date"), all.x=TRUE)
  colnames(stock)[10] <- "MovAvg200"
  
  # Now let's plot a historical chart for each of the stock in our for loop.

  price_chart <- melt(stock[,c(1,2,8,9,10)], id=c("Symbol","Date"))

  qplot(Date, value, data=price_chart, geom="line", color=variable, main=paste(symbol,"Daily Stock Prices"), ylab="Price")
  ggsave(filename=paste0("stock_price_",counter,".png"))
  
  price_summary <- ddply(stock, "Symbol", summarise, open=Open[nrow(stock)], high=max(High), low=min(Low), close=AdjClose[1])
  
  if(counter==0) {
    stocks <- rbind(stock)
    price_summaries <- rbind(price_summary)
  } else {
    stocks <- rbind(stocks, stock)
    price_summaries <- rbind(price_summaries, price_summary)
  }
  counter <- counter+1
  
  # Graph all of our 6 stock data to see how they look like.
}

qplot(Date, AdjClose, data=stocks, geom="line", color=Symbol, main="Daily Stock Prices")
ggsave(filename=("stock_price_combined.png"))

# Graph the summaries of prices.

summary <- melt(price_summaries, id="Symbol")
ggplot(summary, aes(x=variable, y=value, fill=Symbol)) + geom_bar(stat="identity") + facet_wrap(~Symbol)
ggsave(filename=("stock_price_summaries.png"))
