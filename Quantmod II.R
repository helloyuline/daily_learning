#Quantmod III

#Creating trading rules.


library(tidyverse)
library(quantmod)


#Pull down Stock Data
TSLA <- getSymbols("TSLA", auto.assign = F)

TSLA <- getSymbols("TSLA", src = "yahoo")
chartSeries(TSLA) 
addMACD()
addBBands()
sessionInfo("quantmod")
#Store Closing Price (adjusted for stock splits)

price <- AMZN$AMZN.Adjusted # close price 


#What is Lag
?Lag

#
r <- price/Lag(price) - 1
r

#remove scientific notation

options(scipen = 9999)

#Look at Histogram of Percentage Change

hist(r, breaks = 30, main = "Histogram of Lagged Returns %")

# % price change
delta <-0.0075#threshold
signal <-c(0) # first date has no signal

#Loop over all trading days (except the first)
for (i in 2: length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else
    signal[i]<- 0
}


#Reclassify signal to an xts object (tying it to a date)
signal <- reclass(signal,price)

#Chart the Series
chartSeries(AMZN,
            type = 'line',
            subset="2017-08::2017-09-15",
            theme=chartTheme('white'))

#Add signal line to see how the model performs
addTA(signal,type='S',col='red')



