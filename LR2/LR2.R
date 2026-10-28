library(tseries)
library(forecast)

data <- read.table("data.csv",header=TRUE, sep=";",fileEncoding = "windows-1251")
tsData <- ts(data[,"вар.15"],frequency=12, start=c(2010,1))
plot.ts(tsData)