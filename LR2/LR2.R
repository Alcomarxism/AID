library(tseries)
library(forecast)

#1
data <- read.table("data.csv",header=TRUE, sep=";",fileEncoding = "windows-1251")
tsData <- ts(data[,"вар.15"],frequency=12, start=c(2010,1))

#2
windows()
plot.ts(tsData)

tsDataComponents <- decompose(tsData)
windows()
plot(tsDataComponents)

#3.1
df_trend <- na.omit(data.frame(y = as.numeric(tsDataComponents$trend),t = 1:length(tsDataComponents$trend)))

fit1 <- nls(y ~ k/(1+a*exp(-b*t)), data=df_trend,start=c(k=1450,a=50,b=0.05))
windows()
plot(tsData, main = "Модель 1", col = "black")
lines(time(tsData)[df_trend$t], fitted(fit1), col = "red", lwd = 2)
legend("topleft", legend = c("Исходный ряд", "Модель"), col = c("black", "red"))

#3.2
df_seasonal <- na.omit(data.frame(y = as.numeric(tsDataComponents$seasonal),t = 1:length(tsDataComponents$seasonal)))
fit2 <- nls(y ~ c*sin(d+f*t), data=df_seasonal,start=c(c=10,d=0,f=0.523))

windows()
plot(tsData, main = "Модель 2", col = "black")
lines(time(tsData)[df_trend$t], fitted(fit1)+fitted(fit2)[df_trend$t], col = "red", lwd = 2)
legend("topleft", legend = c("Исходный ряд", "Модель"), col = c("black", "red"))