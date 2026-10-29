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
lines(time(tsData)[df_trend$t], fitted(fit1), col = "red")
legend("topleft", legend = c("Исходный ряд", "Модель"), col = c("black", "red"))

errors1 <- tsData[df_trend$t] - fitted(fit1)
acc1 <- accuracy(fitted(fit1),tsData[df_trend$t])
cat("\nModel 1\n")
cat("Min error",min(errors1),"\n")
cat("Max error",max(errors1),"\n")
cat("Mean error",mean(errors1),"\n")
cat("Std. dev.", sd(errors1), "\n")
cat("MAE", acc1[1, "MAE"], "\n")
cat("MPE", acc1[1, "MPE"], "\n")
cat("MAPE", acc1[1, "MAPE"], "\n")
cat("RMSE", acc1[1, "RMSE"], "\n")

windows()
sp1 <- spec.pgram(residuals(fit1), plot = TRUE)
windows()
tsAcf1 <- acf(residuals(fit1), plot=TRUE)
windows()
tsPacf1 <- pacf(residuals(fit1), plot = TRUE)

#3.2
df_seasonal <- na.omit(data.frame(y = as.numeric(tsDataComponents$seasonal),t = 1:length(tsDataComponents$seasonal)))
fit2 <- nls(y ~ c*sin(d+f*t), data=df_seasonal,start=c(c=10,d=0,f=0.523))

windows()
plot(tsData, main = "Модель 2", col = "black")
lines(time(tsData)[df_trend$t], fitted(fit1)+fitted(fit2)[df_trend$t], col = "red")
legend("topleft", legend = c("Исходный ряд", "Модель"), col = c("black", "red"))

errors2 <- fitted(fit1)+fitted(fit2)[df_trend$t]-tsData[df_trend$t]
acc2 <- accuracy(fitted(fit1)+fitted(fit2)[df_trend$t],tsData[df_trend$t])
cat("\nModel 2\n")
cat("Min error",min(errors2),"\n")
cat("Max error",max(errors2),"\n")
cat("Mean error",mean(errors2),"\n")
cat("Std. dev.", sd(errors2), "\n")
cat("MAE", acc2[1, "MAE"], "\n")
cat("MPE", acc2[1, "MPE"], "\n")
cat("MAPE", acc2[1, "MAPE"], "\n")
cat("RMSE", acc2[1, "RMSE"], "\n")

windows()
sp2 <- spec.pgram(residuals(fit2), plot = TRUE)
windows()
tsAcf2 <- acf(residuals(fit2), plot=TRUE)
windows()
tsPacf2 <- pacf(residuals(fit2), plot = TRUE)


#3.3
residuals_model2 <- (tsData[df_trend$t] - (fitted(fit1) + fitted(fit2)[df_trend$t]))
p_order <- 3 
fit3 <- arima(residuals_model2, order = c(p_order, 0, 0))
ar_fitted_values <- residuals_model2 - residuals(fit3)
model3_fitted <- fitted(fit1) + fitted(fit2)[df_trend$t] + ar_fitted_values

acc3 <- accuracy(model3_fitted, tsData[df_trend$t])
errors3 <- tsData[df_trend$t] - model3_fitted
cat("\nModel 3\n")
cat("Min error",min(errors3),"\n")
cat("Max error",max(errors3),"\n")
cat("Mean error",mean(errors3),"\n")
cat("Std. dev.", sd(errors3), "\n")
cat("MAE", acc3[1, "MAE"], "\n")
cat("MPE", acc3[1, "MPE"], "\n")
cat("MAPE", acc3[1, "MAPE"], "\n")
cat("RMSE", acc3[1, "RMSE"], "\n")

windows()
plot(tsData, main = "Модель 3", col = "black")
lines(time(tsData)[df_trend$t], model3_fitted, col = "red")
legend("topleft", legend = c("Исходный ряд", "Модель"), col = c("black", "red"))

windows()
sp3 <- spec.pgram(residuals(fit3), plot = TRUE)
windows()
tsAcf3 <- acf(residuals(fit3), plot = TRUE)
windows()
tsPacf3 <- pacf(residuals(fit3), plot = TRUE)