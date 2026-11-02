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

#Прогноз на 2026
h <- 3
n_total <- length(tsData)
t_future <- (n_total + 1):(n_total + h)
k_val <- coef(fit1)["k"]
a_val <- coef(fit1)["a"]
b_val <- coef(fit1)["b"]
trend_forecast <- k_val / (1 + a_val * exp(-b_val * t_future))

c_val <- coef(fit2)["c"]
d_val <- coef(fit2)["d"]
f_val <- coef(fit2)["f"]
seasonal_forecast <- c_val * sin(d_val + f_val * t_future)
ar_forecast <- forecast(fit3, h = h)
model3_forecast_values <- trend_forecast + seasonal_forecast + ar_forecast$mean
print(as.numeric(model3_forecast_values))

windows()
plot(window(tsData, start=c(2024, 1)), main = "Прогноз на 2026", col = "black", 
     xlim = c(2024, 2026.3), ylim = range(tsData, model3_forecast_values), ylab = "Value", xlab = "Year")

last_time <- tail(time(tsData), 1)
last_val <- tail(tsData, 1)
forecast_times <- seq(2026, by = 1/12, length.out = h)
full_times <- c(last_time, forecast_times)
full_values <- c(last_val, as.numeric(model3_forecast_values))

lines(full_times, full_values, col = "red", lwd = 2, type = "o", pch = 16)
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = 1, pch = c(1, 16))

