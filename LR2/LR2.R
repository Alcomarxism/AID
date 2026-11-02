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
y_fact_1 <- tsData[df_trend$t]
y_fit_1 <- fitted(fit1)
RSS_1 <- sum((y_fact_1 - y_fit_1)^2) 
TSS_1 <- sum((y_fact_1 - mean(y_fact_1))^2) 
R2_1 <- 1 - (RSS_1 / TSS_1)
cat("Коэффициент детерминации (R^2) для Модели 1:", R2_1, "\n")

windows()
sp1 <- spec.pgram(residuals(fit1),detrend = FALSE, log = "no", fast = FALSE, pad =FALSE, taper = 0, plot = TRUE)
windows()
tsAcf1 <- acf(residuals(fit1), lag.max = 15, plot=TRUE, na.action = na.contiguous,demean = TRUE)
windows()
tsPacf1 <- pacf(residuals(fit1),lag.max = 15, plot = TRUE)

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
y_fact_2 <- tsData[df_trend$t]
y_fit_2 <- fitted(fit1) + fitted(fit2)[df_trend$t]
RSS_2 <- sum((y_fact_2 - y_fit_2)^2)
TSS_2 <- sum((y_fact_2 - mean(y_fact_2))^2)
R2_2 <- 1 - (RSS_2 / TSS_2)
cat("Коэффициент детерминации (R^2):", R2_2, "\n")

windows()
sp1 <- spec.pgram(residuals(fit2),detrend = FALSE, log = "no", fast = FALSE, pad =FALSE, taper = 0, plot = TRUE)
windows()
tsAcf1 <- acf(residuals(fit2), lag.max = 15, plot=TRUE, na.action = na.contiguous,demean = TRUE)
windows()
tsPacf1 <- pacf(residuals(fit2),lag.max = 15, plot = TRUE)



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
model3_forecast_values <- trend_forecast + seasonal_forecast
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

