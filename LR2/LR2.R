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

t <- 1:length(tsData)
#3.1
fit1 <- nls(y ~ k/(1+a*exp(-b*t)), data=na.omit(data.frame(y = as.numeric(tsDataComponents$trend),t = 1:length(tsDataComponents$trend))),start=c(k=1450,a=50,b=0.05))
k <- coef(fit1)["k"]
a <- coef(fit1)["a"]
b <- coef(fit1)["b"]
tsModel1 <- ts(k / (1 + a * exp(-b * t)), start = start(tsData), frequency = frequency(tsData))

windows()
plot.ts(tsData, main = "Модель 1", col = "black")
lines(tsModel1, col = "red")
legend("topleft", legend = c("Исходный ряд", "Модель"), col = c("black", "red"))

residuals1 <- tsData - tsModel1
acc1 <- accuracy(tsModel1,tsData)
cat("\nModel 1\n")
cat("k: ",k," a: ",a," b: ",b,":\n")
cat("Min error",min(residuals1),"\n")
cat("Max error",max(residuals1),"\n")
cat("Mean error",mean(residuals1),"\n")
cat("Std. dev.", sd(residuals1), "\n")
cat("MAE", acc1[1, "MAE"], "\n")
cat("MPE", acc1[1, "MPE"], "\n")
cat("MAPE", acc1[1, "MAPE"], "\n")
cat("RMSE", acc1[1, "RMSE"], "\n")
RSS_1 <- sum((tsData - tsModel1)^2) 
TSS_1 <- sum((tsData - mean(tsData))^2) 
R2_1 <- 1 - (RSS_1 / TSS_1)
cat("Коэффициент детерминации (R^2):", R2_1, "\n")

windows()
sp1 <- spec.pgram(residuals1,detrend = FALSE, log = "no", fast = FALSE, pad =FALSE, taper = 0, plot = TRUE)
windows()
tsAcf1 <- acf(residuals1, lag.max = 15, plot=TRUE, na.action = na.contiguous,demean = TRUE)
windows()
tsPacf1 <- pacf(residuals1,lag.max = 15, plot = TRUE)

#3.2
fit2 <- nls(y ~ A*sin((2 * pi / 12)*t)+B*cos((2 * pi / 12)*t), data=na.omit(data.frame(y = as.numeric(tsDataComponents$seasonal),t = 1:length(tsDataComponents$seasonal))),start=c(A=1,B=1))
A <- coef(fit2)["A"]
B <- coef(fit2)["B"]
tsModel2 <- ts(A*sin((2 * pi / 12)*t)+B*cos((2 * pi / 12)*t), start = start(tsData), frequency = frequency(tsData))+tsModel1

windows()
plot.ts(tsData, main = "Модель 2", col = "black")
lines(tsModel2, col = "red")
legend("topleft", legend = c("Исходный ряд", "Модель"), col = c("black", "red"))

residuals2 <- tsData - tsModel2
acc2 <- accuracy(tsModel2,tsData)
cat("\nModel 2\n")
cat("A: ",A," B: ",B,"\n")
cat("Min error",min(residuals2),"\n")
cat("Max error",max(residuals2),"\n")
cat("Mean error",mean(residuals2),"\n")
cat("Std. dev.", sd(residuals2), "\n")
cat("MAE", acc2[1, "MAE"], "\n")
cat("MPE", acc2[1, "MPE"], "\n")
cat("MAPE", acc2[1, "MAPE"], "\n")
cat("RMSE", acc2[1, "RMSE"], "\n")
RSS_1 <- sum((tsData - tsModel2)^2) 
TSS_1 <- sum((tsData - mean(tsData))^2) 
R2_1 <- 1 - (RSS_1 / TSS_1)
cat("Коэффициент детерминации (R^2):", R2_1, "\n")

windows()
sp2 <- spec.pgram(residuals2,detrend = FALSE, log = "no", fast = FALSE, pad =FALSE, taper = 0, plot = TRUE)
windows()
tsAcf2 <- acf(residuals2, lag.max = 15, plot=TRUE, na.action = na.contiguous,demean = TRUE)
windows()
tsPacf2 <- pacf(residuals2,lag.max = 15, plot = TRUE)

# Прогноз на 2026 год 
h <- 3
n_total <- length(tsData)
t_future <- (n_total + 1):(n_total + h)

trend_forecast <- k / (1 + a * exp(-b * t_future))
seasonal_forecast <- A * sin((2 * pi / 12) * t_future) + B * cos((2 * pi / 12) * t_future)

model2_forecast_values <- trend_forecast + seasonal_forecast
next_start <- time(tsData)[n_total] + 1/12
tsForecast2 <- ts(model2_forecast_values, start = next_start, frequency = 12)

windows()
ts_recent <- window(tsData, start = c(2024, 1))

plot(ts_recent,  main = "Прогноз на 2026 год",  col = "black",  lwd = 1.5, xlim = c(2024, 2027),  ylim = range(ts_recent, tsForecast2),  ylab = "Значение",  xlab = "Год")

last_time <- tail(time(tsData), 1)
last_val <- tail(tsData, 1)
full_times <- c(last_time, time(tsForecast2))
full_values <- c(last_val, as.numeric(tsForecast2))

lines(full_times, full_values, col = "red", lwd = 2, type = "o", pch = 16)
legend("topleft", legend = c("Факт (2024–2025)", "Прогноз (2026)"), col = c("black", "red"), lty = 1, pch = c(NA, 16))

