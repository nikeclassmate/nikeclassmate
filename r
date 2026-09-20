#1. Import / inspect data
library(pdfetch)
# Download NIFTY data
NIFTY <- pdfetch_YAHOO("^NSEI")
head(NIFTY)
tail(NIFTY)
str(NIFTY)

#For Excel NIFTY:
library(readxl)
df <- read_excel("YOUR_FILE_PATH.xlsx")
View(df)
head(df)
tail(df)

#2. Time-series object
data("AirPassengers")
str(AirPassengers)
head(AirPassengers)
AP <- ts(AirPassengers,frequency = 12,start = c(1949, 1))
attributes(AP)

#3. Plot the time series
# Plot AirPassengers
plot(AP,main = "AirPassengers",xlab = "Year",ylab = "Passengers")

#For NIFTY:
plot(df$Date, df$Price,type = "l",main = "NIFTY",xlab = "Date",ylab = "Price")

# Log transform
log_AP <- log(AP)
# Plot log transformed series
plot(log_AP,main = "Log Transformed AirPassengers")

# Decompose time series
decomp <- decompose(AP)
# Seasonal component
decomp$figure
# Plot decomposition
plot(decomp)

# 5-day moving average
df$MA5 <- stats::filter(df$Price,rep(1/5, 5),sides = 2)

# Plot
plot(df$Date, df$Price,type = "l",main = "NIFTY with 5-Day Moving Average",xlab = "Date",ylab = "Price")
lines(df$Date, df$MA5)

# 30-day moving average
df$MA30 <- stats::filter(df$Price,rep(1/30, 30),sides = 2)

# Plot
plot(df$Date, df$Price,type = "l",main = "NIFTY with 30-Day Moving Average",xlab = "Date",ylab = "Price")
lines(df$Date, df$MA30)

#LEAST-SQUARES TREND
# Create time variable
t <- 1:nrow(df)
trend_model <- lm(Price ~ t, data = df)
summary(trend_model)
df$Trend <- fitted(trend_model)
plot(df$Date, df$Price,type = "l",main = "Least Squares Trend",
     xlab = "Date",ylab = "Price")
lines(df$Date, df$Trend)

#SEMI-AVERAGE METHOD
n <- nrow(df)
mid <- floor(n / 2)
first_half <- df$Price[1:mid]
second_half <- df$Price[(mid + 1):n]

mean_first <- mean(first_half, na.rm = TRUE)
mean_second <- mean(second_half, na.rm = TRUE)
mean_first
mean_second

#MOVING-AVERAGE FILTERS — SHORT / MEDIUM / LONG
short_n <- 10
medium_n <- 30
long_n <- 90
# Short-term moving average
df$MA_short <- stats::filter(df$Price,rep(1/short_n, short_n),sides = 2)
# Medium-term moving average
df$MA_medium <- stats::filter(df$Price,rep(1/medium_n, medium_n),sides = 2)
# Long-term moving average
df$MA_long <- stats::filter(df$Price,rep(1/long_n, long_n),sides = 2)

# Long-term component
df$Long_term <- df$MA_long

# Medium-term component
df$Medium_term <- df$MA_medium - df$MA_long

# Short-term component
df$Short_term <- df$Price - df$MA_medium

# Plot three components
par(mfrow = c(3,1))

plot(df$Date, df$Long_term,type = "l",main = "Long-Term Component")
plot(df$Date, df$Medium_term,type = "l",main = "Medium-Term Component")
plot(df$Date, df$Short_term,type = "l",main = "Short-Term Component")

par(mfrow = c(1,1))











#1. Import and clean NIFTY data
# Load packages
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)
library(zoo)

# Import Excel file
df <- read_excel("YOUR_FILE_PATH.xlsx")
View(df)

# Keep required columns
df <- df[, 1:2]

# Rename columns
names(df) <- c("Date", "Price")

# Convert Date
df$Date <- as.Date(df$Date)

# Convert Price to numeric
df$Price <- as.numeric(gsub(",", "", df$Price))

# Arrange by date
df <- df[order(df$Date), ]

# Remove missing values
df <- na.omit(df)

# Check data
head(df)
tail(df)


#2. Plot the original series
# Plot NIFTY
plot(df$Date, df$Price,
     type = "l",
     main = "NIFTY Price",
     xlab = "Date",
     ylab = "Price")

# Create zoo time series
zoo_price <- zoo(df$Price, order.by = df$Date)

# Convert to ts
Y <- ts(coredata(zoo_price))

# Plot
plot(Y,
     main = "NIFTY Time Series",
     ylab = "Price",
     xlab = "Time")

#3. Stationarity tests
# ADF test
adf.test(Y)
# PP test
pp.test(Y)
# KPSS test
kpss.test(Y)
Interpretation
# ADF / PP:
# p < 0.05 → stationary
# KPSS:
# p < 0.05 → non-stationary

#4. Differencing
# First difference
retnifty <- diff(Y)

# Plot differenced series
plot(retnifty,main = "Differenced NIFTY",ylab = "Difference")

#Testing again
# ADF
adf.test(retnifty)
# PP
pp.test(retnifty)
# KPSS
kpss.test(retnifty)
# If first difference becomes stationary:
# d = 1

#5. ACF and PACF
# ACF
Acf(retnifty,
    main = "ACF of NIFTY")

# PACF
Pacf(retnifty,
     main = "PACF of NIFTY")
# ACF → helps identify q (MA order)
# PACF → helps identify p (AR order)

#6. Build candidate ARIMA models
# ARIMA(1,1,0)
fit_110 <- Arima(Y, order = c(1,1,0))
# ARIMA(0,1,1)
fit_011 <- Arima(Y, order = c(0,1,1))
# ARIMA(1,1,1)
fit_111 <- Arima(Y, order = c(1,1,1))
# ARIMA(2,1,1)
fit_211 <- Arima(Y, order = c(2,1,1))

#7. Compare models
# AIC
AIC(fit_110)
AIC(fit_011)
AIC(fit_111)
AIC(fit_211)

# BIC
BIC(fit_110)
BIC(fit_011)
BIC(fit_111)
BIC(fit_211)
# Lower AIC/BIC → generally preferred model

#8. Forecast accuracy
# Accuracy of each model
accuracy(fit_110)
accuracy(fit_011)
accuracy(fit_111)
accuracy(fit_211)
# MAE → average absolute error
# RMSE → penalizes larger errors more

#9. Residual diagnostics
# Check residuals
checkresiduals(fit_110)
# Ljung-Box p > 0.05 → no significant autocorrelation

#10. Forecast
# Forecast next 500 periods
fc_110 <- forecast(fit_110, h = 500)
# Plot forecast
plot(fc_110,main = "NIFTY ARIMA Forecast")

#11. Auto ARIMA
# Automatically select ARIMA model
fit_full <- auto.arima(Y)
# Model details
summary(fit_full)

# Forecast
fc_auto <- forecast(fit_full, h = 200)
# Plot
plot(fc_auto,main = "Auto ARIMA Forecast")
# auto.arima() automatically searches for suitable p,d,q

#12. 80–20 Train-Test
# Number of observations
n <- length(zoo_price)

# 80% for training
train_size <- floor(0.8 * n)

# Training data
train_zoo <- zoo_price[1:train_size]

# Testing data
test_zoo <- zoo_price[(train_size + 1):n]

# Convert training data to ts
train_ts <- ts(coredata(train_zoo))

# Build Auto ARIMA using training data
fit_train <- auto.arima(train_ts)

# Forecast the test period
fc_20 <- forecast(fit_train,h = length(test_zoo))

# 8. Create holdout results
result_20 <- data.frame(
  Date = index(test_zoo),
  Actual = as.numeric(coredata(test_zoo)),
  Forecast = as.numeric(fc_20$mean)
)

# 9. Create full actual series data frame
full_actual <- data.frame(
  Date = index(zoo_price),
  Price = as.numeric(coredata(zoo_price))
)

# 10. Plot full actual series + 20% forecast
ggplot() +
  geom_line(data = full_actual, aes(x = Date, y = Price), color = "black", linewidth = 1) +
  geom_line(data = result_20, aes(x = Date, y = Forecast), color = "red", linewidth = 1)+
  labs(
    title = "Nifty 50: Full Actual Series with 20% ARIMA Forecast",
    x = "Date",
    y = "Price"
  ) +
  theme_minimal() +
  theme(
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# 11. Accuracy measures for holdout
mae_20 <- mean(abs(result_20$Actual - result_20$Forecast), na.rm = TRUE)
rmse_20 <- sqrt(mean((result_20$Actual - result_20$Forecast)^2, na.rm = TRUE))

cat("MAE:", mae_20, "\n")
cat("RMSE:", rmse_20, "\n")
















#1. Import World Indices
# Load packages
library(readxl)
library(zoo)
library(tseries)
library(vars)
library(urca)
library(moments)

# Import World Indices
WORLD_INDICES <- read_excel("YOUR_WORLD_INDICES_FILE.xlsx")

# Check data
View(WORLD_INDICES)
head(WORLD_INDICES)
tail(WORLD_INDICES)

#2. Convert to time series
# Convert data to zoo
Y <- read.zoo(data.frame(WORLD_INDICES),
              FUN = as.Date,
              format = "%d%m%Y")

#3. Log transformation
# Log transformation
Y <- log(Y)

#4. Descriptive statistics
# Standard deviation
sd(Y$USA)
# Variance
var(Y$USA)
# Skewness
skewness(Y$USA)
# Kurtosis
kurtosis(Y$USA)
# Summary
summary(Y$USA)
# Plot USA
plot(Y$USA,
     main = "USA Index",
     ylab = "Log Price")

#for plotting all
k = ncol(Y)
NAMES = colnames(Y)
split = 2
par(mfrow = c(ceiling(k/split),split))

for (i in 1:k) {
  plot(Y[,i], type="l",main=NAMES[i])
}

#5. Stationarity
# ADF tests
adf.test(Y$USA)
adf.test(Y$UK)
adf.test(Y$Japan)
adf.test(Y$Brazil)
adf.test(Y$China)
adf.test(Y$India)
adf.test(Y$SouthAfrica)
# KPSS 
kpss.test(Y$USA)
kpss.test(Y$UK)
kpss.test(Y$Japan)
kpss.test(Y$Brazil)
kpss.test(Y$China)
kpss.test(Y$India)
kpss.test(Y$SouthAfrica)

#6. Calculate returns
# Log returns
retusa <- diff(log(Y$USA))
retuk <- diff(log(Y$UK))
retjapan <- diff(log(Y$Japan))
retbrazil <- diff(log(Y$Brazil))
retchina <- diff(log(Y$China))
retindia <- diff(log(Y$India))
retsouthafrica <- diff(log(Y$SouthAfrica))

#7. Combine all return series
# Combine returns
YY <- cbind.zoo(
  retusa,
  retuk,
  retjapan,
  retbrazil,
  retchina,
  retindia,
  retsouthafrica
)
#Now YY contains multiple stationary return series.

#8. VAR Lag Selection
# Select appropriate lag
lagselect <- VARselect(
  YY,
  lag.max = 10,
  type = "const"
)
# View recommended lags
lagselect$selection

#9. Johansen Cointegration Test
# Johansen trace test
Jotest <- ca.jo(Y,type = "trace",ecdet = "trend",K = 2)
# Results
summary(Jotest)
# Johansen test checks for long-run cointegration

#10. VECM
#If Johansen test indicates cointegration:
model1 <- VECM(Y,lag = 2,r = 1,estim = "ML")
summary(model1)
# Converting Johansen result to VECM representation
vecm_model <- cajorls(Jotest,r = 1)
summary(vecm_model$rlm)
# ECT shows adjustment toward long-run equilibrium

#11. Build VAR
VAR1 <- VAR(YY,lag.max = 2,type = "const",season = NULL,exogen = NULL)
summary(VAR1)

#12. VAR with AIC
VAR2 <- VAR(YY,type = "const",lag.max = 2,ic = "AIC")
summary(VAR2)

#13. VAR stability
# Check roots
roots(VAR1, modulus = TRUE)
# All roots < 1 → VAR is stable

#14. Serial correlation
# Serial correlation test
serial.test(VAR1,lags.pt = 10,type = "PT.asymptotic")
# p > 0.05 → no significant serial correlation

#15. ARCH test
arch.test(VAR1,lags.multi = 10,multivariate.only = TRUE)
# p > 0.05 → no significant ARCH effect

#16. Normality test
normality.test(VAR1,multivariate.only = TRUE)
# p > 0.05 → fail to reject normality

#17. Structural stability
stability(VAR1,type = "OLS-CUSUM")

# Plot
plot(stability(VAR1,type = "OLS-CUSUM"))

#18. Granger causality
causality(VAR1,cause = "retauto")

#You can repeat:
causality(VAR1, cause = "retbank")
causality(VAR1, cause = "retfinancial")
causality(VAR1, cause = "retfmcg")
# p < 0.05 → evidence of Granger causality

#19. Impulse Response Function — IRF
# Shock from retbank → response of retauto
BANKIRF <- irf(VAR1,impulse = "retbank",response = "retauto",n.ahead = 10,boot = TRUE)

# Plot
plot(BANKIRF)

#Reverse direction:
# Shock from retauto → response of retbank
AUTOIRF <- irf(VAR1,impulse = "retauto",response = "retbank",n.ahead = 10,boot = TRUE)
plot(AUTOIRF)
# IRF shows how a shock affects another variable over time

#20. IRF for all variables
# IRF with 95% confidence interval
IRF_ALL <- irf(VAR1,n.ahead = 10,ci = 0.95,runs = 100)

# Plot
plot(IRF_ALL)

#21. FEVD
# Forecast Error Variance Decomposition
FEVD <- fevd(VAR1,n.ahead = 10)

# Plot
plot(FEVD)

#You can also inspect individual results:
FEVD$retauto
FEVD$retbank
FEVD$retfinancial
# FEVD shows the contribution of different shocks
# to forecast-error variance

#22. VAR Forecast
# Forecast next 10 periods
VAR_FORECAST <- predict(VAR1,n.ahead = 10,cl = 0.95)

# Plot forecast
fanchart(VAR_FORECAST)
