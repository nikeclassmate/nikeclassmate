##Importing data sets from Yahoo finance

library(pdfetch)# data from yahoo finance
pdfetch_YAHOO("^NSEI")#importing of data
NIFTY_50_Data=pdfetch_YAHOO("^NSEI")#to store the value
View(NIFTY_50_Data)
tail(NIFTY_50_Data)
plot.ts(NIFTY_50_Data$`^NSEI.close`)
ITC=pdfetch_YAHOO("ITC.NS")#Import the ITC share price
View(ITC)
plot.ts(ITC$ITC.NS.close)
summary(ITC$ITC.NS.close)
#Another Method to import
library(quantmod)
getSymbols("^NSEI",from="2010-01-01")
View(NSEI)
getSymbols("^NSEI",from="2010-01-01",to="2026-07-07")
View(NSEI)
plot.ts(NSEI$NSEI.High)
#Multiple data set
library(pdfetch)
x=pdfetch_YAHOO(c("^NSEI","ITC.NS","SBIN.NS","RELIANCE.NS"),
                "close",from="2015-01-01",to="2026-08-05")
View(x)
data=read.delim("clipboard")
data

##########################
# Time series data
data("AirPassengers")
AP <- AirPassengers
str(AP)
head(AP)
ts(AP, frequency = 12, start=c(1949,1))
attributes(AP)
plot(AP)

# Log transform
AP <- log(AP)
plot(AP)

# Decomposition of additive time series
decomp <- decompose(AP)
decomp$figure
plot(decomp$figure,
     type = 'b',
     xlab = 'Month',
     ylab = 'Seasonality Index',
     col = 'blue',
     las = 2)
plot(decomp)



########################
library(readxl)

# file path
path <- "C:/Users/satyaban.sahoo/OneDrive - Manipal Academy of Higher Education/Documents/D Drive/Time Series Analysis/2026-27 3rd Sem MSc (BA)/Nifty 50.xlsx"

# read data
df <- read_excel(path)

# take first column as Date and second column as series
df <- df[, 1:2]
names(df) <- c("Date", "Value")
df$Date <- as.Date(df$Date)
df <- df[order(df$Date), ]
df <- na.omit(df)

###1.Original daily plot
plot(df$Date, df$Value, type = "l",
     main = "Nifty 50",
     xlab = "Date", ylab = "Value")

### 2.Smoothing: 5-day and 30-day moving averages
df$MA5  <- stats::filter(df$Value, rep(1/5, 5), sides = 2)
df$MA30 <- stats::filter(df$Value, rep(1/30, 30), sides = 2)

plot(df$Date, df$Value, type = "l",
     main = "Nifty 50 5-Day Moving Average",
     xlab = "Date", ylab = "Value")
lines(df$Date, df$MA5, col = "red", lwd = 2)

plot(df$Date, df$Value, type = "l",
     main = "Nifty 50 30-Day Moving Average",
     xlab = "Date", ylab = "Value")
lines(df$Date, df$MA30, col = "blue", lwd = 2)

### 3. Least squares trend
t <- 1:nrow(df)
fit <- lm(Value ~ t, data = df)
df$Trend_LS <- fitted(fit)

plot(df$Date, df$Value, type = "l",
     main = "Least Squares Trend",
     xlab = "Date", ylab = "Value")
lines(df$Date, df$Trend_LS, col = "darkgreen", lwd = 2)

### 4. Semi-averages
n <- nrow(df)
h <- floor(n / 2)

m1 <- mean(df$Value[1:h], na.rm = TRUE)
m2 <- mean(df$Value[(h + 1):n], na.rm = TRUE)

df$SemiAvg <- c(rep(m1, h), rep(m2, n - h))

plot(df$Date, df$Value, type = "l",
     main = "Semi-Averages Trend",
     xlab = "Date", ylab = "Value")
lines(df$Date, df$SemiAvg, col = "purple", lwd = 2)

library(readxl)

####Trend Decoposition#####
### 1. Read daily Nifty data
path <- "Nifty 50.xlsx"

df <- read_excel(path)
df <- df[, 1:2]
names(df) <- c("Date", "Value")

df$Date <- as.Date(df$Date)
df <- df[order(df$Date), ]
df <- na.omit(df)

plot(df$Date, df$Value, type = "l",
     main = "Daily Nifty 50",
     xlab = "Date", ylab = "Value")

# 2. Moving-average filters: short, medium, long horizons
# choose windows appropriate for daily data
short_n  <- 10    # ~2 weeks
medium_n <- 30    # ~1 month
long_n   <- 90    # ~1 quarter

df$MA_short  <- stats::filter(df$Value, rep(1/short_n,  short_n),  sides = 2)
df$MA_medium <- stats::filter(df$Value, rep(1/medium_n, medium_n), sides = 2)
df$MA_long   <- stats::filter(df$Value, rep(1/long_n,   long_n),   sides = 2)

# 3. Define components
# long-term: very smooth trend
df$Long_term <- df$MA_long

# medium-term: movements between medium and long trend
df$Medium_term <- df$MA_medium - df$MA_long
# short-term: high-frequency residual around medium trend
df$Short_term <- df$Value - df$MA_medium
# 4. Plot components
par(mfrow = c(3, 1))

plot(df$Date, df$Long_term, type = "l",
     main = "Long-Term Component (~90-Day Trend)",
     xlab = "Date", ylab = "Long-Term")

plot(df$Date, df$Medium_term, type = "l",
     main = "Medium-Term Component (~30 vs 90 Days)",
     xlab = "Date", ylab = "Medium-Term")

plot(df$Date, df$Short_term, type = "l",
     main = "Short-Term Component (Residual Around 30-Day Trend)",
     xlab = "Date", ylab = "Short-Term")
par(mfrow = 1)



##### Stationarity test###
library(tseries)
adf.test(df$Price)
pp.test(df$Price)
kpss.test(df$Price)
###Return Series
retnifty=diff(log(df$Price))
plot(retnifty)

##Convert the data in to ZOO series
library(zoo)
Y=read.zoo(data.frame(df),FUN =as.Date,format='%d%m%Y')
plot(Y)
retnifty=diff(log(Y))
plot(retnifty)


####ARIMA Manual Forecast
# Load libraries
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)
library(zoo)

# File path
path <- "C:/Users/satyaban.sahoo/OneDrive - Manipal Academy of Higher Education/Documents/D Drive/Time Series Analysis/2026-27 3rd Sem MSc (BA)/Nifty 50.xlsx"

# Read data
df <- read_excel(path)

# Clean data
df$Date <- as.Date(df$Date)
df$Price <- as.numeric(gsub(",", "", df$Price))
df <- df %>% arrange(Date) %>% na.omit()
# Create zoo series
zoo_price <- zoo(df$Price, order.by = df$Date)
# Convert to ts for ARIMA
Y <- ts(coredata(zoo_price))

# Plot actual series
plot(zoo_price, main = "Nifty 50", xlab = "Date", ylab = "Price", col = "blue", lwd = 2)

# Stationarity test
adf.test(Y)
pp.test(Y)
kpss.test(Y)
# First difference
retnifty <- diff(Y)

# Stationary test after difference
adf.test(retnifty)
pp.test(retnifty)
kpss.test(retnifty)
plot(retnifty)

# ACF and PACF
acf(retnifty, main = "ACF of First Differenced Series")
pacf(retnifty, main = "PACF of First Differenced Series")

# Manually estimate candidate ARIMA models
fit_110 <- Arima(Y, order = c(1, 1, 0))
fit_011 <- Arima(Y, order = c(0, 1, 1))
fit_111 <- Arima(Y, order = c(1, 1, 1))
fit_211 <- Arima(Y, order = c(2, 1, 1))

# Compare AIC
cat("AIC values:\n")
cat("ARIMA(1,1,0):", AIC(fit_110), "\n")
cat("ARIMA(0,1,1):", AIC(fit_011), "\n")
cat("ARIMA(1,1,1):", AIC(fit_111), "\n")
cat("ARIMA(2,1,1):", AIC(fit_211), "\n")

# Compare BIC
cat("BIC values:\n")
cat("ARIMA(1,1,0):", BIC(fit_110), "\n")
cat("ARIMA(0,1,1):", BIC(fit_011), "\n")
cat("ARIMA(1,1,1):", BIC(fit_111), "\n")
cat("ARIMA(2,1,1):", BIC(fit_211), "\n")

# Accuracy on fitted values
cat("Accuracy ARIMA(1,1,0):\n")
print(accuracy(fit_110))

cat("Accuracy ARIMA(0,1,1):\n")
print(accuracy(fit_011))

cat("Accuracy ARIMA(1,1,1):\n")
print(accuracy(fit_111))

cat("Accuracy ARIMA(2,1,1):\n")
print(accuracy(fit_211))

# Residual diagnostics for preferred model
checkresiduals(fit_110)

# Forecast future values
fc_110 <- forecast(fit_110, h = 500)

# Plot forecast
plot(fc_110, main = "ARIMA Forecast", xlab = "Time", ylab = "Price")

####
# Create future dates with the SAME length as forecast horizon
last_date <- max(df$Date)
future_dates <- seq.Date(from = last_date + 1, 
                         by = "day", length.out = length(fc_110$mean))

# Forecast data frame
fc_df <- data.frame(
  Date = future_dates,
  Forecast = as.numeric(fc_110$mean)
)
# Actual data frame
actual_df <- data.frame(
  Date = df$Date,
  Price = df$Price
)
# Plot actual and forecast
ggplot() +
  geom_line(data = actual_df, aes(x = Date, y = Price), color = "blue", 
            linewidth = 1) +
  geom_line(data = fc_df, aes(x = Date, y = Forecast), color = "red", 
            linewidth = 1) +
  labs(
    title = "Actual Series with ARIMA Forecast",
    x = "Date",
    y = "Price"
  ) +
  theme_minimal()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title   = element_text(hjust = 0.5)
  )

# Save forecast
write.csv(data.frame(Forecast = as.numeric(fc_111$mean)),
          "nifty50_arima_forecast.csv",
          row.names = FALSE)


##############Auto ARIMA##########
# Load libraries
library(readxl)
library(dplyr)
library(zoo)
library(forecast)
library(ggplot2)

# Read the Excel file
data <- read_excel("C:/Users/satyaban.sahoo/OneDrive - Manipal Academy of Higher Education/Documents/D Drive/Time Series Analysis/2026-27 3rd Sem MSc (BA)/Nifty 50.xlsx")

# Clean data
data$Date  <- as.Date(data$Date)
data$Price <- as.numeric(gsub(",", "", data$Price))
data <- data %>%
  arrange(Date) %>%
  na.omit()

# Create zoo series
zoo_price <- zoo(data$Price, order.by = data$Date)

# Plot zoo series
plot(zoo_price,
     main = "Nifty 50 Daily Price (Zoo Series)",
     xlab = "Date",
     ylab = "Price",
     col  = "blue",
     lwd  = 2)

# Convert to ts for ARIMA fitting
# (you can drop frequency = 7 if you don't want to assume weekly seasonality)
ts_price <- ts(coredata(zoo_price))

# Fit ARIMA
fit_full <- auto.arima(ts_price)
summary(fit_full)

# Forecast 200 periods
h <- 200
fc_full <- forecast(fit_full, h = h)

# Create future dates (length must equal h)
last_date    <- max(index(zoo_price))
future_dates <- seq.Date(from = last_date + 1,
                         by   = "day",
                         length.out = h)

# Combine forecast data with dates
fc_df <- data.frame(
  Date     = future_dates,
  Forecast = as.numeric(fc_full$mean)
)

# Plot using ggplot so x-axis shows dates
ggplot() +
  geom_line(data = data,
            aes(x = Date, y = Price),
            color     = "black",
            linewidth = 1) +
  geom_line(data = fc_df,
            aes(x = Date, y = Forecast),
            color     = "red",
            linewidth = 1)+
  labs(
    title = "Out-of-sample ARIMA Forecast",
    x     = "Date",
    y     = "Price"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title   = element_text(hjust = 0.5)
  )


############## In Sample Forecasting ############
# Load libraries
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(zoo)
library(tidyr)

##1.Import Data
data <- read_excel("C:/Users/satyaban.sahoo/OneDrive - Manipal Academy of Higher Education/Documents/D Drive/Time Series Analysis/2026-27 3rd Sem MSc (BA)/Nifty 50.xlsx")

### 2. Clean data
data$Date <- as.Date(data$Date)
data$Price <- as.numeric(gsub(",", "", data$Price))
data <- data %>%
  arrange(Date) %>%
  na.omit()

### 3. Create zoo series
zoo_price <- zoo(data$Price, order.by = data$Date)

### 4. Split into 80% estimation and 20% forecast
n <- length(zoo_price)
train_size <- floor(0.8 * n)

train_zoo <- zoo_price[1:train_size]

## [1:train_size] creates a sequence of integers from 1 
## up to the value of train_size
test_zoo <- zoo_price[(train_size + 1):n]

## train_size + 1 Starts from the first observation after the training data.
## If train_size = 80, then the starting index is 81.

# 5. Convert zoo to ts for ARIMA
train_ts <- ts(coredata(train_zoo))

# 6. Fit ARIMA on training sample
fit_train <- auto.arima(train_ts)
summary(fit_train)

# 7. Forecast 20% holdout period
fc_20 <- forecast(fit_train, h = length(test_zoo))

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




############################################
library(readxl)
WORLD_INDICES <- read_excel("~/D Drive/Time Series Analysis/World Indices.xlsx")
library(zoo)
View(WORLD_INDICES)
head(WORLD_INDICES)
tail(WORLD_INDICES)
library(zoo)
Y=read.zoo(data.frame(WORLD_INDICES),FUN =as.Date,format='%d%m%Y')
View(Y)
Y=log(Y)
sd(Y$USA)
var(Y$USA)
library(moments)
skewness(Y$USA)
kurtosis(Y$USA)
summary(Y)
plot(Y)
write.csv(summary(Y),("Summary_world.csv"))
k = ncol(Y)
NAMES = colnames(Y)
split = 2
par(mfrow = c(ceiling(k/split),split), oma = c(0.5,1,0,0) + 0.1, 
    mar = c(0.5,1,1,0) + .5, mgp = c(3, 0.5, 0))

for (i in 1:k) {
  plot(Y[,i], type="l",las=1,xaxs="i",col="#0746ab",main=NAMES[i],
       tck=-0.01,yaxs="i")
  lines(Y[,i], col="#0746ab")
  box()
}
library(FinTS)
library(tseries)
adf.test(Y$USA)
adf.test(Y$UK)
adf.test(Y$Japan)
adf.test(Y$Brazil)
adf.test(Y$China)
adf.test(Y$India)
adf.test(Y$South.Africa)
library(urca)
kpss.test(Y$USA)
###Return Series
retusa=diff(log(Y$USA))
retuk=diff(log(Y$UK))
retjapan=diff(log(Y$Japan))
retbrazil=diff(log(Y$Brazil))
retchina=diff(log(Y$China))
retindia=diff(log(Y$India))
retsa=diff(log(Y$South.Africa))
adf.test(retusa)
adf.test(retuk)
adf.test(retjapan)
adf.test(retbrazil)
adf.test(retchina)
adf.test(retindia)
adf.test(retsa)

YY = cbind.zoo(retusa,retuk,retjapan,
               retbrazil,retchina,retindia,retsa)
plot(YY)
library(FinTS)
library(tseries)
plot(Y)

##Optimal Lag
library(vars)
library(mFilter)
lagselect<-VARselect(YY,lag.max = 10,type = "const")
lagselect
lagselect$selection
####
library(tidyverse)
library(forecast)
Jotest<-ca.jo(Y,type="trace",ecdet = "trend",K=2)##Trace Stat
Jotest
summary(Jotest)

library(zoo)
Y=read.zoo(data.frame(SECTOR_WISE),FUN =as.Date,format='%d%m%Y')
View(Y)
mean(Y$Healthcare)
median(Y$Healthcare)
min(Y$Healthcare)
max(Y$Healthcare)
range(Y$Healthcare)
quantile(Y$Healthcare)
sd(Y$Healthcare)
var(Y$Healthcare)
library(moments)
skewness(Y$Healthcare)
kurtosis(Y$Healthcare)
plot(Y$Healthcare)
summary(Y)
plot(Y)
write.csv(summary(Y),("Summary_Sector.csv"))
k = ncol(Y)
NAMES = colnames(Y)
split = 2
par(mfrow = c(ceiling(k/split),split), oma = c(0.5,1,0,0) + 0.1, mar = c(0.5,1,1,0) + .5, mgp = c(3, 0.5, 0))

for (i in 1:k) {
  plot(Y[,i], type="l",las=1,xaxs="i",col="#0746ab",main=NAMES[i],tck=-0.01,yaxs="i")
  lines(Y[,i], col="#0746ab")
  box()
}
library(FinTS)
library(tseries)
adf.test(Y$Healthcare)
adf.test(Y$Healthcare)
pp.test(Y$Healthcare)
library(urca)
kpss.test(Y$Healthcare)
###Return Series
retauto=diff(log(Y$Auto))
retbank=diff(log(Y$Bank))
retfinancial=diff(log(Y$FS))
retfmcg=diff(log(Y$FMCG))
rethealthcare=diff(log(Y$Healthcare))
YY = cbind.zoo(retauto,retbank,retfinancial,retfmcg,rethealthcare)
plot(YY$retauto)
library(FinTS)
library(tseries)
adf.test(YY$rethealthcare)
adf.test(YY$retauto)
adf.test(YY$retbank)
adf.test(YY$retfinancial)
adf.test(YY$retfmcg)
pp.test(YY$Healthcare)
kpss.test(YY$retauto)
plot(Y)
##Optimal Lag
library(vars)
library(mFilter)
lagselect<-VARselect(YY,lag.max = 10,type = "const")
lagselect
lagselect$selection
####



library(tidyverse)
library(forecast)
Jotest<-ca.jo(Y,type="trace",ecdet = "trend",K=2)##Trace Stat
Jotest
summary(Jotest)
##VECM
library(urca)
model1=VECM(Y,lag = 2, r=1,estim = 'ML')
summary(model1)

library(urca)
vecm_model=cajorls(Jotest,r = 1)
summary(vecm_model$rlm) #ECT

##Builing VAR
VAR1=VAR(YY,lag.max = 2,type = "const",season = NULL,exog=NULL)
summary(VAR1)
##Display of VAR Results
library(stargazer)
stargazer(VAR1[["varresult"]],type = 'text')
VAR2=VAR(YY,type = "const",lag.max = 2,ic="AIC")
VAR2
VAR2=VAR(data.frame(YY$retauto,retbank,retfinancial),type = "const",lag.max = 2,ic="AIC")
VAR2
########

##Stable model
roots(VAR1,modulus=TRUE) ##Values to be less than one
##Diagnostic Test for VAR Serial Correlation
Serial1=serial.test(VAR1,lags.pt = 10,type = "PT.asymptotic")
Serial1 ##Null: There is no Serial Correlation
##Diagnostic Test for VAR Heteroscedaticity
ARCH1=arch.test(VAR1,lags.multi = 10,multivariate.only = TRUE)
ARCH1 ##There is no ARCH effect
##Diagnostic Test for VAR Normality
NORM1=normality.test(VAR1,multivariate.only = TRUE)
NORM1 ##Residual are Normality Distributed
##TEsting for Structural Break Test
Stability1=stability(VAR1,type = "OLS-CUSUM")
Stability1
plot(Stability1)
#Granger Causality Test
Granger1=causality(VAR1,cause="retauto")
Granger1
Granger1=causality(VAR1,cause = "retbank")
Granger1
Granger1=causality(VAR1, cause = "retfmcg")
Granger1
Granger1=causality(VAR1, cause = "retfinancial")
Granger1
Granger1=causality(VAR1, cause = "rethealthcare")
Granger1
causality(VAR1,cause = "retauto")$Granger
###Impulse REsponse Function(IRF)
AUTOIRF=irf(VAR1,impulse = "retbank",response = "retauto",
            n.ahead = 10,boot = TRUE)
plot(AUTOIRF,ylab="retauto",main="Shock From retbank")

BANKIRF=irf(VAR1,impulse = "retauto",
            response = "retbank",n.ahead = 10,boot = TRUE)
plot(BANKIRF,ylab="retbank",mail="Shock From retauto")


IRF_ALL= irf(VAR1,impulse = colnames(YY),
             response = colnames(YY),
             n.ahead = 10,
             boot = TRUE,
             ci = 0.95,
             runs = 100
)
IRF_ALL
plot(IRF_ALL)

##Variance Decomposition
FEVDI=fevd(VAR1,n.ahead = 10)
plot(FEVDI)
FEVDI[["retauto"]]
FEVDI[["retbank"]]
FEVDI[["retfmcg"]]
FEVDI[["rethealthcare"]]
FEVDI[["retfinancial"]]
##VAR Forecasting
forecast=predict(VAR1,n.ahead = 10,cl=0.95)
fanchart(forecast,names = "retauto")
fanchart(forecast,names = "retbank")




