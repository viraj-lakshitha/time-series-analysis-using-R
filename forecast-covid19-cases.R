# Import required libraries
library(covid19.analytics)
library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)
library(readr)


#-------- FOR US --------#
# Data
tsc <- covid19.data(case = "ts-confirmed")
View(covid19.data(case = "ts-confirmed"))
tsc <- tsc %>% filter(Country.Region == 'US')
tsc <- data.frame(t(tsc))
tsc <<- cbind(row.names(tsc), data.frame(tsc,row.names = NULL))
colnames(tsc) <- c('Date','Confirmed')
tsc <- tsc[-c(1:4),]
tsc$Date <- ymd(tsc$Date)
tsc$Confirmed <- as.numeric(tsc$Confirmed)

str(tsc)

# Plot
qplot(Date, Confirmed, data = tsc,
      main = "Covid 19 - Confirmed Cases in US")
ds <- tsc$Date
y <- tsc$Confirmed
df <- data.frame(ds,y)

# Forecasting
m <- prophet(df)

# Future Prediction
future <- make_future_dataframe(m, periods = 28)
forecast <- predict(m, future)

# Plot forecast
plot(m,forecast)
dyplot.prophet(m, forecast)

# Forecast Components
prophet_plot_components(m, forecast)

# Model Performance
pred <- forecast$yhat[1:421]
actual <- m$history$y
plot(actual, pred)
abline(lm(pred~actual), col = 'red')
summary(lm(pred~actual))


# #------- SRI LANKA -------#
# # Data
# tsc <- covid19.data(case = "ts-confirmed")
# View(covid19.data(case = "ts-confirmed"))
# tsc <- tsc %>% filter(Country.Region == 'Sri Lanka')
# tsc <- data.frame(t(tsc))
# tsc <<- cbind(row.names(tsc), data.frame(tsc,row.names = NULL))
# colnames(tsc) <- c('Date','Confirmed')
# tsc <- tsc[-c(1:4),]
# tsc$Date <- ymd(tsc$Date)
# tsc$Confirmed <- as.numeric(tsc$Confirmed)
# 
# str(tsc)
# 
# # Plot
# qplot(Date, Confirmed, data = tsc,
#       main = "Covid 19 - Confirmed Cases in US")
# ds <- tsc$Date
# y <- tsc$Confirmed
# df <- data.frame(ds,y)
# 
# # Forecasting
# m <- prophet(df)
# 
# # Future Prediction
# future <- make_future_dataframe(m, periods = 28)
# forecast <- predict(m, future)
# 
# # Plot forecast
# plot(m,forecast)
# dyplot.prophet(m, forecast)
# 
# # Forecast Components
# prophet_plot_components(m, forecast)
# 
# # Model Performance 
# pred <- forecast$yhat[1:421]
# actual <- m$history$y
# plot(actual, pred)
# abline(lm(pred~actual), col = 'red')
# summary(lm(pred~actual))
# 
