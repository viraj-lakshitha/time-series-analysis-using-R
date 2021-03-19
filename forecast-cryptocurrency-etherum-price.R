# Import Required Libraries
library(prophet)
library(lubridate)
library(ggplot2)
library(readr)

# Data
data <- read.csv(file.choose(), header = T) # Use the ./dataset/ethereum-history-price.csv
head(data)

# Plot Data
qplot(Date, Close, data=data,
      main = "Ethereum closing prices 2016-2020")

# Log Transformation
ds <- data$Date
y <- log(data$Close)
df <- data.frame(ds,y)

# Forecast with Facebook's Prophet Package
m <- prophet(df)

# Future Forecast
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# Plot
plot(m, forecast)
dyplot.prophet(m, forecast) # Output given as the LOG value, So you have to take the exponential
prophet_plot_components(m, forecast)

# Model Performance
## yhat <- predicted values
pred <- forecast$yhat[1:1438]
actual <- m$history$y

plot(actual, pred)
abline(lm(pred~actual), col = 'red')
summary(lm(pred~actual)) # Accuracy <-- 0.9878
