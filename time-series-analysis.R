# Import Required Libraries
library(prophet)
library(lubridate)
library(ggplot2)
library(readr)

# Read Data from the .csv file
day <- read_csv("dataset/day.csv")
day$dteday <- ymd(day$dteday)
View(day) # To view the data
summary(day) # Summary of the dataset -

# Plot the Data
qplot(dteday,
      cnt,
      data = day,
      main = "Bike Rental in Washington DC (Day)")

# Data Preprocessing
## Create a dataframe using only dteday and cnt
ds <- day$dteday
y <- day$cnt
df <- data.frame(ds,y)
df$temp <- day$temp
df$humi <- day$hum

# Build the Forecasting Model
## Initiate Default Forecasting Model using prophet()
m <- prophet()

## Adding Holidays Regressor
### Because the Dataset from Washington DC
### BR- Brazil | ID - Indonesia | IN - India | CN - China | RU - Russia
m <- add_country_holidays(m, country_name = 'US')

## Adding temparature and holiday regressor
m <- add_regressor(m, 'temp')
m <- add_regressor(m, 'humi')

m <- fit.prophet(m, df)  

### View all the holidays
m$train.holiday.names

# Make Prediction using the Model
# Note :If you are performing the time series analysis, then you have remember to 
# that temperature humidity like things are not available, So we have generate by
# looking existing values
future <- make_future_dataframe(m, periods = 10) # Period of 10 days

x <- data.frame(df$temp)
colnames(x) <- 'temp'

x1 <- data.frame(df$temp)
colnames(x1) <- 'humi'

y <- data.frame(runif(10, 0.1,0.3)) # 0.1 and 0.3 are range to generate values <- 0.1 and 0.3 are get by observing the existing values in dataset
colnames(y) <- 'temp'

y1 <- data.frame(runif(10, 0.4,0.8)) # 0.1 and 0.3 are range to generate values <- 0.1 and 0.3 are get by observing the existing values in dataset
colnames(y1) <- 'humi'

future$temp <- rbind(x,y) # Bind all the data into one column <- temp data
future$humi <- rbind(x1,y1) # Bind all the data into one column <- humi data

future <- as.matrix(future)
colnames(future) <- NULL # Remove Existing COlumn names
colnames(future) <- c('ds', 'temp', 'humi') # Define new column names
future <- data.frame(future)
future$temp <- as.numeric(future$temp) # Convert to numeric form
future$humi <- as.numeric(future$humi) # Convert to numeric form
future$ds <- ymd(future$ds) # Convert to Date form

forecast <- predict(m,future)

# Plot the Output
plot(m, forecast)
dyplot.prophet(m, forecast)

# Forecast Component
prophet_plot_components(m, forecast)

# Evaluate the Model 
pred <- forecast$yhat[1:731]
actual <- df[,2]

plot(actual,pred)
abline(lm(pred~actual), col='red')

summary(lm(pred~actual))
