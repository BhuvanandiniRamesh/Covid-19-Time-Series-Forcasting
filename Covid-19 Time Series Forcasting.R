# Library
library(covid19.analytics)
library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)

# Data
tsdata <- covid19.data(case = 'ts-confirmed')
tsdata <- tsdata %>% filter(Country.Region == 'Norway')
tsdata <- data.frame(t(tsdata))
tsdata <- cbind(rownames(tsdata), data.frame(tsdata, row.names = NULL))
colnames(tsdata) <- c('Date', 'Confirmed')
tsdata <- tsdata[-c(1:39),]
tsdata$Date <- ymd(tsdata$Date)
tsdata$Confirmed <- as.numeric(tsdata$Confirmed)

# Plot
qplot(Date, Confirmed, data = tsdata,
      main = 'Confirmed cases in Norway')
ds <- tsdata$Date
y <- tsdata$Confirmed
df <- data.frame(ds, y)

# Forecasting
m <- prophet(df)

# Prediction
future <- make_future_dataframe(m, periods = 28)
forecast <- predict(m, future)

# Plot forecast
plot(m, forecast)
prophet_plot_components(m, forecast)

# Model performance
pred <- forecast$yhat[1:297]
actual <- m$history$y
plot(actual, pred)
abline(lm(pred~actual), col = 'red')
summary(lm(pred~actual))