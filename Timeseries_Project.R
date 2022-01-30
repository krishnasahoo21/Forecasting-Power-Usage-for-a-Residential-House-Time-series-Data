## TIME SERIES PROJECT.

#Please install packages if required
library(forecast)
library(zoo)
library(corrplot)
library(psych)

## CREATE DATA FRAME. 
# By keeping the csv file in the same folder where the respective R file is kept
#we can directly set the working directory by following the steps:
#go to session -> Set Working Directory -> To Source File Location 

#Or

#setwd(Location of the csv file)

project.data <- read.csv("PowerUsage_Combined.csv", header = T)
head(project.data)
tail(project.data)
 
# Creating time series data set from daily power usage data:
power.ts <- ts(project.data$Value..kWh., 
               start = c(2016,153),end= c(2020, 189), frequency =c(365.25))

## EXPLORE AND VISUALIZE DAILY HISTORICAL DATA
#To Visualize historical data
plot(power.ts, 
     xlab = "Time", ylab = "Power Usage (in KWh)",
     main = "Daily Power Usage", col = "blue") 

# Create zoom-in plot for 1 year from 1917 week 1 to week 52.
power.ts.52weeks <- window(power.ts, start = c(2017, 1), end = c(2017, 52))
plot(power.ts.52weeks, 
     xlab = "Time", ylab = "Power Usage (in KWh)", 
     main = "Daily Power Usage for 1 Year", col = "blue")

# Plot different time series Components of the original data
power.stl <- stl(power.ts, s.window = "periodic")
autoplot(power.stl, main = "Power Usage Time Series Components")

# Identify autocorrelation and plot it for different lags (up to maximum of 365.25).
autocor <- Acf(power.ts, lag.max = 365.25, main = "Autocorrelation for Daily Power Usage")

# Checking for predictability of the time series data
power.ar1<- Arima(power.ts, order = c(1,0,0))
summary(power.ar1)
z.statistics <- (0.8894-1)/0.0118
z.statistics
p_value <- pnorm(z.statistics)
p_value
# Reject the null hypothesis, Data set is predictable

Acf(diff(power.ts, lag = 1), lag.max = 365.25, main = "Autocorrelation for Daily Power Usage")

## PARTITION THE TIME SERIES INTO TRAINING AND VALIDATION DATA

# Define the numbers of months in the training and validation sets
nValid <- 311
nTrain <- length(power.ts) - nValid
train.ts <- window(power.ts, start = c(2016, 153), end = c(2016, nTrain))
valid.ts <- window(power.ts, start = c(2016,nTrain + 1), 
                   end = c(2016,nTrain + nValid))

## APPLYING FORECASTING METHODS

# Regression with Quadratic trend + Seasonality
train.quadTrend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.quadTrend.season)

# We are not able to create models (above is one of the model we tried to 
# create) with daily data expect with ARIMA. Therefore,we decided to 
# aggregate the daily data into weekly data.


## CLEANING THE DATA SET AND AGGREGATING THE DAILY DATASET INTO WEEKLY DATASET

# Add 2 rows data in the starting to complete the week
previous.records <- data.frame(c("2016-05-30","2016-05-31"), c(30,31), c(79,77.5), c(70.85,72.5),
                               c(73.6,78.7),c(7.4,7),c(29.8,29.8), c(28.027,28.5),c("weekday","weekday"))

names(previous.records) <- c("Date","Day","Temp_avg","Dew_avg","Hum_avg", "Wind_avg", "Press_avg","Value..kWh.","notes")
new.project.data <- rbind(previous.records, project.data)

nrow(new.project.data)
head(new.project.data)
tail(new.project.data)

# Remove 2 rows data at the end to complete the week
final.data  = new.project.data[-c(1499, 1500),]
tail(final.data)

# Aggregate daily data to get weekly power usage
Week <- cut(as.Date(final.data$Date), "week")
project <- aggregate(Value..kWh. ~ Week, final.data, sum)


# Aggregate daily data to get weekly average weather parameters
project.temp <- aggregate(Temp_avg ~ Week, final.data, mean)
project.hum <- aggregate(Hum_avg ~ Week, final.data, mean)
project.wind <- aggregate(Wind_avg ~ Week, final.data, mean)
project.press <- aggregate(Press_avg ~ Week, final.data, mean)
project.dew <- aggregate(Dew_avg ~ Week, final.data, mean)

# Combining all the variables to create a single dataframe
project1 <- data.frame(Temp_avg = c(project.temp$Temp_avg),
                                    Hum_avg = c(project.hum$Hum_avg),
                                    Dew_avg = c(project.dew$Dew_avg),
                                    Press_avg = c(project.press$Press_avg),
                                    Wind_avg = c(project.wind$Wind_avg))
project2 <- cbind(project,project1)
head(project2)
tail(project2)
options(max.print = 99999)


# Use ts() function to create time series data set for dependent 
# as well as external variables
value.ts <- ts(project2$Value..kWh., 
               start = c(2016, 22), frequency = 52)
temp.ts <- ts(project2$Temp_avg, 
               start = c(2016, 22), frequency = 52)
press.ts <- ts(project2$Press_avg, 
               start = c(2016, 22), frequency = 52)
hum.ts <- ts(project2$Hum_avg, 
               start = c(2016, 22), frequency = 52)
dew.ts <- ts(project2$Dew_avg, 
               start = c(2016, 22), frequency = 52)
wind.ts <- ts(project2$Wind_avg, 
               start = c(2016, 22), frequency = 52)
length(value.ts)

## EXPLORE AND VISUALIZE FINAL HISTORICAL DATA

# Plot the data set
plot(value.ts, 
     xlab = "Time", ylab = "Power Usage (in KWh)",
     main = "Weekly Power Usage", col = "blue")

# Create zoom-in plot for 1 year from 1917 week 1 to week 52.
value.ts.52weeks <- window(value.ts, start = c(2017, 1), end = c(2017, 52))
plot(value.ts.52weeks, 
     xlab = "Time", ylab = "Weekly Power Usage (in KWh)", 
     main = "Weekly Power Usage for 1 Year", col = "blue")

# Plot different time series Components of the data set
power.stl <- stl(value.ts, s.window = "periodic")
autoplot(power.stl, main = "Power Usage Time Series Components")

# Identify autocorrelation and plot it for different lags (up to maximum of 52).
autocor <- Acf(value.ts, lag.max = 52, main = "Autocorrelation for Power Usage")

# Checking for predictability of the time series data
power.ar1<- Arima(value.ts, order = c(1,0,0))
summary(power.ar1)
z.statistics <- (0.8613-1)/0.0344
z.statistics
p_value <- pnorm(z.statistics)
p_value
# Reject the null hypothesis, Data set is predictable

Acf(diff(value.ts, lag = 1), lag.max = 52, main = "Autocorrelation for Power Usage")

## PARTITION THE TIME SERIES INTO TRAINING AND VALIDATION DATA

# Define the numbers of months in the training and validation sets
#We have added 21 to the training dataset as software is not counting the week
#when a week has dates from different months and due to that we are losing 21 data points 
#so we are adding those back
nValid <- 52 
nTrain <- length(value.ts) - nValid
value.train.ts <- window(value.ts, start=c(2016, 22), end =c(2016,nTrain + 21))
value.valid.ts <- window(value.ts, start = c(2016, nTrain + 22), 
                   end =  c(2016, nTrain + nValid + 21))

valid.week <- project2[c(163:214),1]
week.num <- c("Week 27","Week 28","Week 29","Week 30","Week 31","Week 32","Week 33","Week 34","Week 35","Week 36",
              "Week 37", "Week 38","Week 39","Week 40","Week 41", "Week 42", "Week 43", "Week 44", "Week 45", "Week 46",
              "Week 47", "Week 48", "Week 49", "Week 50", "Week 51","Week 52","Week 1","Week 2","Week 3","Week 4",
              "Week 5","Week 6","Week 7","Week 8","Week 9","Week 10", "Week 11","Week 12","Week 13","Week 14",
              "Week 15", "Week 16", "Week 17","Week 18", "Week 19", "Week 20", "Week 21","Week 22", "Week 23", "Week 24","Week 25","Week 26" )

length(value.ts)
length(value.train.ts)
length(value.valid.ts)

# Plot the time series data and visualize partitions. 
plot(value.train.ts, 
     xlab = "Time", ylab = "Power Usage (in KWh)", ylim = c(30,500), bty = "l",
     xaxt = "n", xlim = c(2016.25, 2021.25), 
     main = "Power Usage Data: Training and Validation Partitions", lwd = 2) 
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE)
lines(value.valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(0, 500))
lines(c(2020.5, 2020.5), c(0, 500))
text(2017.75, 500, "Training")
text(2020, 500, "Validation")
text(2021, 500, "Future")
arrows(2016.25, 450, 2019.35, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 450, 2020.45, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 450, 2020.55+0.9, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## APPLYING FORECASTING METHODS

#Regression with linear trend
train.lintrend <- tslm(value.train.ts ~ trend)
summary(train.lintrend)
train.lintrend.pred <- forecast(train.lintrend, h = nValid, level = 0)
weekly.valid.fst1 <- data.frame(week.num," ", valid.week," ", train.lintrend.pred$mean)
names(weekly.valid.fst1) <- c("Week"," ", "Start Date"," ", "Forecast")
weekly.valid.fst1
round(accuracy(train.lintrend.pred$mean, value.valid.ts),3)

# Plot ts data, linear trend and forecast for validation period.
plot(train.lintrend.pred$mean, 
     xlab = "Time", ylab = "Power Usage (in KWh)", ylim = c (30,500),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Linear Trend for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(train.lintrend.pred$fitted, col = "blue", lwd = 2)
lines(value.train.ts, col = "black", lty = 1)
lines(value.valid.ts, col = "black", lty = 1)
legend(2016,400, legend = c("Power usage Time Series", "Linear Regression for Training Data",
                            "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(0, 500))
lines(c(2020.5, 2020.5), c(0, 500))
text(2017.75, 500, "Training")
text(2020, 500, "Validation")
text(2021, 500, "Future")
arrows(2016.25, 450, 2019.35, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 450, 2020.45, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 450, 2020.55+0.9, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Regression with Quadratic trend
train.quadtrend <- tslm(value.train.ts ~ trend  +I(trend^2))
summary(train.quadtrend)
train.quadtrend.pred <- forecast(train.quadtrend, h = nValid, level = 0)
weekly.valid.fst2 <- data.frame(week.num," ", valid.week," ", train.quadtrend.pred$mean)
names(weekly.valid.fst2) <- c("Week"," ", "Start Date"," ", "Forecast")
weekly.valid.fst2
round(accuracy(train.quadtrend.pred$mean, value.valid.ts),3)

# Plot ts data, regression with quadratic trend & forecast for validation period.
plot(train.quadtrend.pred$mean, 
     xlab = "Time", ylab = "Power Usage (in KWh)", ylim = c (30,500),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Quadratic Trend for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(train.quadtrend.pred$fitted, col = "blue", lwd = 2)
lines(value.train.ts, col = "black", lty = 1)
lines(value.valid.ts, col = "black", lty = 1)
legend(2016,400, legend = c("Power usage Time Series", "Quadratic Trend for Training Data",
                            "Quadratic Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(0, 500))
lines(c(2020.5, 2020.5), c(0, 500))
text(2017.75, 500, "Training")
text(2020, 500, "Validation")
text(2021, 500, "Future")
arrows(2016.25, 450, 2019.35, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 450, 2020.45, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 450, 2020.55+0.9, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Regression with seasonality
train.season <- tslm(value.train.ts ~  season)
summary(train.season)
train.season.pred <- forecast(train.season, h = nValid, level = 0)
weekly.valid.fst3 <- data.frame(week.num," ", valid.week," ", train.season.pred$mean)
names(weekly.valid.fst3) <- c("Week"," ", "Start Date"," ", "Forecast")
weekly.valid.fst3
round(accuracy(train.season.pred$mean, value.valid.ts),3)

# Plot ts data, regression model with seasonality & forecast for validation period.
plot(train.season.pred$mean, 
     xlab = "Time", ylab = "Power Usage (in KWh)", ylim = c (30,500),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Model with Seasonality for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(value.train.ts, col = "black", lty = 1)
lines(value.valid.ts, col = "black", lty = 1)
legend(2016,400, legend = c("Power usage Time Series", "Seasonality Model for Training Data",
                            "Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(0, 500))
lines(c(2020.5, 2020.5), c(0, 500))
text(2017.75, 500, "Training")
text(2020, 500, "Validation")
text(2021, 500, "Future")
arrows(2016.25, 450, 2019.35, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 450, 2020.45, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 450, 2020.55+0.9, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of the model with seasonality.
plot(train.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-200, 300), bty = "l",
     xlim = c(2016.25, 2021.25), main = "Residuals for the Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE)
lines(value.valid.ts - train.season.pred$mean, col = "brown", lty = 1, lwd=2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(-250, 350))
lines(c(2020.5, 2020.5), c(-250, 350))
text(2017.75, 300, "Training")
text(2020, 300, "Validation")
text(2021, 300, "Future")
arrows(2016.25, 250, 2019.35, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 250, 2020.45, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 250, 2020.55+0.9, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Regression with Linear trend + seasonality
train.lintrend.season <- tslm(value.train.ts ~ trend + season)
summary(train.lintrend.season)
train.lintrend.season.pred <- forecast(train.lintrend.season, h = nValid, level = 0)
weekly.valid.fst4 <- data.frame(week.num," ", valid.week," ", train.lintrend.season.pred$mean)
names(weekly.valid.fst4) <- c("Week"," ", "Start Date"," ", "Forecast")
weekly.valid.fst4
round(accuracy(train.lintrend.season.pred$mean, value.valid.ts),3)

# Plot ts data, trend and seasonality data & forecast for validation period.
plot(train.lintrend.season.pred$mean, 
     xlab = "Time", ylab = "Power Usage (in KWh)", ylim = c (30,500),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Model with Linear Trend and Monthly Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(train.lintrend.season.pred$fitted, col = "blue", lwd = 2)
lines(value.train.ts, col = "black", lty = 1)
lines(value.valid.ts, col = "black", lty = 1)
legend(2016,400, legend = c("Power usage Time Series", "Trend and Seasonality Model for Training Data",
                            "Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(0, 500))
lines(c(2020.5, 2020.5), c(0, 500))
text(2017.75, 500, "Training")
text(2020, 500, "Validation")
text(2021, 500, "Future")
arrows(2016.25, 450, 2019.35, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 450, 2020.45, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 450, 2020.55+0.9, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of predictions with trend and seasonality.
plot(train.lintrend.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-200, 300), bty = "l",
     xlim = c(2016.25, 2021.25), main = "Residuals for Trend and Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE)
lines(value.valid.ts - train.lintrend.season.pred$mean, col = "brown", lty = 1, lwd=2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(-250, 350))
lines(c(2020.5, 2020.5), c(-250, 350))
text(2017.75, 300, "Training")
text(2020, 300, "Validation")
text(2021, 300, "Future")
arrows(2016.25, 250, 2019.35, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 250, 2020.45, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 250, 2020.55+0.9, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Regression with Quadratic trend + seasonality
train.quadtrend.season <- tslm(value.train.ts ~ trend +I(trend^2)+ season)
summary(train.quadtrend.season)
train.quadtrend.season.pred <- forecast(train.quadtrend.season, h = nValid, level = 0)
weekly.valid.fst5 <- data.frame(week.num," ", valid.week," ", train.quadtrend.season.pred$mean)
names(weekly.valid.fst5) <- c("Week"," ", "Start Date"," ", "Forecast")
weekly.valid.fst5
round(accuracy(train.quadtrend.season.pred$mean, value.valid.ts),3)

# Plot ts data, Quadratic trend and seasonality data & forecast for validation period.
plot(train.quadtrend.season.pred$mean, 
     xlab = "Time", ylab = "Power Usage (in KWh)", ylim = c (30,500),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Model with Quadratic Trend and Monthly Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(train.quadtrend.season.pred$fitted, col = "blue", lwd = 2)
lines(value.train.ts, col = "black", lty = 1)
lines(value.valid.ts, col = "black", lty = 1)
legend(2016,400, legend = c("Power usage Time Series", "Quad Trend and Seasonality Model for Training Data",
                            "Quad Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(0, 500))
lines(c(2020.5, 2020.5), c(0, 500))
text(2017.75, 500, "Training")
text(2020, 500, "Validation")
text(2021, 500, "Future")
arrows(2016.25, 450, 2019.35, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 450, 2020.45, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 450, 2020.55+0.9, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Compare the Models based on Accuracy measures
round(accuracy(train.lintrend.pred$mean, value.valid.ts),3)
round(accuracy(train.quadtrend.pred$mean, value.valid.ts),3)
round(accuracy(train.season.pred$mean, value.valid.ts),3)
round(accuracy(train.lintrend.season.pred$mean, value.valid.ts),3)
round(accuracy(train.quadtrend.season.pred$mean, value.valid.ts),3)


# CREATING IMPROVED TWO LEVEL MODELS:
# Two level model Regression with linear trend and seasonality + AR(1) model with regression residuals
Acf(train.lintrend.season.pred$residuals, lag.max = 52, main = "Autocorrelation for Regression model residuals")

##Fit AR(1) model for training residuals.
ar.lin.seas <- Arima(train.lintrend.season.pred$residuals, order= c(1,0,0))
summary(ar.lin.seas)
ar.lin.seas.pred <- forecast(ar.lin.seas, h = nValid, level=0)
weekly.valid.fst9 <- data.frame(week.num," ", valid.week," ", ar.lin.seas.pred$mean)
names(weekly.valid.fst9) <- c("Week"," ", "Start Date"," ", "Residual Forecast")
weekly.valid.fst9
Acf(ar.lin.seas.pred$residuals, lag.max = 52, main = "Autocorrelation for Residuals of residuals")

lintrend.season.twolevelAR.pred <- train.lintrend.season.pred$mean + ar.lin.seas.pred$mean
weekly.valid.fst10 <- data.frame(week.num," ", valid.week," ", lintrend.season.twolevelAR.pred)
names(weekly.valid.fst10) <- c("Week"," ", "Start Date"," ", "Twolevel Fst(R.lin.sea+AR(1))")
weekly.valid.fst10
round(accuracy(lintrend.season.twolevelAR.pred, value.valid.ts),3)


##2. Two level model Regression with linear trend and seasonality + trailing MA with regression residuals
# Regression residuals in validation period.
trend.seas.res.valid <- value.valid.ts - train.lintrend.season.pred$mean
trend.seas.res.valid
ma.trailing_1 <- rollmean(train.lintrend.season.pred$residuals, k = 1, align = "right")
ma.pred <- forecast(ma.trailing_1, h= nValid, level =0)
weekly.valid.fst11 <- data.frame(week.num," ", valid.week," ", ma.pred$mean)
names(weekly.valid.fst11) <- c("Week"," ", "Start Date"," ", "Forecast of MA(1) ")
weekly.valid.fst11

# Plot residuals and MA residuals forecast in training and validation partitions. 
plot(train.lintrend.season.pred$residuals, 
     xlab = "Time",ylab = "Power Usage (in KWh)", ylim = c(-200, 300),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Regression Residuals and Trailing MA for Residuals", 
     col = "brown", lwd =2) 
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(trend.seas.res.valid, col = "brown", lwd = 2, lty = 2)
lines(ma.trailing_1, col = "blue", lwd = 2, lty = 1)
lines(ma.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(2016,275, legend = c("Regression Residuals, Training Partition", 
                            "Regression Residuals, Validation Partition",
                            "MA Forecast (k=1), Training Partition", 
                            "MA forecast (k=1), Validation Partition"), 
       col = c("brown", "brown", "blue", "blue"), 
       lty = c(1, 2, 1, 2), lwd =c(2, 2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(-250, 350))
lines(c(2020.5, 2020.5), c(-250, 350))
text(2017.75, 300, "Training")
text(2020, 300, "Validation")
text(2021, 300, "Future")
arrows(2016.25, 250, 2019.35, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 250, 2020.45, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 250, 2020.55+0.9, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)

twolvl.reg.ma <- train.lintrend.season.pred$mean + ma.pred$mean
weekly.valid.fst12 <- data.frame(week.num," ", valid.week," ", twolvl.reg.ma)
names(weekly.valid.fst12) <- c("Week"," ", "Start Date"," ", "Twolevel Fst(R.lin.sea+MA(1)")
weekly.valid.fst12
round(accuracy(twolvl.reg.ma , value.valid.ts),3)
Acf(ma.pred$residuals, lag.max = 52, main = "Autocorrelation for Residuals of residuals")


##3. Two level model Regression with linear trend and seasonality + SES model with regression residuals
ses <- ets(train.lintrend.season.pred$residuals, model = "ZNN")
summary(ses)
ses.pred <- forecast(ses, h= nValid, level = 0)
weekly.valid.fst13 <- data.frame(week.num," ", valid.week," ", ses.pred$mean)
names(weekly.valid.fst13) <- c("Week"," ", "Start Date"," ", "Forecast of SES")
weekly.valid.fst13

twolvl.reg.ses <- train.lintrend.season.pred$mean + ses.pred$mean
weekly.valid.fst14 <- data.frame(week.num," ", valid.week," ", twolvl.reg.ses)
names(weekly.valid.fst14) <- c("Week"," ", "Start Date"," ", "Twolevel Fst(R.lin.sea+SES)")
weekly.valid.fst14
round(accuracy(twolvl.reg.ses, value.valid.ts),3)
Acf(ses.pred$residuals, lag.max = 52, main = "Autocorrelation for Residuals of residuals")


### FIT REGRESSION MODELS WITH APPROPRIATE EXTERNAL VARIABLES:
# Check correlation between power usage and different weather parameters
project.cor <- subset(project2, select = -c(1))
head(project.cor)
corrplot(cor(project.cor),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         col = NULL)       # Color palette

corrplot(cor(project.cor),
         method = "circle",       
         order = "hclust",         # Ordering method of the matrix
         hclust.method = "ward.D", # If order = "hclust", is the cluster method to be used
         addrect = 2,              # If order = "hclust", number of cluster rectangles
         rect.col = 3,             # Color of the rectangles
         rect.lwd = 3)             # Line width of the rectangles

corPlot(project.cor, cex = 0.8) 

# Partition the external variables(temp & dew) into training and validation sets
temp.train.ts <- window(temp.ts, start=c(2016, 22), end =c(2016,nTrain + 21))
temp.valid.ts <- window(temp.ts, start = c(2016, nTrain + 22), 
                        end =  c(2016, nTrain + nValid + 21))

dew.train.ts <- window(dew.ts, start=c(2016, 22), end =c(2016,nTrain + 21))
dew.valid.ts <- window(dew.ts, start = c(2016, nTrain + 22), 
                       end =  c(2016, nTrain + nValid + 21))



#Regression with Linear trend + seasonality with 2 external variables(temp_avg,dew_avg)
lin.season.external_2 <- tslm(value.train.ts ~ trend + season
                              + temp.train.ts + dew.train.ts)
summary(lin.season.external_2)

# To forecast on validation data set
forecast_param1 <- data.frame(trend = c(163:214), temp.train.ts = temp.valid.ts[1:52], 
                              dew.train.ts = dew.valid.ts[1:52])

lin.season.external2.pred <- forecast(lin.season.external_2, newdata = forecast_param1, level = 0)
weekly.valid.fst16 <- data.frame(week.num," ", valid.week," ", lin.season.external2.pred$mean)
names(weekly.valid.fst16) <- c("Week"," ", "Start Date"," ", "Forecast with 2 external variable")
weekly.valid.fst16
round(accuracy(lin.season.external2.pred$mean, value.valid.ts),3)

#Apply two level models to the residuals of Regression with Linear trend + seasonality 
#with 2 external variables(temp_avg,dew_avg) 
#1- Ma to the residuals:
ma.external.res <- rollmean(lin.season.external2.pred$residuals, k = 1, align = "right")
ma.external.res
ma.external.res.pred <- forecast(ma.external.res, h = nValid, level = 0)
Acf(ma.external.res.pred$residuals, lag.max = 52, main = "Autocorrelation for residuals of residuals")
totalpred.external.ma <- lin.season.external2.pred$mean + ma.external.res.pred$mean
round(accuracy(lin.season.external2.pred$mean + ma.external.res.pred$mean, value.valid.ts),3)

#SES to the residuals
ses.external.res <- ets(lin.season.external2.pred$residuals, model = "ZNN")
ses.external.res
ses.external.res.pred <- forecast(ses.external.res, h = nValid, level = 0)
Acf(ses.external.res.pred$residuals, lag.max = 52, main = "Autocorrelation for residuals of residuals")
totalpred.external.ses <- lin.season.external2.pred$mean + ses.external.res.pred$mean
round(accuracy(lin.season.external2.pred$mean + ses.external.res.pred$mean, value.valid.ts),3)

#Autoregressive model to the residuals:
ar.external.res <- arima(lin.season.external2.pred$residuals, order = c(1,0,0))
ar.external.res
ar.external.res.pred <- forecast(ar.external.res, h = nValid, level = 0)
Acf(ar.external.res.pred$residuals, lag.max = 52, main = "Autocorrelation for residuals of residuals")
totalpred.external.ar <- lin.season.external2.pred$mean + ar.external.res.pred$mean
round(accuracy(lin.season.external2.pred$mean + ar.external.res.pred$mean, value.valid.ts),3)

# Arima Models
arima.train <- auto.arima(value.train.ts)
summary(arima.train)
arima.pred <- forecast(arima.train,h = nValid, level = 0)
weekly.valid.fst6 <- data.frame(week.num," ", valid.week," ", arima.pred$mean)
names(weekly.valid.fst6) <- c("Week"," ", "Start Date"," ", "Forecast")
weekly.valid.fst6
round(accuracy(arima.pred$mean, value.valid.ts),3)
Acf(arima.pred$residuals, lag.max = 52, main = "Autocorrelation for Arima Residuals of residuals")

# Plot ts data, ARIMA model & forecast for validation period.
plot(arima.pred, 
     xlab = "Time", ylab = "Power Usage (in KWh)", ylim = c (30,500),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Auto ARIMA Model", 
     lwd = 2, flty = 5)
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(arima.pred$fitted, col = "blue", lwd = 2)
lines(value.valid.ts, col = "black", lwd = 2, lty = 1)
legend(2016,450, legend = c("Power usage Time Series", 
                            "ARIMA Forecast for Training Period",
                            "ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.5 - 1, 2020.5 - 1), c(0, 500))
lines(c(2020.5, 2020.5), c(0, 500))
text(2017.75, 500, "Training")
text(2020, 500, "Validation")
text(2021, 500, "Future")
arrows(2016.25, 450, 2019.35, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.55, 450, 2020.45, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 450, 2020.55+0.9, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)



## EVALUATE AND COMPARE PERFORMANCE OF THE MODELS

#Accuracy check to accept the models for future forecasting:
round(accuracy(train.lintrend.season.pred$mean, value.valid.ts),3)
round(accuracy(lintrend.season.twolevelAR.pred, value.valid.ts),3)
round(accuracy(twolvl.reg.ses, value.valid.ts),3)
round(accuracy(twolvl.reg.ma , value.valid.ts),3)
round(accuracy(arima.pred$mean, value.valid.ts),3)
##Accuracy for multivariable regression model:
round(accuracy(lin.season.external2.pred$mean, value.valid.ts),3)
##Accuracy for Two Level multivariable regression model:
round(accuracy(lin.season.external2.pred$mean + ma.external.res.pred$mean, value.valid.ts),3)
round(accuracy(lin.season.external2.pred$mean + ar.external.res.pred$mean, value.valid.ts),3)
round(accuracy(lin.season.external2.pred$mean + ses.external.res.pred$mean, value.valid.ts),3)

# Best Accuracy with two level model: 
# regression with linear trend and seasonality + trailing MA(1)

future.week <- c("2020-07-06", "2020-07-13", "2020-07-20","2020-07-27","2020-08-03",
                 "2020-08-10", "2020-08-17","2020-08-24","2020-08-31", "2020-09-07",
                 "2020-09-14","2020-09-21")
week.No <- c("Week 27","Week 28","Week 29", "Week 30","Week 31","Week 32","Week 33",
             "Week 34","Week 35","Week 36","Week 37", "Week 38")


## IMPLEMENT THE FINAL FORECASTING MODEL TO PREDICT FUTURE DATA

# Model 1: Regression with Linear trend + seasonality for Entire data set
# Create regression forecast for future 12 weeks
total.lintrend.season <- tslm(value.ts ~ trend + season)
summary(total.lintrend.season)
total.lintrend.season.pred <- forecast(total.lintrend.season, h = 12, level = 0)
weekly.valid.fst17 <- data.frame(week.No," ", future.week," ", total.lintrend.season.pred$mean)
names(weekly.valid.fst17) <- c("Week"," ", "Start Date"," ", "Future Forecast (Reg.lin+sea)")
weekly.valid.fst17

Acf(total.lintrend.season$residuals, lag.max = 52, 
    main = "Autocorrelations of Regression lin+seas Residuals")

# Plot ts data, regression model with trend and seasonality data, 
# and predictions for future 4 periods.
plot(total.lintrend.season.pred$mean, 
     xlab = "Time", ylab = "Power Usage (in KWh)", ylim = c (30,500),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Model with Linear Trend and Monthly Seasonality and Forecast for Future Periods", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(total.lintrend.season.pred $fitted, col = "blue", lwd = 2)
lines(value.ts, col = "black", lty = 1)
legend(2016,450, legend = c("Power usage Time Series", "Trend and Seasonality Model for for Entire Data",
                            "Trend and Seasonality Forecast for Future 12 Weeks"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing Entire data set and future prediction intervals.
lines(c(2020.5, 2020.5), c(0, 500))
text(2018.5, 500, "Data Set")
text(2021, 500, "Future")
arrows(2016.25, 450, 2020.45, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 450, 2020.55+0.9, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(total.lintrend.season.pred$fitted, value.ts),3)
round(accuracy((naive(value.ts))$fitted, value.ts), 3)
round(accuracy((snaive(value.ts))$fitted, value.ts), 3)

# Model 2: Two level model Regression with linear trend and seasonality + trailing MA(1)
tot.ma.trail.res <- rollmean(total.lintrend.season$residuals, k = 1, align = "right")
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)

weekly.valid.fst18 <- data.frame(week.No," ", future.week," ", tot.ma.trail.res.pred$mean)
names(weekly.valid.fst18) <- c("Week"," ", "Start Date"," ", "Residual Future Forecast (MA)")
weekly.valid.fst18

tot.fst.2level <- total.lintrend.season.pred$mean + tot.ma.trail.res.pred$mean
weekly.valid.fst19 <- data.frame(week.No," ", future.week," ", tot.fst.2level)
names(weekly.valid.fst19) <- c("Week"," ", "Start Date"," ", "Future Forecast (Reg.lin.sea + MA)")
weekly.valid.fst19
Acf(tot.ma.trail.res.pred$residuals, lag.max = 52, 
    main = "Autocorrelations of Two level(Reg+MA) Residuals")

# Plot residuals and MA residuals forecast in training and validation partitions. 
plot(total.lintrend.season.pred$residuals, 
     xlab = "Time",ylab = "Power Usage (in KWh)", ylim = c(-200, 300),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Regression Residuals and Trailing MA for Residuals", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(tot.ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(tot.ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(2016,260, legend = c("Regression Residuals", 
                        "Trailing MA (k=1) for Residuals", 
                        "Trailing MA Forecast (k=1) for Future 12 Periods"), 
       col = c("brown", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows 
# describing Entire data set and future prediction intervals.
lines(c(2020.5, 2020.5), c(-250, 350))
text(2018.5, 300, "Data Set")
text(2021, 300, "Future")
arrows(2016.25, 250, 2020.45, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 250, 2020.55+0.9, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Model 3: ARIMA Model
arima.total <- auto.arima(value.ts)
summary(arima.total)
arima.total.pred <- forecast(arima.total, h=12,level = 0)
weekly.valid.fst20 <- data.frame(week.No," ", future.week," ", arima.total.pred$mean)
names(weekly.valid.fst20) <- c("Week"," ", "Start Date"," ", "Future Forecast ARIMA")
weekly.valid.fst20
# Use Acf() function on auto ARIMA model residuals.
Acf(arima.total$residuals, lag.max = 52, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 4 future periods.
plot(value.ts, 
     xlab = "Time", ylab = "Power Usage (in KWh)", ylim = c (30,500),
     bty = "l", xlim = c(2016.25, 2021.25),
     main = "Auto ARIMA Model for Entire Dataset")
axis(1, at = seq(2016.25, 2021, 0.25), labels = FALSE )
lines(arima.total$fitted, col = "blue", lwd = 2)
lines(arima.total.pred$mean, col = "blue", lwd = 5, lty = 2)
legend(2016,450, legend = c("Power usage Time Series", 
                            "Auto ARIMA Forecast", 
                            "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows 
# describing Entire data set and future prediction intervals.
lines(c(2020.5, 2020.5), c(0, 500))
text(2018.5, 500, "Data Set")
text(2021, 500, "Future")
arrows(2016.25, 450, 2020.45, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.55, 450, 2020.55+0.9, 450, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## EVALUATE AND COMPARE PERFORMANCE OF THE MODELS ON FUTURE DATA

round(accuracy(total.lintrend.season.pred$fitted, value.ts),3)
round(accuracy(total.lintrend.season.pred$fitted + tot.ma.trail.res.pred$fitted, value.ts), 3)
round(accuracy(arima.total.pred$fitted, value.ts),3)
round(accuracy((naive(value.ts))$fitted, value.ts), 3)
round(accuracy((snaive(value.ts))$fitted, value.ts), 3)


