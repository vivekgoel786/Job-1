ek <- rnorm(100)

install.packages("TSA")
install.packages("astsa")
install.packages("fpp2")
install.packages("forecast")


#plot
#mean
#variance
#summary
#histogram
#normality test
#stationarity test

#If it is not time series
#plot(sales)
#plot(sales[,2])
#plot(sales[,2], type='l)

#convert to time series
#Sales_ts <- ts(sales[,2], start=c(1949, 1), frequency=12)

data("AirPassengers")
library(data.table)
library(ggplot2)
library(fpp2)
library(forecast)
library(tseries)
library(stats)

#plot the time series data
AirPassengers #already a time series data
plot(AirPassengers, xlab="Years", ylab="No. of Passengers", main="Airpassengers data")
hist(AirPassengers)

View(AirPassengers)

#seasonal plot year-wise
ggseasonplot(AirPassengers, year.labels = TRUE, year.labels.left = TRUE) +ylab("degree") +
  ggtitle("Seasonal plot : Airpassendger Data")

#polar seasonal plot year-wise, if its circular then no seasonality
ggseasonplot(AirPassengers, polar  = TRUE) +ylab("degree") +
  ggtitle("Seasonal plot : Airpassendger Data")

#monthly plot
monthplot(AirPassengers)#horizontal line is the average
monthplot(AirPassengers[1:72])
monthplot(AirPassengers[73:144])
par(mfrow=c(2,1))
par(mfrow=c(1,1))

AirPassengers

#decomposition using Decompose function
Air_Decom <- decompose(AirPassengers, type = "multiplicative")
plot(Air_Decom)

Air_Decom

plot(decompose(Air_Decom$random))

#observing the seasonal indices of Air_Decom
Seasonal_index <-round(t(Air_Decom$figure), 2) 
colnames(Seasonal_index) <- c("J", "F", "M", "A", "M","J", "J", "A", "S", "O", "N", "D")
Seasonal_index


#decomposition using stl(seasonality and trend using lowess) function
#this function is used for additive models not multiplicative, so taken log
#the observed section differs from previous decompose as its additive now in log sales 
Air_Decom_Log <- stl(log10(AirPassengers), s.window = 'p')
plot(Air_Decom_Log)

#measures of forecast accuracy
#et = ythat - yt 
#mean absolute deviation, absolute sum of error terms by number of obs
#mean absolute percentage error, sum of ratio of absolute error and Yt, divided by n into 100
#mean squared error, sum of squared error by n
#root mean sqaure error

#splitting the training and test
Train <- window(AirPassengers, start=c(1949, 1), end=c(1958, 12), frequency=12)
Test <- window(AirPassengers, start=c(1959, 1), frequency=12)
Train

autoplot(Train, series = "Train")+autolayer(Test, series = "Test")+
  ggtitle("No. of passengers train and test data")+
  xlab("Year") + ylab("No. of passengers") + 
  guides(colour=guide_legend(title = "Forecast"))

#FORECASTING METHODS
#random walk with drift
Train_Decom_Log <- stl(log10(Train), s.window = 'p')
Train_Decom_Log_stlforcast <- forecast(Train_Decom_Log, method = "rwdrift", h = 24)
Train_Decom_Log_stlforcast

#plotting the model
plot(Train_Decom_Log_stlforcast)
Vec2 <- 10^(cbind(log10(Test), as.data.frame(Train_Decom_Log_stlforcast)[,1]))
ts.plot(Vec2, col=c("blue", "red"), main="No. of Passengers Actual vs Forecast") 
#actual is blue and forecast is red.....actual is a bit high #under-predicted

#check accuracy of RWD
RMSE2<- round(sqrt(sum((Vec2[,1]- Vec2[,2])^2)/length(Vec2[,1])),4)
MAPE2 <- round(mean(abs(Vec2[,1]-Vec2[,2])/Vec2[,1]),4)

paste("Accuracy Measures RMSE:", RMSE2, "and MAPE:", MAPE2)


#exponential smoothing
#Yt+1hat=alpha*Yt + (1-alpha)Ythat
#Yt+1hat=Ythat + alpha(Yt-Ythat)
#0<alpha<1
# if alpha = 1, Yt+1hat=Yt, naive forecast.. changing with today's forecast
# if alpha = 0, Yt+1hat=Ythat=Yt-1hat..... constant forecast 


#simple exponential smoothing #no seasonality
Train_SES <- ses(Train, h=24)
Train_SES

#holt #no seasonality   
Train_Holt <- holt(Train, h=24)

#holt winters smoothing model(tripple exponential- level, trend, seasonal in forecast eq)
?hw
Train_HW <- hw(Train, h=24, seasonal="multiplicative")
Train_HW
Train_HW$model
#alpha = 0.3477, smoothing of the level,btw 0 & 1 i.e.avg of the current and the past level
#beta  = 2e-04, close to 0, means trend is not changing much, series constantly increasing in straight line, slope not changing much
#gamma = 0.6519, high seasonal fluctuation  

#plotting the forecast
plot(Train_HW)
plot(forecast(Train_HW, h=24), main = "HW forecast")

Vec3 <- cbind(Test, as.data.frame(Train_HW)[,1], as.data.frame(Train_SES)[,1], as.data.frame(Train_Holt)[,1])
ts.plot(Vec3, col=c("blue", "red", "green", "yellow"), main="No. of Passengers Actual vs Forecast") 

Vec3 <- cbind(Test, as.data.frame(Train_HW)[,1])
ts.plot(Vec3, col=c("blue", "red"), main="No. of Passengers Actual vs Forecast") 
#still under-predicted

#check accuracy of HW Expo Smoothing
RMSE3 <- round(sqrt(sum((Vec3[,1]- Vec3[,2])^2)/length(Vec3[,1])),4)
MAPE3 <- round(mean(abs(Vec3[,1]-Vec3[,2])/Vec3[,1]),4)
paste("Accuracy Measures RMSE:", RMSE3, "and MAPE:", MAPE3)
#RMSE tracks variance and MAPE tracks bias
