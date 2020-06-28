#week1
data <- rnorm(1000)

#histogram
hist(data, freq = F)
hist(data, freq = F, col = "green", breaks = 20)
lines(density(data), col='red', lwd=2)

#scatterplot of bivariate data
set.seed =2016
test1_scores=round(rnorm(50, 78, 10))
test2_scores=round(rnorm(50, 70, 14))
plot(test1_scores,test2_scores)
plot(test2_scores~test1_scores)
plot(test2_scores~test1_scores, main = 'test scores', col = 'blue')


#regression line
help(co2)
plot(co2)
co2
co2.lm <- lm(co2~ time(co2))
co2.lm
summary(co2.lm)

plot(co2)
abline(co2.lm)

co2.fitted <- co2.lm$fitted.values
co2.residuals <- resid(co2.lm)
hist(co2.residuals)
qqnorm(co2.residuals)
qqline(co2.residuals)

plot(co2.residuals~time(co2))
plot(co2.residuals~time(co2), xlim= c(1960,1963), main = 'Zoomed Residuals')


help("sleep")
sleep
plot(extra~group, data=sleep)

attach(sleep)
extra.1=extra[group==1]
extra.2=extra[group==2]


#t test
#H0: mu1  =  mu2
#H1: mu1 !=  mu2
extra.1-extra.2
s = sd(extra.1-extra.2)
d = mean(extra.1) - mean(extra.2)
numerator= d - 0
denominator = (sd(extra.1-extra.2))/sqrt(10)
tcalculated = numerator/denominator #-4.062128 

#CI =  d - [(talpha/2) (s/sqrt(n))]
#talpha/2) at (0.975, 9)

t.test(extra.1, extra.2, paired = TRUE, alternative = "two.sided")
#pval<0.05, so we reject the null.... and there is significant difference between the two drugs
#95% CI does not include 0
detach(sleep)


plot(trees, pch=21, bg=c("blue"))
pairs(trees, pch=21, bg=c("red"))#same
cov(trees) #wrong compared to visuals because of units
cor(trees) # better results



#week2
library(astsa)
#plotting the time series data
plot(jj, type = 'o', xlab = 'Years', ylab = 'Earnings',
     main = 'J&J quaterly earnings per share') #seasonal and trend, variation increasing overtime

plot(flu, xlab = 'Months', ylab = 'No. of deaths per 10000 people ',
     main = 'Monthly Pneumonia and Influenza Deaths in US') #seasonality, cant say about trend

plot(globtemp, type = 'o', xlab = 'Years', ylab = 'Temp Deviations', #trend and seasonality
     main = 'Global mean land-ocean deviations from avg temp of 1951-1980')

plot(star, xlab = 'Days', ylab = 'Magnitude',
     main = 'Magnitude of a star for 600 consecutive days') #seasonality , no trend


#stationarity
#in a weak stationary time series there is 
# no systematic change in mean (no trend)
# no systematic change in variation overtime
# no periodic fluctuation (no seasonality)

purely_random_process <- ts(rnorm(100))
(acf(purely_random_process, type = 'covariance')) #autocov coeff for every single lag


(acf(purely_random_process))
#its correlogram until lag 20, starts at 1 as c0/c0 = 1
#not much correlation btw all the different lags


#random walk
#Xt = Xt-1  +  Zt
#Zt is white noise, random noise, residual with some expectation and some variance
# X0 = 0, X1 = Z1, X2 = Z1+Z2 or X1+Z2, Xt = sum of Zi till time t
#E[Xt] = mut, Var[Xt] = sigma sqt

x = NULL
x[1] = 0
for(i in 2:1000){
  x[i]=x[i-1]+rnorm(1)} #rnorm which purely random process adds noise
print(x)
length(x)
random_walk <- ts(x)
plot(random_walk, main='A random walk', ylab = ' ', xlab = 'Days', col = 'blue', lwd = 2)
acf(random_walk) # very high correlation, no stationarity

#if we can take differences in random walk ..it makes the time series stationary, which is purely random process
#Xt = Xt-1  +  Zt
#Xt - Xt-1  =  Zt (purely random process)

diff(random_walk)  
plot(diff(random_walk))
acf((diff(random_walk))) #not much correlation btw all the different lags


#Moving Average
#xt= Zt+ b1 Zt-1  + b2 Zt-2 (MA order 2)
#Zi normaly distributed with mu and sigma sq 

#xt= Zt+ 0.7 Zt-1 +  0.2 Zt-2 #Zt ~ N(0,1)

#generate noise
noise=rnorm(10000)

ma_2=NULL
#loop for generating MA(2) process
for (i in 3:10000){
  ma_2[i]=noise[1]+0.7*noise[i-1]+0.2*noise[i-2]
} 

#shift data to left by 2 units
moving_average_process = ma_2[3:10000]

#put time series structure on a vanilla data
moving_average_process = ts(moving_average_process)
par(mfrow=c(2,1))

plot(moving_average_process, main = 'Moving average process of order 2', ylab = '', col = 'blue')
acf(moving_average_process, main = 'ACF of Moving_average_process of order 2')
#Autocorrelation function of a moving average process cuts off at lag qq if the order of the process is qq.
par(mfrow=c(1,1))



#WEEK 3

