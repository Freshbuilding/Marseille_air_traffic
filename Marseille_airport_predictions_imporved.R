# Load required libraries
library(readxl)
library(dplyr)
library(forecast)
library(TSA)
library(zoo)

# Import and explore the data
Mtraf <- read_excel('marseille-1982-2016.xls', sheet = "marseille")
str(Mtraf)
summary(Mtraf)

# Transform the data into a time series object
traf <- ts(Mtraf[,3], start=c(1982,1), frequency=12)
str(traf)
class(traf)
start(traf) # Starting date
end(traf) # Ending date
frequency(traf)

# Plot the time series
plot(traf)
# Observe a trend, an increasing variance, and probably a seasonal effect

# Analyze stationarity on Toulouse airport dataset
traf %>% ggtsdisplay(lag.max=60) 
# ACF confirms non-stationarity

# Decompose the series into trend, seasonality, and random component
x <- decompose(traf)
plot(x)

# Visualization of the frequency
monthplot(traf)

# Boxplot
boxplot(traf~cycle(traf))

# Time series transformations
# Log transformation to handle increasing variance
ltraf <- log(traf)
ltraf %>% ggtsdisplay(lag.max=60, main="log transformation") 

# First order difference transformation to handle the trend
dltraf <- diff(ltraf, 1)
dltraf %>% ggtsdisplay(lag.max=60, main="1st order diff of log(main series)")

# Difference of order 12 to handle the seasonality
dltraf_12 <- diff(dltraf, 12)
dltraf_12 %>% ggtsdisplay(lag.max=60, main="Diff  order 12 of log(main series)")

# Estimation step
# Fit an initial ARIMA model with AR1, SAR1, and dummy variables
mod1 <- arima(ltraf, c(1,1,1), 
              seasonal=list(order=c(1,1,1), period=12), 
              method='ML')
mod1 # aic = -1056.55

# Check significance of coefficients
tstat <- mod1$coef/sqrt(diag(mod1$var.coef)) # test statistic of the Student test
pvalue <- 2*(1-pnorm(abs(tstat))) # pvalue of the test 
tstat
pvalue
# AR1 and SAR1 are not significant 

# Fit a refined ARIMA model without AR1 and SAR1
mod2 <- arima(ltraf, c(0,1,1), 
              seasonal=list(order=c(0,1,1), period=12), 
              method='ML')
mod2 # aic = -1058.51

# Check significance of coefficients
tstat <- mod2$coef/sqrt(diag(mod2$var.coef)) # test statistic of the Student test
pvalue <- 2*(1-pnorm(abs(tstat))) # pvalue of the test 
tstat
pvalue

# Check residuals for autocorrelation
checkresiduals(mod2)

# Analysis of the outlier
# Identify the outliers using the standardized residuals
res_norm <- mod2$residuals/sqrt(mod2$sigma2)
out <- which(res_norm > 4 | res_norm < -4)
out_dates <- index(res_norm)[out] # Get the dates of the outliers

# Create dummy variables for the outliers
dummy_data <- sapply(out_dates, function(date) {
  as.numeric(index(res_norm) == date)
})

# Fit the ARIMA model with dummy variables
mod3 <- arima(ltraf, c(0,1,1), 
              seasonal=list(order=c(0,1,1), period=12), 
              method='ML',
              xreg=dummy_data)
mod3 # aic = -1275.87

# Check significance of coefficients
tstat <- mod3$coef/sqrt(diag(mod3$var.coef)) # test statistic of the Student test
pvalue <- 2*(1-pnorm(abs(tstat))) # pvalue of the test 
tstat
pvalue

# Check residuals
checkresiduals(mod3)

# Normality test
shapiro.test(mod3$residuals)

# Split the data into training and test sets
data.train <- window(ltraf, start=c(1982,1), end=c(2009,12))
data.test <- window(ltraf, start=c(2010,1), end=c(2016,12))

# Fit the model on the training data
mod.train <- arima(data.train, c(0,1,1), 
                   seasonal=list(order=c(0,1,1), period=12),
                   method='ML',
                   xreg=dummy_data[1:336,])

# Predict the test data
pred.test <- predict(mod.train, n.ahead=84, newxreg=dummy_data[337:420,])

# Assess model performance on the test data
accuracy(pred.test$pred, data.test)
#                  ME       RMSE        MAE       MPE     MAPE      ACF1    Theil's U
#   Test set -0.07240402 0.09690592 0.07946776 -1.124981 1.231118 0.6946365 0.7822077

# All the indicators are low especially for the first three
# The results are good for air traffic expressed in K planes
# The RMSE being 0.1 we have a margin of error of exp(0.1) or 1.1 thus 1100 planes per month

# Predict air traffic for 2017-2019
pred2019 <- predict(mod3, n.ahead=36, newxreg=matrix(0, nrow = 36, ncol = ncol(dummy_data)))
ts.plot(traf, 2.718^pred2019$pred, log="y", lty=c(1,3))

# Import Marseille airport data for 2017-2019
Mtraf2 <- read_excel('marseille-2017-2019.xlsx', sheet = "Feuil1")

# Transform it into a time series
Mtraf2 <- ts(Mtraf2[,3], start=c(2017,1), frequency=12)

# Log transformation
lMtraf2 <- log(Mtraf2)

# Assess the model's performance on the out-of-sample data
accuracy(pred2019$pred, lMtraf2)

#                  ME       RMSE        MAE       MPE      MAPE      ACF1    Theil's U
#   Test set 0.02308529 0.04352146 0.03541769 0.3440979 0.5302491 0.2640584  0.368477

# Once again, the indicators are low especially for the first three
# The results are good for air traffic expressed in K planes
# The RMSE being 0.04 we have a margin of error of exp(0.04) or 1 thus 1000 planes per month

## test for 2017 only 

#subset of the second dataset
lMtraf2017 <- window(lMtraf2, start=c(2017,1), end=c(2017,12))

# Pred for 2017
pred2017 <- predict(mod3, n.ahead=12, newxreg=matrix(0, nrow = 1, ncol = 3))
ts.plot(traf, 2.718^pred2017$pred,log="y", lty=c(1,3))

# Accuracy check
accuracy(pred2017$pred, lMtraf2017)

#                   ME       RMSE        MAE        MPE      MAPE       ACF1    Theil's U
#    Test set 0.002375281 0.02224107 0.01887177 0.03552158 0.2846438 0.05764374 0.1808991

# We can observe that the accuracy indicators are even lower for the following year prediction
# We can conclude that the model lose some prediction power as time passes 

