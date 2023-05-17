##load libraries
library(tidyverse)
library(psych) #gives us all our descriptive parameters
library(modeest)
library(caret)
library(corrplot)
library(car) ## scatter plot

##DESCRIPTIVE STATISTICS
##load data set
suicide_data <- read.csv('Suicide_Data_R1.csv')
suicide_data %>% head()
view(suicide_data)

##checking columns specifications
glimpse(suicide_data)

##checking missing observations in the columns
colSums(is.na(suicide_data))

##Descriptive Statistic Analysis
##excluding country name and year from the analysis
variables <- colnames(suicide_data[, -c(1,2)])

describe(suicide_data[variables])

#OR to include mode

descriptive_stat <- function(x, na.omit=F){
  
  if (na.omit)
    x <- x[!is.na(x)] ## dropping the missing observations
  if (length(mfv(x)) != 1){
    mod <- NA
  }else{
    mod <- mfv(x)
  }
  n <- length(x)
  m <- mean(x) ## the mean
  s <- sd(x)
  me <- median(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, median=me, mode=mod, stdev=s,
           skewness=skew, kurtosis=kurt))
}

sapply(suicide_data[variables], descriptive_stat)

#df_sample<-data.frame(sapply(suicide_data[variables], descriptive_stat))
#apply(df_sample, 2, round, 2)

#write.csv(df_sample, 'desc.csv')

##Descriptive analysis for each country
country_des <- function(x) sapply(x, descriptive_stat)
by(suicide_data[variables], suicide_data$Country, country_des)

##CORRELATION ANALYSIS
#using the inbuilt R functions and rounding it up to 2 dp
round(cor(suicide_data[variables]), 2)


#checking if the correlation is significant using the Psych package
#this means that the correlation is not just a random occurrence
#complete removes missing observations
#N.B any value less than 0.05 is significant
corr.test(suicide_data[variables], use = 'complete')

##defining the correlation matrix
M= cor(suicide_data[variables])
corrplot(M)

corrplot(M, method = 'shade', order = 'alphabet')
corrplot(M, method = 'square', order = 'alphabet', type = 'lower', diag = F)
## between 0.5 and 0.7 is a strong correlation_Absolute values
##between 0.3 and 0.5 is medium
##less than 0.3 is weak


##REGRESSION ANALYSIS
## Viewing suicide data by countries and dropping year

average_per_country <-
  suicide_data %>%
  select(-Year) %>%
  group_by(Country) %>%
  summarise_all(mean)

View(average_per_country)

##viewing the suicide data per year and dropping the country
average_per_year <-
  suicide_data %>%
  select(-Country) %>%
  group_by(Year) %>%
  summarise_all(mean)

View(average_per_year)


##MULTIPLE Linear Regression (Ordinary Least Square-OLS)
scatterplotMatrix(suicide_data[variables],
                  smooth = FALSE, main='Scatterplot Matrix')

colnames(suicide_data)

#usig this data for reg would not make the model work cos its a perfect cor
#hence, we drop variables that are 80% correlated
#defining h-cor=high correlation values
h_cor<-findCorrelation(cor(suicide_data[variables][,-1]), cutoff = 0.8)
## selecting uncorrelated independent var
un_correlated_vars<- suicide_data[variables][,-1][,-c(h_cor)]

##adding the dependent variable to the uncorrelated variables 
df_regression <- cbind(suicide_data['X..Pop.growth'], un_correlated_vars)
df_regression  #new data for regression

##Linear model

linear_model <- lm(X..Pop.growth ~., data =df_regression)
summary(linear_model)


##Residual is the difference between the observed values and the predicted values.
##for the whole data set. it is called error. but for a population, its called residual

summary(linear_model$residuals)
##F-statistic test the significance of the model. its a type of test of hypothesis
## tells whether the model is goo or not

##for hypothesis testing
#Define your Null Hypothesis (H0)
#Define your Alternative Hypothesis (H1) (researcher's claim)
#Define your level of significance (??):
#Define your test statistic e.g F-statistics
#Decision rule
#Conclusion

#Level of Significance: this is the probability of committing Type I error. Type I error occurs when you
#reject the null hypothesis when it should not be rejected. Default value is 5%.
#Decision Rule: reject null hypothesis if the p-value <= (chosen level of significance), otherwise do not reject

#H0: regression is not significant(means no variable is significantly associated with the suicide result)
#H1: Regression is significant (at least, one variable is significantly associated with suicide)
#assume Alpha is 0.05
#F-statistic = 33.61
#Decision rule: same as above
#conclusion: Since p-value is less than the level of significance(0.05), Hence, we have the statistical
# reason to reject the null hypothesis and conclude that our regression model is significant
#R^2(co efficient of determination): The amount of variation in population growth that is jointly explained by other variables
# About 64% variation in population growth is jointly explained by the independent variables(Multiple R-squared:   0.5705).
#The adjusted R^2 is best to be reported so that if any other variable is added, it will only increase the R value if it has any 
#significance on it. Hence, we change the value to 56%.

##Knowing which variables exactly contributes to the population growth
#thats what the regression model does.... and the formular is 
#Regression model = (3.170e+00 +-5.522e-09  × Population..Male + -1.522e-01 × Suicide.Rate..per.100k.Total.Pop.)
#signif.codes: stating the level of significance. anyone that has at least one asteric is significant
#for Estimate, the is also showing how significant it is.e.g, the positive values of female pop indicates that 
#as the pop increase, the pop growth also increases and so on. and the negative shows that as male pop decreases, the pop growth
#increase(this needs to be queried)
#Interpreting the coefficients: the regression coefficients indicate the increase in the dependent variable for a
#unit change in a predictor variable, holding all other predictor variables constant.
#and this further means that if male population increases by 1000, the population growth would reduce by 
#1000 *  3.170e+00 = 3170 ( should be expressed as a percentage)
#estimate by standard error gives t-value and the t-value is use to calculate the p value
-1.522e-01  * 10 

#Assumptions of Linear Regression
#1. Normality of Residuals: whenever a multiple inear regression, we assume that the error is normally distributed.
#normal Q-Q plot could be used 
#2. Residuals are independent: OLS assumes residuals are not auto-correlated
#3. Linearity: OLS assumes r/ship between X and Y is linear
#4. Homoskedaskicity: your residual should have a constant variance.

par(mfrow=c(2,2))
plot(linear_model)

par(mfrow=c(1,1))

#outlier in linear regression occurs when the predicted value is far from the true value





##TIME SERIES ANALYSIS
#loading libraries
library(forecast) #major library for time series analysis
library(tseries) #for time series
library(xts) #for time series

##Data cleaning
##loading data
Population_data <- read.csv('Population_growth_data.csv')
Population_data

#checking the overview of our columns
glimpse(Population_data)

#selecting the columns needed
Pop_data<-
  Population_data %>%
  select(Time, X..Pop.growth)
Pop_data

##converting our data to time series object, which is an R structure that contains the observations and date specifications.
##using the xts function: it is flexible cos it supports both regular and irregularly structured objects
summary(Pop_data)

date <- seq(from = as.Date('1997/1/1'),
            to = as.Date('2021/1/1'),
            by = 'years')

pop.xts <- xts(Pop_data$X..Pop.growth, date)

#here, pop.xts has now been created as a xts object (i.e time series object)

##proper Time series analysis
#ploting the time series
autoplot(pop.xts)

#time series components
#1. Trend: how does the plot looks overall
#2. Sesonality: do we have some patters repeats itself at a constant season e.g toys sales could have a 12 month seasonality cos
#its sales gets boosted cos of christmas
#3. Cyclic: similar to the above. its like a business cylce
#4. Random Error

##using ggplot-grammar of graphics plot to improve our plots, bolden axis tite, improve the line color etc.
autoplot(pop.xts) +
  geom_line(color='blue') +
  labs(x='Years', y='Population growth (%)', title = 'Time Series Plot')+
  theme_bw() +
  theme(axis.title.x = element_text(size=20, face='bold'),
        axis.text.x = element_text(size=16, face='bold', color = 'blue'),
        axis.title.y = element_text(size=20, face='bold'),
        axis.text.y = element_text(size=16, face='bold', color = 'blue'),
        plot.title = element_text(hjust = 0.5))

##forcasting using ARIMA-autoregressive integrated moving average
#a data is stationary is the mean, covariance and autocovariance is constant
#when the data isnt stationary, the I in ARIMA is introduced

ndiffs(pop.xts)

#since ndiffs gave us a value of 1, it means we need to difference our values once to make our data stationary
##function 'diff' is used to get the lagged differences
#by default, the number if differences is 1, o, we wont be stating it- differencing is the I in ARIMA
dpop<-diff(pop.xts, differences = 1)
autoplot(na.omit(dpop)) #this plot visually confirms if the data is now statinary

Acf(na.omit(dpop))

#a test is also carried out to confirm the stationarity of a data
#this is called ADF (Augmented Dickey-Fuller), and its a formal test for detecting stationarity in Time Series.


adf.test(na.omit(dpop), k=0)

#########################################################################
## In time series, a random walk is defined as series whose first difference 
# is stationary and uncorrelated.hence, carry out this test to know if its random walk

Box.test(dpop, type='Ljung-Box')

auto.arima(pop.xts)




#to fit the arima model, we have to specify the AR, MA and the I components
#ar uses the recent actual value
#autoplot(Acf(dpop)) #to determine the order of MA


#autoplot(Pacf(dpop)) #to determine the order of AR

#because its hard to count lines on each graphs to determine the order, auto-arima helps to achieve that

fit<-auto.arima(pop.xts)
fit

##Goodness of fit of the Arima model; checking if the model is good or not
#four basic things to note. If the model is appropriate
#1. the residuals will be normally distributed
#2. the autocorrelation of the residuals should be zero for all possible lags (white noise)

checkresiduals(fit) ##gives us 3 plots and the Ljung-box test result

##if the residual is normally distributed, then the Jarque bera test should be insignificant
##this means the p-value should be greater than 0.05. Hence, with this, we can say our
#model is normally distributed. Also, the p-value of the Ljung-box test should show uncorrelation, that is p>0.05
#in addition to this, the lag image (the autocorrelation of the residuals) has no signficance as there is no peak
##Checking the normality of the residual
jarque.bera.test(fit$residuals)

#then we can forcast since the residual of the  model is normally distributed
##to forecast into the future
forecast(fit, 5)



autoplot(forecast(fit, 5)) + labs(x='Year', y='Population growth')

##getting the accuracy of the fit- how good is the model in terms of accuracy
accuracy(fit) 
