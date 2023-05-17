##load libraries
library(tidyverse)
library(psych) 
library(modeest)
library(caret)
library(corrplot)
library(car) 

##DESCRIPTIVE STATISTICS - TASK 1
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



##CORRELATION ANALYSIS- TASK 2
#using the inbuilt R functions and rounding it up to 2 dp
round(cor(suicide_data[variables]), 2)


#checking if the correlation is significant using the Psych package
#N.B any value less than 0.05 is significant
corr.test(suicide_data[variables], use = 'complete')

##defining the correlation matrix
M= cor(suicide_data[variables])
corrplot(M)

corrplot(M, method = 'shade', order = 'alphabet')
corrplot(M, method = 'square', order = 'alphabet', type = 'lower', diag = F)



##REGRESSION ANALYSIS - TASK 3
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

#usiNg this data for reg would not make the model work cos its a perfect cor
#hence, I dropped variables that are 80% correlated

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

#checking the residuals alone
summary(linear_model$residuals)


##for hypothesis testing
#Define your Null Hypothesis (H0)
#Define your Alternative Hypothesis (H1) (researcher's claim)
#Define your level of significance (??):
#Define your test statistic e.g F-statistics
#Decision rule
#Conclusion

##plotting the model
par(mfrow=c(2,2))
plot(linear_model)

##returning back to default
par(mfrow=c(1,1))





##TIME SERIES ANALYSIS -TASK 4
#loading libraries
library(forecast) 
library(tseries) 
library(xts) 

##Data cleaning
#loading data
Population_data <- read.csv('Population_growth_data.csv')
Population_data

#checking the overview of the columns
glimpse(Population_data)

#selecting the columns needed
Pop_data<-
  Population_data %>%
  select(Time, X..Pop.growth)
Pop_data

#converting the data to time series object, which is an R structure that contains the observations and date specifications.
summary(Pop_data)

date <- seq(from = as.Date('1997/1/1'),
            to = as.Date('2021/1/1'),
            by = 'years')

pop.xts <- xts(Pop_data$X..Pop.growth, date)

#here, pop.xts has now been created as a xts object (i.e time series object)

#plotting the time series
autoplot(pop.xts)

##using ggplot to improve the above plot
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

#checking the number of differencing needed
ndiffs(pop.xts)

#differencing once
dpop<-diff(pop.xts, differences = 1)

#confirming if the series is stationary
autoplot(na.omit(dpop)) #1

Acf(na.omit(dpop)) #2

#a test is also carried out to confirm the stationarity of a data
#this is called ADF (Augmented Dickey-Fuller), and its a formal test for detecting stationarity in Time Series

adf.test(na.omit(dpop), k=0) #3


#confirming the time series is a random walk
Box.test(dpop, type='Ljung-Box') #1

auto.arima(pop.xts) #2
fit<-auto.arima(pop.xts)
fit

#checking the goodness of fit of the ARIMA model
checkresiduals(fit) ##gives us 3 plots and the Ljung-box test result

##testing the normality of the residual
jarque.bera.test(fit$residuals)

#then we can forecast since the residual of the  model is normally distributed
forecast(fit, 5)

#plotting the forecast
autoplot(forecast(fit, 5)) + labs(x='Year', y='Population growth')

##getting the accuracy of the fit
accuracy(fit) 



##Comparative Analysis -TASK 5
#loading library
library(tidyverse)

#loading data
suicide_data <- read.csv('Suicide_Data_R1.csv')

#data wrangling
#getting the unique countries
unique(suicide_data$Country)

##dividing the countries into developed and underdeveloped class
developed_class <- c('France', 'Germany', 'Japan', 'United Kingdom', 'United States')
underdeveloped_class <- c('Afghanistan', 'Angola', 'Bangladesh', 'Ethiopia', 'Sudan')

class <- c()

for (country in unique(suicide_data$Country)) {
  if (country %in% developed_class){
    class <- c(class, rep('dev', 10))
  } else {
    class <- c(class, rep('underdev', 10))
  }
}
class

##adding the new classification column to the data
suicide_data$classification <- class
View(suicide_data)


##testing assumptions of equal variance using Bartlett test
bartlett.test(suicide_data$Suicide.Rate..per.100k.Total.Pop. ~ suicide_data$classification)

##T-test (Independent sample t-test)
class_test <- t.test(suicide_data$Suicide.Rate..per.100k.Total.Pop. ~ suicide_data$classification)
class_test


##result shows that the 2 groups are significantly different as our p<0.05
##checking the level of difference using plot
plotdata <- suicide_data %>% 
  group_by(classification) %>% 
  summarise(n=n(),
            mean=mean(Suicide.Rate..per.100k.Total.Pop.),
            sd = sd(Suicide.Rate..per.100k.Total.Pop.),
            ci = qt(0.975, df = n-1)* sd/sqrt(n)) ##confidence interval formula(its called margin of error)
plotdata

##plotting the mean vs classification
ggplot(plotdata,
       aes(x=classification, y=mean, group=1))+
  geom_point(size=5, color='darkblue') +
  geom_line(linetype='dashed', color='darkgrey')+
  geom_errorbar(aes(ymin = mean-ci,
                    ymax=mean+ci),
                width=0.1) +
  theme_bw()+
  labs(x='Class', y='Suicide rate',
       title = 'Mean Plot with 95% confidence interval')
