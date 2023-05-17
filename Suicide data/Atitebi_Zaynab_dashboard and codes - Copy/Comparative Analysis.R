##t-test and ANOVA are used forcomparative analysis
#for 2 groups, use t-test, for more than 2, use ANOVA
library(tidyverse)
suicide_data <- read.csv('Suicide_Data_R1.csv')
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


##testing assumptions of equal variance-there are diff test, this is the most common
##this should be done before the t-test
bartlett.test(suicide_data$Suicide.Rate..per.100k.Total.Pop. ~ suicide_data$classification)
##null hypothesis of the test: variances are equal
#Alternative hypothesis: variances are not equal

##p-value < 0.05: means that our variance is non-homogeneous

##Hence in the t-test, we set var.equal to false, which is the default



##T-test (Independent sample t-test)
#dependent variable is the numeric var we are comparing-the suicide rate, the independent var is the 
#classification we just did
class_test <- t.test(suicide_data$Suicide.Rate..per.100k.Total.Pop. ~ suicide_data$classification)
class_test

##using analysis of variance test
#fit<-aov(suicide_data$Suicide.Rate..per.100k.Total.Pop. ~ suicide_data$classification)
#summary(fit)


##result shows that the 2 groups are significantly different as our p<0.05
##seeing how they are different using plot
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
