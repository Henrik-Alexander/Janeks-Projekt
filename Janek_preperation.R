###preperation
library(readr)
library(tidyverse)
library(stats)
library(ggplot)
library(moments)

###loading the data
data <- read.csv("Janek-Data.csv", sep=";") 

###String replacement in the country variable
data$number_pattern <- "_[0-9]*"
data$Country <- str_replace(data$Country, number_pattern, replacement = "")
data$GNI <- str_replace_all(data$GNI, pattern = "\\$", replacement = "") 
data$GNI <- str_replace_all(data$GNI, pattern = "(.00)", replacement = "") 
data$Population <- as.numeric(data$Population)
data$GNI <- as.numeric(data$GNI)


###plotting the data per continent
###point
ggplot(data, aes(Demo_ind, e_par))+
  geom_point(aes(colour=Continent, size=Population))+
  facet_wrap(~ Year)
###drop missings
data %>% filter(e_par!= is.na & Demo_ind != is.na)
#scatterplot
plot(data$Demo_ind, data$e_par)
abline(main.effect, col = "red")
#correlation
corrplot::colorlegend(data)

###linear model
main.effect <- lm(e_par ~ Demo_ind, data=data)
summary(main.effect)
###predicting values
plot(data$Demo_ind, data$e_par)
abline(Demomain.effect$coefficients[1] + x * main.effect$coefficients[2])


##including controls
model.lm <- lm(e_par ~ Demo_ind + Year  + Population  + GNI, data = data)
#model results
summary(model.lm)
#coefficients
coef(model.lm)
#prediction
data$prediction <- predict(model.lm, data.frame(data))
#plotting the predicted values
plot(data$Demo_ind, data$prediction)
abline(a = 0.793 , b = 0.417, col="red", lwth=2)
#calculation of the intercept
-944.9413+0.47*2011.5+0.0000000035*14690000+993*0.00028

###checking for regression assumptions
#normal distribution of errors
x <- model.lm$residuals
h <- hist(x, breaks = 30, xlab = "Model Residuals", main = "Postregression Estimattion")
xfit<- seq(min(x), max(x), length =2100 )
yfit <- dnorm(xfit, mean=mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col = "red")

##residuals are approximately normally distributed
sd(model.lm$residuals)
#qq plot
qqnorm(model.lm$residuals);qqline(model.lm$residuals)  ##normal distribution is confirmed
    
#check on symmetry of distribution skewness
postregression <- summary(model.lm$residuals)
Q1 <- postregression[2]
Q2 <- postregression[3]
Q3 <- postregression[5]
skewness <- ((Q1-Q2)+(Q3-Q2))/(Q3-Q1)
skewness  ###slight right-skewed

###Kurtosis
kurtosis(model.lm$residuals)n ##distribution is platykurtic

#smoothed values
graph <- list(unique(data$Year))
data <- data %>% arrange( Year)


 #boxplot
data %>% boxplot.stats(Demo_ind)

  