###preperation
rm(list=ls())
library(readr)
library(tidyverse)
library(stats)
library(ggplot)
library(moments)
library(RCurl)
library(ggiraphExtra)
library(ggeffects)
library(ggrepel)


###loading the data
setwd("C:/Users/henri/OneDrive/Desktop/Janeks-Projekt-main")
data <- read.csv("Janek-Data.csv", sep=";")

###String replacement in the country variable
number_pattern <- "_[0-9]*"
data$Country <- str_replace(data$Country, number_pattern, replacement = "")
data$GNI <- str_replace_all(data$GNI, pattern = "\\$", replacement = "") 
data$GNI <- str_replace_all(data$GNI, pattern = "(.00)", replacement = "") 
data$Population <- as.numeric(data$Population)
data$GNI <- as.numeric(data$GNI)
data$e_par <-   str_replace(data$e_par, pattern = ",", replacement = ".")
data$Demo_ind <- str_replace(data$Demo_ind, pattern = ",", replacement = ".")
data$e_par <- as.numeric(data$e_par)
data$Demo_ind <- as.numeric(data$Demo_ind)
data <- data %>% select(-Regimetype)
data$Yeart <- is.numeric(data$Year)

#creating regimecluster
data <- data %>% mutate(regimetype = case_when(Demo_ind < 5 ~ 1,
                                        Demo_ind >=5 & Demo_ind < 6 ~ 2,
                                        Demo_ind >=6 & Demo_ind < 8 ~ 3,
                                        Demo_ind >= 8 ~ 4))
data$regimetype <- factor(data$regimetype, levels = c(1,2,3,4), labels=c("Autokratie", "Hybrides System", "Defekte Demokratie", "Demokratie"))
###plotting the data per continent
data %>%  ggplot(aes(e_par, fill = regimetype))+
  geom_histogram(bins = 30)+
  facet_wrap(~ Year)
#Development over time
ggplot(data, aes(Year, e_par, group = Country))+
  geom_line(alpha = 0.5, aes( color = "Country", size = "Country"))+
  geom_line(stat = "smooth", method = "loess", aes(group = Continent, color = "Continent", size = "Continent"), alpha = 0.5)+
  facet_wrap(~ Continent)+
  scale_color_manual(name = "E-Participation, for: ", values = c("Country" = "blue", "Continent" = "red"))+
  scale_size_manual(name = "E-Participation, for: ", values = c("Country" = 0.25, "Continent" = 3))+
  theme_minimal(base_size = 14)+ ylab("E Participation")+ xlab("Year")+
  ggtitle("E Participation, 2007-2016", subtitle = "By Continent and country")
  
#Aggregate data by continent and year
continent <- data %>% group_by(Continent, Year) %>%  summarize(mean_egov = mean(e_par),
                                               mean_demo = mean(Demo_ind))
ggplot(continent, aes(Year, mean_egov, colour = Continent))+
  geom_step(size = 2, alpha = 0.8)+
  theme(legend.position = "bottom")+
  theme_bw()+
  labs(title= "Entwicklung der E-Partizipation nach Kontinent (2006-2016)")+
  ylab("E-Partizipation")

###aggregated by regimetype
regime <- data %>% group_by(regimetype, Year) %>%  summarize(mean_egov = mean(e_par),
                                                            number = n())
regime  %>% ggplot(aes(Year, mean_egov, colour= regimetype))+
  geom_line(size=2)+
  geom_label(aes(Year, mean_egov, label = number))

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
model.lm <- glm(e_par ~ Demo_ind + Year  + Population  + GNI, data = data)
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

###interaction between time and democracy
model2 <- lm(e_par ~  regimetype*Year + GNI + Population , data = data)
ggpredict(model2, terms = c( "Year", "regimetype")) %>% plot() + ggtitle("Predicted values of E-participation") + labs(caption="Model includes regimtype, Year, Interaction of year and Regimetype, GNI")



  