setwd("C:/Users/drag88/Documents/GitHub/Machine Learning_AS/Linear Regression")

#loading packages
library(tidyverse)
library(caret)
library(GGally)
library(ISLR)
library(corrplot)
library(gvlma)
library(caTools)

# Multiple Regression without qualitative variables -----------------------------------------------------

## Here's the data
advert <- read.csv("Advertising.csv")

#######################################################
##
## Let's start by reviewing simple regression by 
## modeling Sales  with impact on TV , radio and newspaper
##
#######################################################

## STEP 1: Checking pairwise relationship between variables

ggpairs(advert)

## STEP 2: Do the regression
lm_model = lm(Sales ~ TV+Radio+ Newspaper + TV*Radio, data=advert)

## STEP 3: Look at the R^2, F-value and p-value
summary(lm_model)


## now let's verify that our formula for R^2 is correct..
ss.mean <- sum((advert$Sales - mean(advert$Sales))^2)
ss.model <- sum(lm_model$residuals^2)

(ss.mean - ss.model) / ss.mean # this is the R^2 value

## now let's verify the our formula for F is correct...
f.model <- ((ss.mean - ss.model) / (5 - 1)) / 
    (ss.model/ (nrow(advert) - 5))

f.simple # this is the F-value

#checking the plots to verify the linear model assumptions
#interpretation for the graphs can be found at >> https://data.library.virginia.edu/diagnostic-plots/

plot(lm_model) 


# Multiple Regression with qualitative variables --------------------------
##loading the data set
carseats = ISLR::Carseats

# running a linear regression model
set.seed(101) 
sample = sample.split(carseats$Sales, SplitRatio = .8)
train = subset(carseats, sample == TRUE)
test  = subset(carseats, sample == FALSE)


lm_model_1 =lm(Sales???.+ Income:Advertising + Price:Age, data=train )
summary(lm_model_1)
plot(lm_model_1)

salespred = predict(lm_model_1, test)

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=test$Sales, predicteds=salespred))  
actuals_preds = actuals_preds %>%
    filter(actuals > 0 | predicteds >0 )
correlation_accuracy <- cor(actuals_preds)

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)


