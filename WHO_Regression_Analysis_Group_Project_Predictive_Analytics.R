
#==============================Functions========================================
capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  print(qnt)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  print(caps)
  H <- 1.5 * IQR(x, na.rm = T)
  print(H)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  print(x)
  return(x)
}
#=================================Libraries=====================================
library(readr)
install.packages('psych')
library(psych)
install.packages("dlookr", dep = TRUE)
library(dlookr)
library(tidyr)
library(mice)
library(tidyverse)
library(caret)
library(caTools)
install.packages("corrr")
library(corrr)
install.packages("corrplot")
library(corrplot)
library(tidyr)
library(olsrr)
#================================Reading File===================================
#Select Life Expectancy-Raw provided as an attachment to this report 
led <- read.csv(file.choose(), sep=',')
#================================Correlation====================================

#to perform correlation on all numerical variables we need to drop 2 categorical columns, we also need to get rid of na values (deleted for now)

led.train.num = subset(led, select = -c(Country, Status))
led.num1<- led.train.num %>% drop_na()
#Correlation without dropping na values
res.cor <- correlate(led.train.num)
res.cor
#Check Correlation of all columns with respect to life expectancy 
res.cor %>% 
  focus(Life.expectancy)

# for those who do not have the library install.packages("corrplot")
corrplot(correlation, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



#=========================Cleaning Pipeline=====================================
#Cleaning Pipeline

#Read Human Development Index file provided as annex to this report 
HDI <- read.csv(file.choose())
HDI[is.na(HDI)] <- 0
led[is.na(led)] <- 0
# Fills income composition of resources missing values for USA and the Bahamas 
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2000 & 
                                      led$Country=='Bahamas']<- HDI$'2000'[HDI$Country=='Bahamas']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2000 & 
                                      led$Country=='United States of America']<- HDI$'2000'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2001 & 
                                      led$Country=='United States of America']<- HDI$'2001'[HDI$Country=='United States']

led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2002 & 
                                      led$Country=='United States of America']<- HDI$'2002'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2003 & 
                                      led$Country=='United States of America']<- HDI$'2003'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2004 & 
                                      led$Country=='United States of America']<- HDI$'2004'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2005 & 
                                      led$Country=='United States of America']<- HDI$'2005'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2006 & 
                                      led$Country=='United States of America']<- HDI$'2006'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2007 & 
                                      led$Country=='United States of America']<- HDI$'2007'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2008 & 
                                      led$Country=='United States of America']<- HDI$'2008'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2009 & 
                                      led$Country=='United States of America']<- HDI$'2009'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2010 & 
                                      led$Country=='United States of America']<- HDI$'2010'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2011 & 
                                      led$Country=='United States of America']<- HDI$'2011'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2012 & 
                                      led$Country=='United States of America']<- HDI$'2012'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2013 & 
                                      led$Country=='United States of America']<- HDI$'2013'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2014 & 
                                      led$Country=='United States of America']<- HDI$'2014'[HDI$Country=='United States']
led$Income.composition.of.resources[led$Income.composition.of.resources==0 & 
                                      led$Year==2015 & 
                                      led$Country=='United States of America']<- HDI$'2015'[HDI$Country=='United States']


#Drops all the columns that are not going to be used
drops<-c("Year","Status","infant deaths","percentage expenditure","Hepatitis B","under-five","GDP","Population","thinness 5-9 years")
led_prep<-led[,!(names(led)%in%drops)]
led_prep[is.na(led_prep)]<-0
#Prepares an empty Data Frame to store cleaned data 
led_clean<-data.frame(Country=character(),
                      "Life.expectancy"=double(),
                      "Adult.Mortality"=double(),
                      "Alcohol"=double(),
                      "BMI"=double(),
                      "Polio"=double(),
                      "Diphteria"=double(),
                      "thinness.5-9.years"=double(),
                      "Income.composition.of.resources"=double(),
                      "Schooling"=double())

#Country vector
countries<-unique(led$Country)
#Imputation of missing values/ Outlier Treatment 
for (country in countries){
  mask <- led_prep %>% filter(Country=="Afghanistan")
  prep_mask<-mask[,-c(1,2)]
  no_outlier_winsor<-winsor(prep_mask,trim=0.37,na.rm=TRUE)
  no_outlier_mask<-cbind(mask[,c(1)],no_outlier_winsor)
  #Mice goes here drop NA as placeholder
  imp <- mice(led_prep, method = "cart", m = 1,ignore=NULL)
  clean_mask<-complete(imp)
  #Append dataframes
  led_clean<-rbind(led_clean,clean_mask)
  
}
warnings()
summary(led_clean)
summary(led)
led_clean_complete<-cbind(led_prep[,c(2)],led_clean)
#Writes the cleaned dataset to disk 
write.csv(led_clean_complete,file.choose(),
          row.names=FALSE)

#=========================Modelling  ===========================================

#Reload cleaned dataset Life Expectancy Data-Clean Final provided as an attachment to the report 
led.original <- read.csv(file.choose(), sep=',')
View(led.original)

led.original1<- led.original %>% drop_na()

#Initial model on Raw data (less GDP, Population and Hep B)
#Model on Raw Data was used a benchmark to evaluate the performance of the cleaned model

names(led.original)
set.seed(142)
dt <- sort(sample(nrow(led.original), nrow(led.original)*0.8))
train.original <- led.original[dt, ]
test.original <- led.original[-dt,]

model1.initial <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+
                       Alcohol+percentage.expenditure+Measles+BMI+Polio+ 
                       Total.expenditure+Diphtheria+thinness.5.9.years+
                       Income.composition.of.resources+Schooling+
                       under.five.deaths + HIV.AIDS + 
                       thinness..1.19.years, data = train.original)
summary(model1.initial)


ols_step_both_p(model1.initial)

model2.initial <-lm(Life.expectancy ~ Adult.Mortality+
                      percentage.expenditure+BMI+Polio+ 
                      Diphtheria+
                      Income.composition.of.resources+Schooling+
                      under.five.deaths + HIV.AIDS, data = train.original)
summary(model2.initial)

#Create a prediction and calculate the R2, RMSE and MAE for training and testing datasets
predictions.initial <- predict(model1.initial, test.original)



validation.train<-data.frame(
  R2<-rsquare(model1.initial,data=train.original),
  RMSE <- sqrt(mse(model1.initial, data=train.original)),
  MAE<-mae(model1.initial, data=train.original),
  MSE<-mse(model1.initial, data=train.original)
)
validation.train


validation.test <- data.frame(
  R2 <- R2(predictions.initial, test.original$Life.expectancy),
  RMSE <- RMSE(predictions.initial, test.original$Life.expectancy),
  MAE <- MAE(predictions.initial, test.original$Life.expectancy)
)
validation.test

#Read previously cleaned dataset

led.cleaned <- read.csv(file.choose(), sep=',')
View(led.cleaned)

#Creating bins for income composition
bins <- c(0, 0.4, 0.6, 0.8, 1.0)
names <- c("Low", "Medium","Medium-High", "High")

led.cleaned$Income.composition.categorical <- cut(led.cleaned$Income.composition.of.resources, breaks = bins, labels = names)
led.cleaned

#Create dataframes for each category so that we can run separate models on each

led.low<-filter(led.cleaned, Income.composition.categorical == "Low")
led.medium<-filter(led.cleaned, Income.composition.categorical == "Medium")
led.medium.high<-filter(led.cleaned, Income.composition.categorical == "Medium-High")
led.high<-filter(led.cleaned, Income.composition.categorical == "High")

#******************************************Models for LOW Income composition group********************************************
set.seed(142)
dt <- sort(sample(nrow(led.low), nrow(led.low)*0.8))
train.low <- led.low[dt, ]
test.low <- led.low[-dt,]

model1.low <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+
                   Alcohol+percentage.expenditure+Measles+BMI+Polio+
                   Total.expenditure+Diphtheria+thinness.one.to.nineteen.years+
                   Income.composition.of.resources+Schooling+
                   under.five.deaths + HIV.AIDS + 
                   thinness.five.to.nine.years, data = train.low)
summary(model1.low)


#perform a stepwise variable selection
ols_step_both_p(model1.low)
#Model with only those variables selected from stepwise
model2.low <- lm(Life.expectancy ~ Adult.Mortality+under.five.deaths+Alcohol+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+HIV.AIDS, data = train.low)
summary(model2.low)


#Polynomials model
model3.low <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+poly(HIV.AIDS, 3, raw = TRUE), data = train.low)
summary(model3.low)

#Polynomial with interactions model
model4.low <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+ HIV.AIDS+I(HIV.AIDS^2)+I(HIV.AIDS^3)+ HIV.AIDS*Adult.Mortality + Alcohol*Adult.Mortality, data = train.low)
summary(model4.low)


model5.low <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+ I(HIV.AIDS^2)+I(HIV.AIDS^3)+ HIV.AIDS*Adult.Mortality +Adult.Mortality*Income.composition.of.resources+ Alcohol*Adult.Mortality, data = train.low)
summary(model5.low)


#look at the statistics for various models selected and determine which one is the best model
glance(model1.low)
glance(model2.low)
glance(model3.low)
glance(model4.low)
glance(model5.low)

#Anova test run to see if there is a significant difference between model4 and model 5
anova(model4.low, model5.low) 

#Compile all models
model_list = list(model1.low,model2.low,model3.low,model4.low,model5.low)
#obtain train RMSE, test RMSE
train_rmse = sapply(model_list, get.rmse, data = train.low, response = "Life.expectancy")
train_rmse
test_rmse = sapply(model_list, get.rmse, data = test.low, response = "Life.expectancy")
test_rmse

#Model 5 obtained the highest performance scores and is used as baseline for prediction/validation

#Predicting model5 on test data for low income countries
predictions.low <- predict(model5.low, test.low)


validation.train.low<-data.frame(
  R2 <- rsquare(model5.low, data = train.low),
  RMSE <- sqrt(mse(model5.low, data=train.low)),
  MAE <- mae(model5.low, data = train.low),
  MSE<-mse(model5.low, data=train.low)
)
validation.train.low
#R2     MAE
#0.8384227 2.23097

#RMSE
# 2.974417

validation.test.low<-data.frame(
  R2 <- R2(predictions.low, test.low$Life.expectancy),
  RMSE <- RMSE(predictions.low, test.low$Life.expectancy),
  MAE <- MAE(predictions.low, test.low$Life.expectancy)
)
validation.test.low

#R2     RMSE      MAE
# 0.8360016 2.436521 2.065909
#**********************************************Models for MEDIUM Income Composition Group ******************************
set.seed(142)
dt <- sort(sample(nrow(led.medium), nrow(led.medium)*0.8))
train.medium <- led.medium[dt, ]
test.medium <- led.medium[-dt,]

model1.medium <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+
                      Alcohol+percentage.expenditure+Measles+BMI+Polio+
                      Total.expenditure+Diphtheria+thinness.one.to.nineteen.years+
                      Income.composition.of.resources+Schooling+
                      under.five.deaths + HIV.AIDS + 
                      thinness.five.to.nine.years, data = train.medium)
summary(model1.medium)
 

#perform a stepwise variable selection
ols_step_both_p(model1.medium)

#Model with only those variables selected from stepwise
model2.medium <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+percentage.expenditure+Total.expenditure+Diphtheria+Income.composition.of.resources+
                      HIV.AIDS, data = train.medium)
summary(model2.medium)


#Polynomials model
model3.medium <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+percentage.expenditure+Total.expenditure+Diphtheria+Income.composition.of.resources+
                      poly(HIV.AIDS, 3, raw = TRUE), data = train.medium)
summary(model3.medium)


#Polynomial with interactions model 
model4.medium <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+percentage.expenditure+Total.expenditure+Diphtheria+Income.composition.of.resources+
                      HIV.AIDS+I(HIV.AIDS^2)+I(HIV.AIDS^3) + HIV.AIDS*Adult.Mortality + Alcohol*Adult.Mortality, data = train.medium)
summary(model4.medium)



glance(model1.medium)
glance(model2.medium)
glance(model3.medium)
glance(model4.medium)

#Model 3 Found to have best performance

#Model 3 and Model 4 compared , model 3 selected based on lower complexity
anova(model4.medium, model3.medium) 

plot(model3.medium)

#Compile all models
model_list = list(model1.medium,model2.medium,model3.medium,model4.medium)

#obtain train RMSE, test RMSE
train_rmse = sapply(model_list, get.rmse, data = train.medium, response = "Life.expectancy")
train_rmse
# 2.985807 3.055568 2.998914 2.991446

test_rmse = sapply(model_list, get.rmse, data = test.medium, response = "Life.expectancy")
test_rmse
#3.878464 3.979557 3.925744 3.914265

#Here we are getting lowest RMSE for model 3 for testing data 

predictions.medium <- predict(model3.medium,test.medium)

validation.train.medium <-data.frame(
  R2 <- rsquare(model3.medium, data = train.medium),
  RMSE <- sqrt(mse(model3.medium, data=train.medium)),
  MAE <- mae(model3.medium, data = train.medium),
  MSE<-mse(model3.medium, data=train.medium)
)

validation.train.medium

#R2             MAE
# 0.8098645    2.22

#RMSE
# 2.998914

validation.test.medium <-data.frame(
  R2 <- R2(predictions.medium, test.medium$Life.expectancy),
  RMSE <- RMSE(predictions.medium, test.medium$Life.expectancy),
  MAE <- MAE(predictions.medium, test.medium$Life.expectancy)
)

validation.test.medium

#    R2     RMSE      MAE
# 0.7447238 3.925744 2.644394

#Model 3 is best model for medium income group


#**********************************Models for MEDIUM_HIGH Income Composition group *********************************

set.seed(142)
dt <- sort(sample(nrow(led.medium.high), nrow(led.medium.high)*0.8))
train.medium.high <- led.medium.high[dt, ]
test.medium.high <- led.medium.high[-dt,]

model1.medium.high <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+
                           Alcohol+percentage.expenditure+Measles+BMI+Polio+
                           Total.expenditure+Diphtheria+thinness.one.to.nineteen.years+
                           Income.composition.of.resources+Schooling+
                           under.five.deaths + HIV.AIDS + 
                           thinness.five.to.nine.years, data = train.medium.high)
summary(model1.medium.high)

#perform a stepwise variable selection

ols_step_both_p(model1.medium.high)

#Model with only those variables selected from stepwise
model2.medium.high <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+BMI+Polio +Total.expenditure+Diphtheria+HIV.AIDS+Income.composition.of.resources
                         , data = train.medium.high)
summary(model2.medium.high)


#Interactions model 
model3.medium.high <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+BMI+Polio +Total.expenditure+Diphtheria+HIV.AIDS+Income.composition.of.resources+ HIV.AIDS*Adult.Mortality + Alcohol*Adult.Mortality, data = train.medium.high)
summary(model3.medium.high)

glance(model1.medium.high)
glance(model2.medium.high)
glance(model3.medium.high)


#Model 3 seems best model with highest R square value and lowest AIC

anova(model3.medium.high, model2.medium.high) 
plot(model3.medium.high)

#Compile all models
model_list = list(model1.medium.high,model2.medium.high,model3.medium.high)
#obtain train RMSE, test RMSE
train_rmse = sapply(model_list, get.rmse, data = train.medium.high, response = "Life.expectancy")
train_rmse
# 2.190066 2.200370 2.172461

test_rmse = sapply(model_list, get.rmse, data = test.medium.high, response = "Life.expectancy")
test_rmse
#2.454066 2.468679 2.434401
#From above above, RMSE for test data is lowest for model 3. Therefore, model3 fits test data best.


predictions.medium.high <- predict(model3.medium.high,test.medium.high)

validation.train.medium.high <-data.frame(
  R2 <- rsquare(model3.medium.high, data = train.medium.high),
  RMSE <- sqrt(mse(model3.medium.high, data=train.medium.high)),
  MAE <- mae(model3.medium.high, data = train.medium.high),
  MSE<-mse(model3.medium.high, data=train.medium.high)
)

validation.train.medium.high

#R2      MAE
# 0.7543232 1.648939

#RMSE
# 2.172461

validation.test.medium.high <-data.frame(
  R2 <- R2(predictions.medium.high, test.medium.high$Life.expectancy),
  RMSE <- RMSE(predictions.medium.high, test.medium.high$Life.expectancy),
  MAE <- MAE(predictions.medium.high, test.medium.high$Life.expectancy)
)

validation.test.medium.high

#R2     RMSE      MAE
# 0.6927767 2.434401 1.845505

validation.test.medium.high <-data.frame(
  R2 <- R2(predictions.medium.high, test.medium.high$Life.expectancy),
  RMSE <- RMSE(predictions.medium.high, test.medium.high$Life.expectancy),
  MAE <- MAE(predictions.medium.high, test.medium.high$Life.expectancy)
)

validation.test.medium.high


#Model 3 fits testing data well, RMSE is low on test data whereas MAE is high in test data.
#*****************************************************Models for HIGH Income Composition group *************************


set.seed(142)
dt <- sort(sample(nrow(led.high), nrow(led.high)*0.8))
train.high <- led.high[dt, ]
test.high <- led.high[-dt,]

model1.high <- lm(Life.expectancy ~ Adult.Mortality+infant.deaths+
                    Alcohol+percentage.expenditure+Measles+BMI+Polio+
                    Total.expenditure+Diphtheria+thinness.one.to.nineteen.years+
                    Income.composition.of.resources+Schooling+
                    under.five.deaths + HIV.AIDS + 
                    thinness.five.to.nine.years, data = train.high)
summary(model1.high)
ols_vif_tol(model1.high)

#After checking multicollinearity, removed infant deaths, thinness one to 19 years
model1.high <- lm(Life.expectancy ~ Adult.Mortality+
                    Alcohol+percentage.expenditure+Measles+BMI+
                    Total.expenditure+Diphtheria+
                    Income.composition.of.resources+Schooling+
                    under.five.deaths + HIV.AIDS + 
                    thinness.five.to.nine.years, data = train.high)
summary(model1.high)
# R2 value is 0.5377 with no multicollinearity

ggplot(led.high, aes(Adult.Mortality, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Income.composition.of.resources, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Schooling, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(HIV.AIDS, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Total.expenditure, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(under.five.deaths, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Alcohol, Life.expectancy)) + 
  geom_jitter()

ggplot(led.high, aes(Total.expenditure, Life.expectancy)) + 
  geom_jitter()

#perform a stepwise variable selection

ols_step_both_p(model1.high)

#Model with only those variables selected from stepwise
model2.high <- lm(Life.expectancy ~ Adult.Mortality+under.five.deaths+Measles+Alcohol+Total.expenditure+thinness.five.to.nine.years+Diphtheria+Income.composition.of.resources, data = train.high)
summary(model2.high)


#With interactions model
model3.high <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+
                    Alcohol*Adult.Mortality, data = train.high)
summary(model3.high)


model4.high <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+
                    Adult.Mortality*Income.composition.of.resources, data = train.high)
summary(model4.high)


#With polynomial term for income composition of resources, on scatter plot there seems to be a curvilinear relationship between income composition and Life Expectancy
model5.high <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+
                    I(Alcohol^2) + I(Income.composition.of.resources^2) , data=train.high)

summary(model5.high)
#Adding  poly term for income, improved the model. R2 value is improved to 0.5422


glance(model1.high)
glance(model2.high)
glance(model3.high)
glance(model4.high)
glance(model5.high)

#Model 5 Obtained best performance scores with highest R square value of 0.542 and lowest AIC 2308


predictions.high <- predict(model5.high,test.high)

validation.train.high <-data.frame(
  R2 <- rsquare(model5.high, data = train.high),
  RMSE <- sqrt(mse(model5.high, data=train.high)),
  MAE <- mae(model5.high, data = train.high),
  MSE<-mse(model5.high, data=train.high)
)

validation.train.high

#R2      MAE
# 0.5421922 1.698369

validation.test.high <-data.frame(
  R2 <- R2(predictions.high, test.high$Life.expectancy),
  RMSE <- RMSE(predictions.high, test.high$Life.expectancy),
  MAE <- MAE(predictions.high, test.high$Life.expectancy)
)

validation.test.high


#R2     RMSE      MAE
# 0.5619784 2.518997 1.749626

#Errors RMSE is low for test, while MAE is high for test data as compared to train data

#Model 5 fits testing data well like training data with high R2 value on testing data



#=========================Model Validation======================================


#Compare the two models on the initial full dataset (before cleaning)
library(broom)
glance(model1.initial)
glance(model2.initial)

#Compare models for Low income countries
glance(model1.low)
glance(model2.low)
glance(model3.low)
glance(model4.low)
glance(model5.low)
#Model 5 is best for low income countries

#Compare models for Medium income countries
glance(model1.medium)
glance(model2.medium)
glance(model3.medium)
glance(model4.medium)
#Model3 is best for medium income countries

#Compare models for Medium - High income countries
glance(model1.medium.high)
glance(model2.medium.high)
glance(model3.medium.high)
#Model3 is best for medium high countries

#Compare models for High income countries
glance(model1.high)
glance(model2.high)
glance(model3.high)
glance(model4.high)
glance(model5.high)
#Model 5 is best for high income countries

#Import final cleaned full dataset
#Run best models from each bin
#Compare performance and select the final model
led.cleaned <- read.csv(file.choose())
set.seed(142)
dt <- sort(sample(nrow(led.cleaned), nrow(led.cleaned)*0.8))
train.cleaned <- led.cleaned[dt, ]
test.cleaned <- led.cleaned[-dt,]

#  ------------------------------1st model (low income model)----------------------------
m1 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+ I(HIV.AIDS^2)+I(HIV.AIDS^3)+ HIV.AIDS*Adult.Mortality +Adult.Mortality*Income.composition.of.resources+ Alcohol*Adult.Mortality, data = train.cleaned)
summary(m1)
#R2 value is 0.8838

# -------------------------------2nd model (medium income model) -----------------------------
m2 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+percentage.expenditure+Total.expenditure+Diphtheria+Income.composition.of.resources+
           poly(HIV.AIDS, 3, raw = TRUE), data = train.cleaned)
summary(m2)
#R2 value is 0.8743

# ------------------------------3rd model (medium high income model) ------------------------------
m3 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+percentage.expenditure+Total.expenditure+Diphtheria+Income.composition.of.resources+
           poly(HIV.AIDS, 3, raw = TRUE), data = train.cleaned)
summary(m3)
#R2 value is 0.8743

# -----------------------------4th model (high income model) -------------------------------
m4 <- lm(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+
           I(Alcohol^2) + I(Income.composition.of.resources^2) , data=train.cleaned)

summary(m4)
# R2 value is 0.8687

glance(m1)
glance(m1)
glance(m3)
glance(m4)
#R2 is highest for model 1 and AIC is lowest for model1

#Compile all models
model_list = list(m1,m2,m3, m4)
#obtain train RMSE, test RMSE
train_rmse = sapply(model_list, get.rmse, data = train.cleaned, response = "Life.expectancy")
train_rmse
# 3.265068 3.396293 3.396293 3.471133

test_rmse = sapply(model_list, get.rmse, data = test.cleaned, response = "Life.expectancy")
test_rmse
#3.121948 3.170139 3.170139 3.075824

#Difference between RMSE of trained and test is lowest for model 1 i.e low income group model

#Best model for cleaned dataset would be the model for low income group
predictions.cleaned <- predict(m1,test.cleaned)

validation.train.cleaned<-data.frame(
  R2 <- rsquare(m1, data = train.cleaned),
  MAE <- mae(m1, data = train.cleaned),
  MSE<-mse(m1, data=train.cleaned),
  RMSE <- sqrt(mse(m1, data=train.cleaned)))

validation.train.cleaned
#R2             RMSE       MAE
# 0.8837942     3.265068    2.382841


validation.test.cleaned <-data.frame(
  R2 = R2(predictions.cleaned, test.cleaned$Life.expectancy),
  RMSE = RMSE(predictions.cleaned, test.cleaned$Life.expectancy),
  MAE = MAE(predictions.cleaned, test.cleaned$Life.expectancy)
)

validation.test.cleaned
#R2             RMSE      MAE
# 0.8870816   3.121948 2.271015

#Testing data has low errors and higher R2 as compared to train data

#**********************************VALIDATE THE MODEL USING K-FOLD *********************************


# Define training control
set.seed(123) 
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# Train the model
model <- train(Life.expectancy ~ Adult.Mortality+Alcohol+under.five.deaths+percentage.expenditure+BMI+Total.expenditure+Diphtheria+thinness.five.to.nine.years+Income.composition.of.resources+ I(HIV.AIDS^2)+I(HIV.AIDS^3)+ HIV.AIDS*Adult.Mortality +Adult.Mortality*Income.composition.of.resources+ Alcohol*Adult.Mortality, data = train.cleaned, method = "lm", trControl = train.control)

# Summarize the results
print(model)

#  RMSE      Rsquared   MAE     
#  3.295351  0.8824259  2.404237


#=========================DATA EXPLORATION=====================================
#diagnose the variables more deeply 
diagnose(led)
summary(led)

#diagnose the variables more deeply 
diagnose_category(led)

diagnose_numeric(led)

diagnose_outlier(led)

#Missing values table
sort(sapply(led,function(x) sum(is.na(x))),decreasing = T)
md.pattern(led)

#Dropping 10 missing values of target variable
led<- led %>% drop_na(Life.expectancy)

#All histograms of numerical variables
names<-names(led)
classes<-sapply(led,class)
for(name in names[classes == 'numeric'])
{
  dev.new()
  hist(led[,name]) # subset with [] not $
} 
 
#Barplot and table for categorical variable       
barplot(table(led$Status))
table(led$Status) #Developed 512 and developing 2426

#Box plots for continous variables:
ggplot(data=led) + geom_boxplot(aes(x=Adult.Mortality), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=infant.deaths), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=BMI), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=GDP), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=population), size=1.2)

ggplot(data=led) + geom_boxplot(aes(x=Life.expectancy), size=1.2)

#Scatter plots with target variable
ggplot(led, aes(GDP, Life.expectancy)) + 
  geom_jitter()

ggplot(led, aes(GDP, Life.expectancy, color = Year)) + 
  geom_jitter()

ggplot(led, aes(Adult.Mortality, Life.expectancy)) + 
  geom_jitter()

ggplot(led, aes(Income.composition.of.resources, Life.expectancy)) + 
  geom_jitter()

ggplot(led, aes(Schooling, Life.expectancy)) + 
  geom_jitter()

ggplot(led, aes(HIV.AIDS, Life.expectancy)) + 
  geom_jitter()
            
#Boxplot of life expectancy on the basis of status of country
ggplot(data = led, aes(x=Status, y=Life.expectancy, fill = Status)) + geom_boxplot()
#Life Expectancy is high for developed countries as already expected

#Cheking difference in means of developed and developing countries using ANOVA
summary(aov(Life.expectancy ~ Status, data = led))
#p-value is less than 0.05 - reject null hypothesis
#There is significant differences in life expectancy for developed and developing countries

#Correlation
data_num <- led %>% 
  select_if(is.numeric)
cor(data_num, use="complete.obs")

#Life expenctancy has strong positive correlation with Schooling and Income composition of resources
#And negative strong correlation with Adult mortality
#Weak correlation with population and measles
#Strong correlation between infant deaths and under 5 deaths - multicollinearity
#Therefore removing under 5 deaths

#Hepatitis, Polio and diphtheria is converted into 2 groups according to World health assembly rule by 2020
#Under 90% and above 90%

led3 <- led %>% 
  select(-Country, -Year) %>%
  mutate(Hepatitis.B = ifelse(Hepatitis.B < 90, "<90% Covered", ">=90% Covered"),
         Polio = ifelse(Polio < 90, "<90% Covered", ">=90% Covered"),
         Diphtheria = ifelse(Diphtheria < 90, "<90% Covered", ">=90% Covered"),
         Hepatitis.B = as.factor(Hepatitis.B),
         Polio = as.factor(Polio),
         Diphtheria = as.factor(Diphtheria))
str(led3)
table(led3$Hepatitis.B)
table(led3$Polio)
table(led3$Diphtheria)

#Box plot of life expectancy on hepatitis
library(ggplot2)
ggplot(data = led3, aes(x=Hepatitis.B, y=Life.expectancy, fill = Hepatitis.B)) + geom_boxplot()
#anova
summary(aov(Life.expectancy ~ Hepatitis.B, data = led3)) #p less than 0.05

#Box plot of life expectancy on polio
ggplot(data = led3, aes(x=Polio, y=Life.expectancy, fill = Polio)) + geom_boxplot()
summary(aov(Life.expectancy ~ Polio, data = led3)) #p less than 0.05
dev.off()

#Box plot of life expectancy on Diphtheria
ggplot(data = led3, aes(x=Diphtheria, y=Life.expectancy, fill = Diphtheria)) + geom_boxplot()
summary(aov(Life.expectancy ~ Diphtheria, data = led3)) #p less than 0.05


