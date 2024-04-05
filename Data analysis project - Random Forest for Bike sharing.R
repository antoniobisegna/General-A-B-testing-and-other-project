#loading the required libraries
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

# reading the data files
train=read.csv("train.csv")
test=read.csv("test.csv")
str(train)

# introducing variables in test to combine train and test
# can also be done by removing the same variables from training data
test$registered=0
test$casual=0
test$count=0
data=rbind(train,test)
View(data)

# getting some information about the combined data
str(data)
summary(data)

# 4. Find missing values in data set if any
table(is.na(data))
# no missing values in the data frame.

# 5. Understand the distribution of numerical 
# variables and generate a frequency table for
# numeric variables.  Now, I???ll test and plot 
# a histogram for each numerical variables and
# analyze the distribution.

par(mfrow=c(4,2))
par(mar = rep(2, 4))

hist(data$season,col=rgb(0.2,0.8,0.5,0.5), border = F)
hist(data$weather,col=rgb(0.2,0.8,0.5,0.5), border = F)
hist(data$humidity,col=rgb(0.2,0.8,0.5,0.5), border = F)
hist(data$holiday,col=rgb(0.2,0.8,0.5,0.5), border = F)
hist(data$workingday,col=rgb(0.2,0.8,0.5,0.5), border = F)
hist(data$temp,col=rgb(0.2,0.8,0.5,0.5), border = F)
hist(data$atemp,col=rgb(0.2,0.8,0.5,0.5), border = F)
hist(data$windspeed,col=rgb(0.2,0.8,0.5,0.5), border = F)
# Few inferences can be drawn by looking at the these histograms:
# - Season has four categories of almost equal distribution
# - Weather 1 has higher contribution i.e. mostly clear weather.
prop.table(table(data$weather))

# As expected, mostly working days and variable holiday is also
# showing a similar inference. You can use the code above to 
# look at the distribution in detail. Here you can generate a 
# variable for weekday using holiday and working day. 
# Incase, if both have zero values, then it must be a working day.
# - Variables temp, atemp, humidity and windspeed  looks naturally
# distributed.

# factoring some variables from numeric
data$season=as.factor(data$season)
data$weather=as.factor(data$weather)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)

# extracting hour from the datetime variable
data$hour=substr(data$datetime,12,13)
data$hour=as.factor(data$hour)

# dividing again into train and test
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

# creating some boxplots on the count of rentals
boxplot(train$count~train$hour,xlab="hour", ylab="count of users")
boxplot(train$casual~train$hour,xlab="hour", ylab="casual users")
boxplot(train$registered~train$hour,xlab="hour", ylab="registered users")

# Log version
par(mfrow=c(1,1))
boxplot(log(train$count)~train$hour,xlab="hour",ylab="log(count)")

# extracting days of week from datetime
date=substr(data$datetime,1,10)
days<-weekdays(as.Date(date))
data$day=days

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

# creating boxplots for rentals with different variables to see the variation
ggplot(train, aes(x=as.factor(train$day), y=train$registered)) + 
  geom_boxplot(fill="greenyellow", alpha=0.2) + 
  xlab("day")+
  ylab("registered")
ggplot(train, aes(x=as.factor(train$day), y=train$casual)) + 
  geom_boxplot(fill="greenyellow", alpha=0.2) + 
  xlab("day")+
  ylab("casual")

ggplot(train, aes(x=as.factor(train$weather), y=train$registered)) + 
  geom_boxplot(fill="red", alpha=0.2) + 
  xlab("weather")+
  ylab("registered")
ggplot(train, aes(x=as.factor(train$weather), y=train$casual)) + 
  geom_boxplot(fill="red", alpha=0.2) + 
  xlab("weather")+
  ylab("casual")

ggplot(train, aes(x=as.factor(train$temp), y=train$registered)) + 
  geom_boxplot(fill="blue", alpha=0.2) + 
  xlab("temp")+
  ylab("registered")
ggplot(train, aes(x=as.factor(train$temp), y=train$casual)) + 
  geom_boxplot(fill="blue", alpha=0.2) + 
  xlab("temp")+
  ylab("casual")

# extracting year from data
data$year=substr(data$datetime,1,4)
data$year=as.factor(data$year)

# ignore the division of data again and again, this could have been done together also
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

# again some boxplots with different variables
# these boxplots give important information about the dependent variable with respect to the independent variables
ggplot(train, aes(x=as.factor(train$year), y=train$registered)) + 
  geom_boxplot(fill="gray31", alpha=0.2) + 
  xlab("year")+
  ylab("registered")
ggplot(train, aes(x=as.factor(train$year), y=train$casual)) + 
  geom_boxplot(fill="gray31", alpha=0.2) + 
  xlab("year")+
  ylab("casual")

ggplot(train, aes(x=as.factor(train$windspeed), y=train$registered)) + 
  geom_boxplot(fill="aquamarine", alpha=0.2) + 
  xlab("windspeed")+
  ylab("registered")
ggplot(train, aes(x=as.factor(train$windspeed), y=train$casual)) + 
  geom_boxplot(fill="aquamarine", alpha=0.2) + 
  xlab("windspeed")+
  ylab("casual")

ggplot(train, aes(x=as.factor(train$humidity), y=train$registered)) + 
  geom_boxplot(fill="mediumpurple", alpha=0.2) + 
  xlab("humidity")+
  ylab("registered")
ggplot(train, aes(x=as.factor(train$humidity), y=train$casual)) + 
  geom_boxplot(fill="mediumpurple", alpha=0.2) + 
  xlab("humidity")+
  ylab("casual")


data$hour=as.integer(data$hour)

# created this variable to divide a day into parts, but did not finally use it
data$day_part=0

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

data=rbind(train,test)

#using decision trees for binning some variables, 
#this was a really important step in feature engineering

d=rpart(registered~hour,data=train)
fancyRpartPlot(d)

d=rpart(casual~hour,data=train)
fancyRpartPlot(d)

data=rbind(train,test)
data$dp_reg=0
data$dp_reg[data$hour<8]=1
data$dp_reg[data$hour>=22]=2
data$dp_reg[data$hour>9 & data$hour<18]=3
data$dp_reg[data$hour==8]=4
data$dp_reg[data$hour==9]=5
data$dp_reg[data$hour==20 | data$hour==21]=6
data$dp_reg[data$hour==19 | data$hour==18]=7

data$dp_cas=0
data$dp_cas[data$hour<=8]=1
data$dp_cas[data$hour==9]=2
data$dp_cas[data$hour>=10 & data$hour<=19]=3
data$dp_cas[data$hour>19]=4

f=rpart(registered~temp,data=train)
fancyRpartPlot(f)

f=rpart(casual~temp,data=train)
fancyRpartPlot(f)

data$temp_reg=0
data$temp_reg[data$temp<13]=1
data$temp_reg[data$temp>=13 & data$temp<23]=2
data$temp_reg[data$temp>=23 & data$temp<30]=3
data$temp_reg[data$temp>=30]=4

data$temp_cas=0
data$temp_cas[data$temp<15]=1
data$temp_cas[data$temp>=15 & data$temp<23]=2
data$temp_cas[data$temp>=23 & data$temp<30]=3
data$temp_cas[data$temp>=30]=4



data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)

# creating another variable day_type which may affect 
# our accuracy as weekends and weekdays are important in deciding rentals

data$day_type=0

data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

plot(train$temp,train$count)
data=rbind(train,test)
data$month=substr(data$datetime,6,7)
data$month=as.integer(data$month)

# dividing total data depending on windspeed to impute/predict the missing values
table(data$windspeed==0)
k=data$windspeed==0
wind_0=subset(data,k)
wind_1=subset(data,!k)

# predicting missing values in windspeed using a random forest model
# this is a different approach to impute missing values rather than 
# just using the mean or median or some other statistic for imputation

set.seed(415)
fit <- randomForest(windspeed ~ season+weather +humidity +month+temp+ year+atemp, data=wind_1,importance=TRUE, ntree=250)
fit
pred=predict(fit,wind_0)
wind_0$windspeed=pred


data=rbind(wind_0,wind_1)

data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday"]=1

str(data)


# converting all relevant categorical variables into factors 
# to feed to our random forest model

data$season=as.factor(data$season)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)
data$weather=as.factor(data$weather)
data$hour=as.factor(data$hour)
data$month=as.factor(data$month)
data$day_part=as.factor(data$dp_cas)
data$day_type=as.factor(data$dp_reg)
data$day=as.factor(data$day)
data$temp_cas=as.factor(data$temp_cas)
data$temp_reg=as.factor(data$temp_reg)

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

# log transformation for some skewed variables, which can be seen from their distribution
train$reg1=train$registered+1
train$cas1=train$casual+1
train$logcas=log(train$cas1)
train$logreg=log(train$reg1)
test$logreg=0
test$logcas=0

boxplot(train$logreg~train$weather,xlab="weather", ylab="registered users")
boxplot(train$logreg~train$season,xlab="season", ylab="registered users")


# final model building using random forest
# note that we build different models for predicting for registered and casual users
# this was seen as giving best result after a lot of experimentation

set.seed(415)
fit1 <- randomForest(logreg ~ hour +workingday+day+holiday+ day_type +temp_reg+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part, data=train,importance=TRUE, ntree=250)

pred1=predict(fit1,test)
test$logreg=pred1

set.seed(415)
fit2 <- randomForest(logcas ~hour + day_type+day+humidity+atemp+temp_cas+windspeed+season+weather+holiday+workingday+dp_cas+weekend+year+year_part, data=train,mtry=4,importance=TRUE, ntree=250)

pred2=predict(fit2,test)
test$logcas=pred2


print(fit2)




#creating the final submission file
test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1

registered.pred = data.frame(datetime=test$day, test$hour,tot_reg=test$registered) 
View(registered.pred)

casual.pred = data.frame(datetime=test$hour,tot_cas=test$casual) 
View(casual.pred)

test$count=test$casual+test$registered
s<-data.frame(datetime=test$datetime,count=test$count)
write.csv(s,file="submit.csv",row.names=FALSE)

Submit = read.csv("submit.csv")

View(Submit)
summary(Submit)

?randomForest

