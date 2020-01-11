setwd("C:/Users/sweto/Dropbox/Documents/People/Sweto/MSTM/Spring_Course/BADM-590-Predictive Data Analytics/After-MidTerm/Final Project")
getwd()

# importing csv file
LED=read.csv("Life Expectancy Data.csv",header=T)
# Viewing the entire data set
View(LED)
# using attach command making all the variables global
attach(LED)

#Checking the structure of the data
str(LED) #data.frame':	2938 obs. of  22 variables

# Checking the dimension of data
dim(LED) # rows=2938, cols=22

# Checking for total number of missing values in the entire data frame
sum(is.na(LED)) # o/p = 2563

# ---------------------------------------------------------------------------------
# Replacing the NA's in the entire data frame with 0's only for corelation matrix
LED[is.na(LED)]=0
dim(LED)
# ----------------------------------------------------------------

# creating the corelation matrix 
cor_matrix=cor(LED[,-c(1,3)])
cor_matrix

#plot all the correlation coefficient of variables
library(corrplot)
corrplot(cor_matrix, type = "upper", tl.col = "black", tl.srt = 45)

# assigning the corelation matrix to an excel file as the size of it's very big
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_201')
library(rJava)
library(xlsx)
R.Version()
.Machine$sizeof.pointer
write.xlsx(cor_matrix, "C:/Users/sweto/Dropbox/Documents/People/Sweto/MSTM/Spring_Course/BADM-590-Predictive Data Analytics/After-MidTerm/Final Project/cor_matrix_new.xlsx")

# Checked in excel that which explanatory variables are strongly corelated among each other

# creating multiple linear regression model
LED=read.csv("Life Expectancy Data.csv",header=T)
g=lm(Life.expectancy~.-Country,data=LED)
summary(g)

# To keep only rows with no NA 
LED=na.omit(LED)
dim(LED)
attach(LED)

# now we are going to create predictions for all data
g_predict=lm(Life.expectancy~.-Country-Status,data=LED)
predicted=predict(g_predict,LED[,-c(1,3)])


# Finding the residuals
residuals=LED$Life.expectancy-predicted

# Finding prediction interval
qt(0.025,df=2917) # value is -1.960778

# Checking the multiple regression model
# unobserved error should be - 1)independent of one another, 2)have equal variance & 3)normally distributed around the regression line
plot(predicted,g_predict$residuals)
# As it's scattered, so this satisfy 1st criteria

# checking scatter plot of residuals vs. each explanatory variables
plot(LED$Country,g_predict$residuals)
plot(LED$Year,g_predict$residuals)
plot(LED$Status,g_predict$residuals)
plot(LED$Life.expectancy,g_predict$residuals)
plot(LED$Adult.Mortality,g_predict$residuals)
plot(LED$infant.deaths,g_predict$residuals)
plot(LED$Alcohol,g_predict$residuals)
plot(LED$percentage.expenditure,g_predict$residuals)
plot(LED$Hepatitis.B,g_predict$residuals)
plot(LED$Measles,g_predict$residuals)
plot(LED$BMI,g_predict$residuals)
plot(LED$under.five.deaths,g_predict$residuals)
plot(LED$Polio,g_predict$residuals)
plot(LED$Total.expenditure,g_predict$residuals)
plot(LED$Diphtheria,g_predict$residuals)
plot(LED$HIV.AIDS,g_predict$residuals)
plot(LED$GDP,g_predict$residuals)
plot(LED$Population,g_predict$residuals)
plot(LED$thns.1.19.yrs,g_predict$residuals)
plot(LED$thns.5.9.yrs,g_predict$residuals)
plot(LED$incm.cmp.rsrs,g_predict$residuals)
plot(LED$Schooling,g_predict$residuals)
# As all the plots show no pattern, so it satisfy 2nd criteria

# Checking the normal condition
qqnorm(g_predict$residuals)
qqline(g_predict$residuals)
# satisfy 3rd condition

# Inference we already got from F-test & p-value from summary, that its significant

# Check Collinearity
# so for that we create simple regression model for those specific variables which are corelated among each other
g_year=lm(Life.expectancy~Year,data=LED)
summary(g_year)

g_status=lm(Life.expectancy~Status,data=LED)
summary(g_status)

g_Adult.Mortality=lm(Life.expectancy~Adult.Mortality,data=LED)
summary(g_Adult.Mortality)

g_infant.deaths=lm(Life.expectancy~infant.deaths,data=LED)
summary(g_infant.deaths)

g_Alcohol=lm(Life.expectancy~Alcohol,data=LED)
summary(g_Alcohol)

g_percentage.expenditure=lm(Life.expectancy~percentage.expenditure,data=LED)
summary(g_percentage.expenditure)

g_Hepatitis.B=lm(Life.expectancy~Hepatitis.B,data=LED)
summary(g_Hepatitis.B)

g_Measles=lm(Life.expectancy~Measles,data=LED)
summary(g_Measles)

g_BMI=lm(Life.expectancy~BMI,data=LED)
summary(g_BMI)

g_under.five.deaths=lm(Life.expectancy~under.five.deaths,data=LED)
summary(g_under.five.deaths)

g_Polio =lm(Life.expectancy~Polio,data=LED)
summary(g_Polio)

g_Total.expenditure=lm(Life.expectancy~Total.expenditure,data=LED)
summary(g_Total.expenditure)

g_Diphtheria=lm(Life.expectancy~Diphtheria,data=LED)
summary(g_Diphtheria)

g_HIV.AIDS=lm(Life.expectancy~HIV.AIDS,data=LED)
summary(g_HIV.AIDS)

g_GDP =lm(Life.expectancy~GDP,data=LED)
summary(g_GDP)

g_Population =lm(Life.expectancy~Population ,data=LED)
summary(g_Population )

g_thns.1.19.yrs=lm(Life.expectancy~thns.1.19.yrs,data=LED)
summary(g_thns.1.19.yrs)

g_thns.5.9.yrs=lm(Life.expectancy~thns.5.9.yrs,data=LED)
summary(g_thns.5.9.yrs)

g_incm.cmp.rsrs=lm(Life.expectancy~incm.cmp.rsrs,data=LED)
summary(g_incm.cmp.rsrs)

g_Schooling=lm(Life.expectancy~Schooling,data=LED)
summary(g_Schooling)

# measuring Variance Inflation Factor
library(car)
vif(g)

# Backward elimination
LED=read.csv("Life Expectancy Data.csv",header=T)
g=lm(Life.expectancy~.-Country,data=LED)
summary(g)

g1=update(g,.~.-thinness..1.19.years)
summary(g1)

g2=update(g1,.~.-Population)
summary(g2)

g3=update(g2,.~.-Hepatitis.B)
summary(g3)

g4=update(g3,.~.-Total.expenditure)
summary(g4)

g5=update(g4,.~.-Measles)
summary(g5)

g6=update(g5,.~.-percentage.expenditure)
summary(g6)

# now all the p-values are significant (<0.05)

# AIC
g=lm(Life.expectancy~.-Country,data=LED)
LED=na.omit(LED)
View(LED)
dim(LED)
g.aic=step(g)
summary(g.aic)

#=======================================================================================
# plotting through ggplot2
View(LED)
df=melt(LED,id.vars='Country',variable.name='series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(time,value)) + geom_line(aes(colour = series))

# choosing numerical variables from data set
num_data <- LED[, sapply(LED, is.numeric)] 
num_data
# choosing categorical variables from data set
cat_data <- LED[, sapply(LED, is.character)]
cat_data <- c(colnames(LED)[which(sapply(LED, is.character))])
cat_data
dim(LED)

# R-factor (Check this link - https://www.tutorialspoint.com/r/r_factors.htm)
# Factor can store both integers and strings. So to get the advantage of it we are converting character into factor


dim(num_data) # 2938   20
names(num_data)
colnames(num_data)
# Both the above command will give same result

# More reliable than hist() is the histogram function from the Hmisc package
# checking the distribution of every numerical column
install.packages("Hmisc")
library(Hmisc)
hist.data.frame(num_data[,1])
# But here using through Hmisc, we can't create histogram as it will be too large, error occurred
hist(num_data[1],main='hist', breaks=20, prob=TRUE)
hist(num_data[2],main='hist', breaks=20, prob=TRUE)
# or we can run a for loop for this . But here we can't see all the graphs
for (i in 1:length(names(num_data)))
  {
  print(i)
  hist( num_data[i], main='hist', breaks=20, prob=TRUE)
}

# removing all the rows which are NA or blank (Empty cell)
df[!(is.na(df$start_pc) | df$start_pc==""), ]