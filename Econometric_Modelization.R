# Code Health Economics Project

# Cardiovascular Disease dataset : https://www.kaggle.com/sulianova/cardiovascular-disease-dat

#### librairies import ####
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(psych)
library(dplyr)
library(gridExtra)
library(gtsummary)
library(forestmodel)
library(car)


##########################################################

### 1/ Preparation of data              

##########################################################

# Data directory path
setwd("")

# Reading the data
data <- read.csv("cardio.csv", sep = ";")

# First impressions 
dim(data)
head(data,n=5)
describeBy(data)

##########################################################

### 2/ Data cleaning   

##########################################################

# 1) Some columns are renamed
data = rename(data, systolic=ap_hi, diastolic=ap_lo, glucose=gluc,alcohol= alco)

# 2) Deleting the id column
data <- subset(data, select=-c(id))

# 3) Removal of duplicates per line
data <- data[!duplicated(data),]

# 4) We check if there are missing values in our data.
sum(is.na(data))

# 5) Convert the age column to years
data[,1] <- data[,1]/365

# 6) Recoding the gender
data$gender[data$gender==2] <- "Men"
data$gender[data$gender==1] <- "Women"

# 7) Recoding active
data$active[data$active==0] <- "No"
data$active[data$active == 1] <- "Yes"

# 8) Recoding cardio
data$cardio[data$cardio==0]<- "No"
data$cardio[data$cardio==1]<-"Yes"


# Création d'une nouvelle variable IMC ("BMI")
data$BMI<- data$weight/(data$height/100)^2


# We will classify the BMI of individuals by level
data$BMIcat[data$BMI<18.5] <- 1
data$BMIcat[data$BMI>=18.5 & data$BMI<25]  <- 2
data$BMIcat[data$BMI>=25 & data$BMI<30] <- 3
data$BMIcat[data$BMI>=30] <- 4


# Removing extreme values
data <- subset(data, weight  > 29)
data <- subset(data, height < 250  & height > 120)
data <- subset(data, systolic < 250 & systolic > 50)
data <- subset(data, diastolic < 200  & diastolic > 50)
data <- subset(data,BMI <72)


# Convert categorical variables into factors
cols = c("gender", "cholesterol", "glucose", "smoke", "alcohol", "active", "cardio", "BMIcat")
data[cols] = lapply(data[cols], factor)

##########################################################

### 2/ Data viz            

##########################################################

### Bar plot to get a first glimpse of the data

# Bar chart gender
plot1 <- ggplot(data, aes(x = gender,fill=gender)) +
  geom_bar() +
  labs(x = "Gender", 
       y = "Count", 
       title = "Bar plot Gender")

# Presence of cardio disease
plot2 <- ggplot(data, aes(x = cardio ,fill=cardio)) +
  geom_bar() +
  labs(x = "Cardio", 
       y = "Count", 
       title = "Bar plot Cardiovascular disease")

# Bar plot Cholesterol
plot3 <- ggplot(data, aes(x = cholesterol)) + 
  geom_bar() +
  labs(x = "Cholesterol", 
       y = "Count", 
       title = "Bar plot Cholesterol")

# Bar plot Glucose
plot4 <- ggplot(data, aes(x = glucose)) + 
  geom_bar() +
  labs(x = "Glucose", 
       y = "Count", 
       title = "Bar plot Glucose")

grid.arrange(plot1, plot2, plot3, plot4, nrow= 2, ncol=2)

# Bar plot to visualize the possible links between the categorical variables and the cardio variable
plot1 <- ggplot(data, aes(x = gender, fill=cardio)) +
  geom_bar(position="dodge") +
  xlab("Gender")+
  ylab("Count")

plot2 <- ggplot(data, aes(x = active, fill=cardio)) +
  geom_bar(position="dodge") +
  xlab("Active")+
  ylab("Count") 

plot3 <- ggplot(data, aes(x = glucose,  fill=cardio)) +
  geom_bar(position="dodge") +
  xlab("Glucose")+
  ylab("Count") 

plot4 <- ggplot(data, aes(x = smoke,fill=cardio)) +
  geom_bar(position="dodge") +
  xlab("Smoke")+
  ylab("Count") 

plot5 <- ggplot(data, aes(x = cholesterol,fill=cardio)) +
  geom_bar(position="dodge") +
  xlab("Cholesterol")+
  ylab("Count") 

plot6 <- ggplot(data, aes(x = alcohol,fill=cardio)) +
  geom_bar(position="dodge") +
  xlab("Alcohol")+
  ylab("Count") 

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow= 3, ncol=2)

# Box plot to illustrate the qualitative variables
plot1 <- ggplot(data = data,aes(x=cardio, y=systolic, col=cardio))+
  geom_boxplot()+
  xlab("Presence of the disease")+
  ylab("Systolic blood pressure")

plot2 <- ggplot(data = data,aes(x=cardio,y=diastolic, col=cardio))+
  geom_boxplot()+
  xlab("Presence of the disease")+
  ylab("Diastolic blood pressure")

plot3 <- ggplot(data = data,aes(x=cardio,y=age, col=cardio))+
  geom_boxplot()+
  xlab("Presence of the disease")+
  ylab("Age") 

plot4 <- ggplot(data = data,aes(x=cardio,y=BMI, col=cardio))+
  geom_boxplot()+
  xlab("Presence of the disease")+
  ylab("BMI")

grid.arrange(plot1, plot2, plot3, plot4, nrow= 2, ncol=2)

# Continuous variables
plot1 <- ggplot(data, aes(x=age)) + 
  geom_density(adjust = 1.5)

plot2 <- # Histogram height + density
  
  ggplot(data, aes(x=height)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="yellow", fill="white") +
  geom_density(alpha=.2, fill="red")

plot3 <- # Histogram weight + density
  
  ggplot(data, aes(x=weight)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="red", fill="white") +
  geom_density(alpha=.2, fill="blue")

grid.arrange(plot1, plot2, plot3, nrow= 1, ncol=3)

# Calculate correlations (quantitative variables)
r <- cor(data[,c(1,3:6,13)], use="complete.obs")
round(r,2)

ggcorrplot(r, hc.order = TRUE, type = "lower", 
           lab = TRUE)

# Corrélogramme 
corrplot(r, method="color")

##########################################################

### 3/ Hypothèses             

##########################################################

## Hypothesis 1: Are the variables cholesterol and BMI independent?

# oneway.test: Test if two or more samples from normal distributions have the same means. 
# Variances are not necessarily assumed to be equal.
# var.equal = True: A simple F-test for the equality of the means in a unidirectional analysis of variance is then performed.
# the boxplot is used to illustrate the results of this test

bartlett.test(data$BMI~data$cholesterol)
oneway.test(data$BMI~data$cholesterol, var.equal = TRUE)
boxplot(data$BMI~data$cholesterol)

# Hypothesis 2: Are diastolic and systolic variables correlated?
corr.test(data$systolic, data$diastolic)

# Hypothesis 3: Does gender affect smoking habits?

# chisq.test: performs chi-square contingency table and fit tests.
# correct=False: indicates that a continuity correction should not be applied when calculating the statistic 
# of the test for 2 by 2 tables 

chisq.test(data$gender, data$smoke, correct=FALSE)

# Hypothesis 4: Are the variables cholesterol and glucose independent? 
chisq.test(data$cholesterol, data$glucose, correct=FALSE)


##########################################################

### 4/ Logistic Regression             

##########################################################

# Logistic regression is used to isolate the effects of each variable. 
# Our variable of interest is cardio. 

# Model without interaction effect
model <- glm(cardio ~ age + gender + BMI + systolic + diastolic + cholesterol + glucose + smoke + 
               alcohol + active, data = data, family = binomial(logit))
summary(model)

# Presentation of the results of the logistic regression (Odds ratio)
forest_model(model)

## Reading the 3 columns of the table, 1st line:
# odds ratio = 1.05, IC à 95% [1.05, 1.05], P-value < 0.001

### How to interpret the odds ratio? 
#odds ration = 1 -> no effect
#odds ratio >> 1 -> increase of the studied phenomenon
#odds ratio << 1 -> decrease of the studied phenomenon

### Evaluate the quality of a model
#The lower the AIC, the better the model

# Graphical representation of the effects
cowplot::plot_grid(plotlist = plot(ggeffect(model)))


# Modèle avec effets d'interactions
full_model <- glm(cardio~age+ gender + BMI + systolic + diastolic + cholesterol + glucose + 
                    smoke + alcohol + active + (systolic*diastolic) + (gender*smoke) + 
                    (cholesterol * BMI)+ (cholesterol*glucose), data = data, family = binomial(logit)) 
summary(full_model)

# Choice of the best quality model (AIC criteria)
step(full_model, direction="backward")

# The step procedure stops as soon as the AIC found is higher than that of the previous model
# Here, our model with interaction effects had an AIC of 76,851 at the start
# The step procedure found a model with an AIC of 76,849
# this AIC is lower than the starting AIC. So we will retain this model

full_model_2 <- glm(formula = cardio ~ age + BMI + systolic + diastolic + cholesterol + 
                      glucose + smoke + alcohol + active + systolic:diastolic + 
                      BMI:cholesterol + cholesterol:glucose, family = binomial(logit), 
                    data = data)
summary(full_model_2)

## The AIC of the non-interaction model was 76,960. The AIC of the model with interaction effects was 76,849.

## Presentation of the results of the logistic regression (Odds ratio)
forest_model(full_model_2)


# Confusion matrix (test the quality of the model)

# Model without interaction effect
cardio.pred <- predict(model, type = "response", newdata = data)
head(cardio.pred)
table(cardio.pred > 0.5, data$cardio)


# Model with interaction effect
cardio.pred2 <- predict(full_model_2, type = "response", newdata = data)
head(cardio.pred2)
table(cardio.pred2 > 0.5, data$cardio)
