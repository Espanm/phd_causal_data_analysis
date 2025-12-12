# CHAPTER 20
# CH20A Working from home and employee performance

########## Preparation

# Installing and defining libraries
#install.packages("tidyverse")
#install.packages("fixest")
library(tidyverse)
library(fixest)

# Loading data
data <- read_csv('https://osf.io/5c3rf/download')

# Checking data
View(data)

# Number of treated and control units
table(data$treatment)
table(data$ordertaker)
table(data$treatment, data$ordertaker)

# It is a prepared, cleaned dataset, one thing is needed: modifying the ageyoungestchild variable
table(data$ageyoungestchild)
data$ageyoungestchild <- ifelse(data$children == 0, NA, data$ageyoungestchild)

########## Checking covariate balance 
# Selecting necessary variables for the table
data_temp <- data %>% 
  select(ordertaker, age:perform10, prior_experience:internet)

# Generating empty table-elements
vars <- colnames(data_temp)
rm(data_temp)
mean_t <- c()
mean_c <- c()
sd <- c()
p_value <- c()
model <- c()

# Generating statistics to the table
for(i in vars){
  model <- lm(paste(i, "~treatment"), data=data) 
  mean_c[i] <- mean(subset(data, treatment == 0)[[paste(i)]], na.rm=T)
  mean_t[i] <- mean(subset(data, treatment == 1)[[paste(i)]], na.rm=T)
  p_value[i] <- anova(model)$'Pr(>F)'[1]
  sd[i] <- sd(data[[paste(i)]], na.rm=T)
}

# Filling table
table <- data.frame(round(mean_t, 2), round(mean_c, 2), round(sd, 2), round(p_value, 2))
col.names <- c("Treatment mean", "Control mean", "Std.dev.", "p-value of test of equal means")  
names(table) <- col.names
print(table)

########## Regression
# Regression 1: ATE estimates, no covariates
reg1 <- feols(quitjob ~ treatment, data=data , vcov = "HC1")
reg2 <- feols(phonecalls1 ~ treatment, data=data[data$ordertaker==1, ], vcov = "HC1")
etable(reg1,reg2,fitstat = c('n','r2'))

# Regression 2: ATE estimates, with covariates
reg3 <- feols(quitjob ~ treatment + married + children, data=data, vcov = "HC1")
reg4 <- feols(phonecalls1 ~ treatment + married + children, data=data[data$ordertaker==1, ], vcov = "HC1")
etable(reg3,reg4,fitstat = c('n','r2'))

# Regression 3: ATE estimates, with covariates, only for non-quitters
smallsample <- filter(data, ordertaker==1 & quitjob==0)
reg5 <- feols(phonecalls1 ~ treatment, data=smallsample, vcov = "HC1")
reg6 <- feols(phonecalls1 ~ treatment + married + children, data=smallsample, vcov = "HC1")
etable(reg5,reg6,fitstat = c('n','r2'))

# Phonecalls regression together
etable(reg2,reg4,reg6,fitstat = c('n','r2'))
