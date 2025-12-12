# CHAPTER 21
# CH21A Founder/family ownership and quality of management
# Multiple regression and matching

########## Preparation
# Clearing memory
rm(list=ls())

# Defining libraries
library(tidyverse)
library(fixest)
library(MatchIt)

################################################################################ Regression
setwd("C:/Users/tokes/OneDrive - Corvinus University of Budapest/TANITAS/kauzalis_adat/R_anyagok/chapter_21/")
data <- read_csv("wms_work.csv")

data$industry <- as.factor(data$industry)

# OLS without controls
ols1 <- feols(management ~ foundfam_owned, data = data)

# OLS with controls
ols2 <- feols(management ~ foundfam_owned + degree_nm + degree_nm_sq + compet_moder + compet_strong +
                lnemp + age_young + age_old + age_unknown + industry + countrycode, data=data)
summary(ols2)

# Printing results
etable( ols1,ols2,
        fitstat = c('n','r2'),
        keep = c('Constant', 'foundfam_owned'),
        headers = c("no confounders", "with confounders"))

################################################################################ Exact matching - manual

########## Creating variables for exact matching
# employment: We create 5 bins based on quintiles (roughly 20% of the obs in each bin)
# age: 4 categories (the original ones)
# degree_nm: 4 bins (0s and 3 bins based on the percentiles)
summary(data$degree_nm)
summary(data$degree_nm[data$degree_nm>0])
# compet: 3 categories (the original ones)
table(data$competition)

data <- data %>%
  mutate(
    empbin5 = cut(emp_firm, quantile(emp_firm, seq(0,1,0.2)), include.lowest = TRUE, right = FALSE),
    agecat4 = (age_young == TRUE) + 2*(age_mid == TRUE) + 3*(age_old == TRUE) + 4*(age_unknown == TRUE),
    degree_nm_bin4 = cut(degree_nm, c(0,0.001,0.05,0.20,1.01), right= FALSE),
    compet3 = recode(competition, "0 competitors" = "weak", "1-4 competitors" = "weak", "5-9 competitors" = "moderate", "10+ competitors" = "strong")
  )

table(data$empbin5)
table(data$agecat4)
table(data$degree_nm_bin4)
table(data$compet3)
table(data$industry)
table(data$countrycode)

########## Number of theoretical combinations
# See: ppt
5*4*4*3*20*24

########## The exact matching
data_agg <- data %>%
  group_by(empbin5, degree_nm_bin4, agecat4, compet3, industry, countrycode) %>%
  summarise(
    n = n(), n0 = sum(1-foundfam_owned), n1 = sum(foundfam_owned),
    y0 = sum(management*(foundfam_owned == 0))/sum(1-foundfam_owned), 
    y1 = sum(management*(foundfam_owned == 1))/sum(foundfam_owned)
  ) %>%
  ungroup()

View(data_agg)
# actual number of combinations: 6976

# Number of combinations with control and without treated: (3622)
nrow(data_agg[data_agg$n0 > 0 & data_agg$n1 == 0, ])
# Number of combinations with treated without control: (2886)
nrow(data_agg[data_agg$n0 == 0 & data_agg$n1 > 0, ])
# Number of combinations with control and treated: (468)
nrow(data_agg[data_agg$n0 > 0 & data_agg$n1 > 0, ])
# Number of firms which will be included: (1207)
sum(data_agg$n[data_agg$n0 > 0 & data_agg$n1 > 0])

# ATE and ATET
data_agg %>%
  filter(n0>0 & n1>0) %>%
  summarise(ATE = weighted.mean(y1-y0, n), ATET = weighted.mean(y1-y0, n1))

################################################################################ Exact matching - MatchIt
data_exact <- data %>% 
  select(management, foundfam_owned, degree_nm_bin4, agecat4,
         compet3, empbin5, 
         industry, countrycode) %>%
  na.omit()

formula_exact <- as.formula(foundfam_owned ~ empbin5 + degree_nm_bin4 + agecat4 +
                              compet3 + industry + countrycode)

mod_match <- matchit(formula_exact, 
                     data = data_exact, 
                     method = 'exact', estimand = 'ATE') 
# With estimand = 'ATT' we can calculate the ATT, too.

data_match <- match.data(mod_match)

reg_match <- feols(management ~ foundfam_owned, 
                   data = data_match, 
                   weights = data_match$weights
)

reg_match

################################################################################ Pscore matching
### Selecting data
data_pscore <- data %>% 
  select(management, foundfam_owned, degree_nm, degree_nm_sq,
         compet_moder, compet_strong, 
         lnemp, age_young, age_old, age_unknown,
         industry, countrycode) %>%
  na.omit()

### Defining formula
formula_pscore <- as.formula(foundfam_owned ~ degree_nm + degree_nm_sq +
                               compet_moder + compet_strong + 
                               lnemp + age_young + age_old + age_unknown +
                               industry + countrycode)
### Doing matching
mod_match <- matchit(formula_pscore, 
                     data = data_pscore, 
                     method = 'nearest', distance = 'logit', replace=TRUE)

mod_match

### Restricting data to matched 
data_match <- match.data(mod_match)

# Estimating treatment effects
reg_match <- feols(management ~ foundfam_owned, 
                   data = data_match, 
                   weights = data_match$weights
)

reg_match