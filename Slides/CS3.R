# CHAPTER 22
# CH22A How does a merger between airlines affect prices?

########## Preparation

# Clearing memory
rm(list=ls())

# Installing and defining libraries
library(tidyverse)
library(fixest)


# Loading data
data_agg <- read_rds('CS3.rds')

# Definition of the market
# New York JFK, Los Angeles LAX
data_agg %>%
  select(market, origin, finaldest, return, year) %>%
  filter(origin=="JFK" & finaldest=="LAX")

# Tagging cross-sectional units with any missing prices
data_agg <- data_agg %>%
  group_by(market) %>%
  mutate(price_missing_tag = ifelse(any(is.na(lnavgp)), 1, 0)) %>%
  ungroup()

# Keep balanced data
data_balanced <- data_agg %>%
  filter(balanced == 1,
         price_missing_tag == 0)  

table(data_balanced$year)

# Checking the number of treated & untreated in 2016
# treated = both AA and USA were presented in 2011
# untreated = neither AA nor USA were presented in 2011
nrow(filter(data_balanced, year == 2016, treated == 0, untreated == 0))
nrow(filter(data_balanced, year == 2016, treated == 1, untreated == 0))
nrow(filter(data_balanced, year == 2016, treated == 0, untreated == 1))
nrow(filter(data_balanced, year == 2016, treated == 1, untreated == 1))

data_final <- data_balanced %>%
  filter((treated==0 & untreated==1) | (treated==1 & untreated==0))

# Generating small market and large market samples
# small market = less than 5000 passengers
small <- data_final %>%
  filter(smallmkt==1)  

large <- data_final %>%
  filter(smallmkt==0)  

########## DiD regressions
# Running
reg_total <- feols(lnavgp ~ treated*after, weights = data_final$pass_bef, data = data_final, vcov="HC1")
reg_small <- feols(lnavgp ~ treated*after, weights = small$pass_bef, data = small, vcov="HC1")
reg_large <- feols(lnavgp ~ treated*after, weights = large$pass_bef, data = large, vcov="HC1")

# Presenting
etable(reg_total , reg_small, reg_large, 
       headers = c("All markets", "Small markets", "Large markets"), digits = 3 )

# Related DiD averages
data_final %>%
  group_by(after, treated) %>%
  summarise(sprintf("%.3f", weighted.mean(lnavgp, pass_bef, na.rm = TRUE)), n())

