# CHAPTER 24
# CH24A: Estimating the effect of the 2010 Haiti earthquake on GDP

########## Preparation

# Clearing memory
rm(list=ls())

# Importing libraries
library(tidyverse)
library(Synth)

# Loading data
data <- read_csv('https://osf.io/sdj62/download')

# Defining the donor pool
dp_countries <- c("HTI","BGD","BEN","BFA","BDI","KHM", 
                  "CMR","KEN","KGZ","LBR","MDG","MLI","MDA","MOZ", 
                  "NPL", "NIC","RWA","SEN","SLE","SDN","TZA","TGO","UGA")

# Keeping the donor pool countries and making some data manipulation
data <- data %>%
  mutate(dp = code %in% dp_countries) %>% 
  filter(dp==TRUE) %>%
  mutate(code = factor(code, levels = dp_countries, ordered = TRUE)) %>% 
  mutate(ccode = as.numeric(code))

# Predictors
View(select(data, ccode, year, gdptb_us, cons, exp, imp, gcf, land, pop, inf, gdppc_w))

# Generating the synthetic control
# dataprep: this function prepares the data for the optimization process
dataprep_out <-
  dataprep(
    foo = as.data.frame(data),
    predictors = c("cons", "exp", "imp", "gcf", "land", "pop", "inf", "gdppc_w"),
    predictors.op = c("mean"),
    dependent = "gdptb_us",
    unit.variable = "ccode",
    time.variable = "year",
    special.predictors = list(
      list("gdptb_us", 2005, "mean"),
      list("gdptb_us", 2007, "mean"),
      list("gdptb_us", 2009, "mean")
    ),
    treatment.identifier = 1,
    controls.identifier = c(2:23),
    time.predictors.prior = c(2004:2009),
    time.optimize.ssr = c(2004:2009), 
    unit.names.variable = "country",
    time.plot = c(2004:2015)
  )

# synth: it calculates the optimal weights
synth_out <- synth(dataprep_out)

# Presenting main results
synth_tables <- synth.tab(
  dataprep.res = dataprep_out,
  synth.res = synth_out)
print(synth_tables)

# Plotting results
path.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 2010,
          Ylab = "GDP",
          Xlab = "Year",
          Legend = c("Haiti", "Synthetic Haiti"),
          Legend.position=c("topleft"),
          Main = "Haiti vs Synthetic Haiti")

gaps.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 2010,
          Ylab = "GDP",
          Xlab = "Year",
          Main = "Haiti vs Synthetic Haiti")
