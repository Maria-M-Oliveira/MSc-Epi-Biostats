### Libraries ####
library(tidyverse)
library(foreign)
library(survival)
library(ranger)
library(ggfortify)

### Data ####
df <- read.spss("./Breast_cancer_survival.sav", to.data.frame = TRUE) %>% 
  mutate(status = recode(status, `Censored` = 0, `Died` = 1))
# Variable status: "Died" or "Censored"
# Variable time: survival time in months
# Need to change status variable from string to number where 1=Died and 0=Censored otherwise this does not work as intended


km <- with(df, Surv(time, status))
head(km,80)
#  + after the time in the print out of km indicates censoring

# Kaplan-Meier analysis
km_fit <- survfit(Surv(time, status) ~ 1, data=df)
summary(km_fit, times = c(1,6,12*(1:10))) 
# times = c(1,6,12*(1:10)) for printing estimates for 1, 6 and 12 months and then every 12 months thereafter
autoplot(km_fit)
#The former only informs us of the generic survival time

# The next two lines give us the survival curves taking into account the ln_yesno variable
# This can be done for several other variables
km_trt_fit <- survfit(Surv(time, status) ~ ln_yesno, data=df)
autoplot(km_trt_fit)

