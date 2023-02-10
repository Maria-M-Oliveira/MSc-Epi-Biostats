# Libraries
library(tidyverse)
library(ggplot2)
library(foreign)
library(DescTools)

#Files
BD <- read.spss("BaseCovid_trabalho4.sav", to.data.frame = TRUE)

# Quiquadrado
cross_age_cases <- xtabs(~BD$total_cases_per_million_10_tercil+BD$median_age_2cat)

# To check that the assumptions are met, let's make an expected frequencies table
expected <- ExpFreq(cross_age_cases)
# Every value is <5, so assumptions are met
qui <- chisq.test(BD$total_cases_per_million_10_tercil, BD$median_age_2cat)
qui #p-value 3.299e-07, we reject the null hypothesis that these variables are independent.
# As such, there is a statistically significant association between the variables
