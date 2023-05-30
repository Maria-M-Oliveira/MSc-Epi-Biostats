### Libraries ####
library(tidyverse)
library(foreign)

### Data ####
df <- read.spss("./MSc-Epi-Biostats/Breast_cancer_suvival.sav", to.data.frame = TRUE)
