### Libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(freqtables)
library(naniar)
library(stats)
library(moments)

### Files
asma <- read_xlsx("Asma_GDH.xlsx")

##Objective
# 1- Data validation
# 2- Variable transformation from quantitative continuous to binary
# 3- Univariate analysis
# 4- Bivariate analysis (qualixquali, quantixquali, quantixquanti)


### Data validation
# To validate data, i must look for missing and impossible values, as well as outliers
asma_miss_analysis <- miss_var_summary(asma) #Returns a tibble of missing values count and percent by variable
#quantitativa variables: idade, dias internamento,num procedimentos
age_box_plot <- boxplot(asma$`Idade (anos)`) #No outliers or impossible values
intern_days_box_plot <- boxplot(asma$`Dias de internamento`) #Several outliers and some extreme points that could be indicative of impossible values
procedure_box_plot <- boxplot(asma$`Numero de Procedimentos`) #Several outliers and some extreme points that could be indicative of impossible values

asma %>%
  summarise(n = n(), 
            mean = mean(`Idade (anos)`, na.rm = TRUE), 
            sd = sd(`Idade (anos)`, na.rm = TRUE),
            stderr = sd/sqrt(n), 
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median=median(`Idade (anos)`, na.rm = TRUE),
            min=min(`Idade (anos)`, na.rm = TRUE), 
            max=max(`Idade (anos)`, na.rm = TRUE),
            IQR=IQR(`Idade (anos)`, na.rm = TRUE),
            kurt= kurtosis(`Idade (anos)`),
            skew = skewness(`Idade (anos)`))

### Variable transformation from quantitative continuous to binary
# I need to transform a continuous variable (age) to a binary one (0-Below Median; 1-Above Median)
median_age <- median(asma$`Idade (anos)`)

asma <- asma %>% 
  mutate(age_bin = ifelse(`Idade (anos)`>median_age,1,0))

### Univariate analysis of chosen variables
freq_NUTS <- freq_table(asma, `NUTS III`)

bar_NUTS <- ggplot() + 
  geom_bar(asma, mapping=aes(`NUTS III`, fill=`NUTS III`))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
bar_NUTS

freq_Dx <- freq_table(asma,`Diagnůstico (GCD)`)
bar_dx <- ggplot() +
  geom_bar(asma, mapping=aes(`Diagnůstico (GCD)`))
bar_dx

asma %>%
  summarise(n = n(), 
            mean = mean(`Numero de Procedimentos`, na.rm = TRUE), 
            sd = sd(`Numero de Procedimentos`, na.rm = TRUE),
            stderr = sd/sqrt(n), 
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median=median(`Numero de Procedimentos`, na.rm = TRUE),
            min=min(`Numero de Procedimentos`, na.rm = TRUE), 
            max=max(`Numero de Procedimentos`, na.rm = TRUE),
            IQR=IQR(`Numero de Procedimentos`, na.rm = TRUE),
            kurt= kurtosis(`Numero de Procedimentos`),
            skew = skewness(`Numero de Procedimentos`))

hist_procedure <- hist(asma$`Numero de Procedimentos`, breaks=50, right=F,
                       main="Histogram of number of procedures",
                       xlab="Number of procedures",
                       col="#d2d67a")

asma %>%
  summarise(n = n(), 
            mean = mean(`Idade (anos)`, na.rm = TRUE), 
            sd = sd(`Idade (anos)`, na.rm = TRUE),
            stderr = sd/sqrt(n), 
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median=median(`Idade (anos)`, na.rm = TRUE),
            min=min(`Idade (anos)`, na.rm = TRUE), 
            max=max(`Idade (anos)`, na.rm = TRUE),
            IQR=IQR(`Idade (anos)`, na.rm = TRUE),
            kurt= kurtosis(`Idade (anos)`),
            skew = skewness(`Idade (anos)`))
hist_age <- hist(asma$`Idade (anos)`, breaks=90, right=F,
                 main="Histogram of participants age",
                 xlab="Age (years)",
                 col="#7dd67a")


### Bivariate analysis
#Quali x Quali --> Districts X Episode Type
cross_dist_epi <- xtabs(~asma$Distrito + asma$`Tipo Episodio`)
cross_dist_epi

#Quanti x Quali
# Type of asthma dx and age
summary_asth_age <- asma %>% 
  group_by(`Tipo diagnostico Asma`) %>% 
  summarize(mean = mean(`Idade (anos)`),
            median = median(`Idade (anos)`),
            min = min(`Idade (anos)`),
            max = max(`Idade (anos)`),
            stdev = sd(`Idade (anos)`),
            var = stdev^2,
            kurt= kurtosis(`Idade (anos)`),
            skew = skewness(`Idade (anos)`),
            interquart = IQR(`Idade (anos)`))
  
summary_asth_age
#Quanti x Quanti
# No of procedures and intensive care days
plot(asma$`Numero de Procedimentos`, asma$`Dias de internamento`,
     main="Graphical representation of the correlation \nbetween hospitalization days and number of procedures",
     xlab="Number of procedures",
     ylab="Hospitalization days",
     col="#141d82")
correlation <- cor.test(asma$`Numero de Procedimentos`, asma$`Dias de internamento`)
correlation

