### Libraries
library(tidyverse)
library(foreign)
library(Hmisc)
library(nortest)
library(car)
library(lmtest)

### Files
BD <- read.spss("BD_TPC1.sav", to.data.frame = TRUE)

### Exercise 1
# Correlate the different variables and interpret its correlation matrix

#Changing factors from string to numeric
BD$sev_num <- as.numeric(BD$severidade_APR31)
BD$mort_num <- as.numeric(BD$mortalidade_APR31)

df <- BD %>% 
  select(-c(severidade_APR31, mortalidade_APR31))

# Creating correlation matrix
# testing normality before matrix is key
do.call(rbind, lapply(df, function(x) ks.test(x, pnorm)[c("statistic", "p.value")]))
# Doing a simple ks.test returns different statistics value for the ks test
# this is due to a correction that spss does, "Lilliefors Corrected"
# To get the same results:
# Package nortest
do.call(rbind, lapply(df, function(x) lillie.test(x)[c("statistic", "p.value")]))


# Returns the same table as the SPSS one, but separated into 3
rcorr(as.matrix(df)) #defaults to pearson correlation
rcorr(as.matrix(df), type="spearman") #in case we have a non normal distribution, such as our df here

#We then need to interpret not only the p-values but also the correlation to understand how the different variable vary and correlate between themselves


### Exercise 2
# Develop a multiple linear regression
# 1st we need to test assumptions

# 1 - Our dependent variable must be quantitative continuous
# Our dependent variable is hospitalization days, so check

# 2- There must be a linear relationship between the dependent variable and the independents
# We tested this in the previous exercise, where we saw the presence of linear relationship between the dependent and independent variables

# 3- No multicollinearity
# To assess for multicollinearity we first need to define the model we intend to use
model <- lm(dias_int ~ idade + Poderdecompra + Taxadeemprego + sev_num + mort_num, data=df)
vif(model)
# all the variable present VIF <3, which means there is no multicollinearity

# 4- Errors must be independent, normally distributed and Homoscedasticity (constant variance)
residuals <- model$residuals
# .1 testing for normality
ks.test(residuals, pnorm) #p-value <2.2e-16 
lillie.test(residuals) #p-value <2.2e-16 
# errors are not normally distributed

# .2 testing constant variance
# this is possible by assessing the "Residuals vs. Fitted" plot, complemented by the "Scale-Location" plot.
plot(model) #And check 1st and 3rd graph
# Analyzing the plots, we see a lack of homscedasticity (scale location plot does not show a horizontal line with equally distant points)
# But in the residuals vs fitted plot we see an horizontal line, which could be indicative of homoscedasticity, except we can see some larger residuals, evidence of heteroscedasticity

# .3 testing for independence
# We test independence of errors by applying the durbin watson test
# We compare the test statistic with the value 2
# When statistic  <2 --> positive autocorrelation
# When >2 --> negative autocorrelation
# When = 2 --> zero autocorrelation
durbinWatsonTest(model)
#Statistic= 1.964226 and p-value=0.05
# 1.96 ~= 2, zero autocorrelation (in practice we find 1.5 and 2.5 to be the tresholds for autocorrelation)

#Model:
#Now we need to analyze and interpret our model
summary(model)
#returns p-values and coefficients, as well as r-squared.
# We have that:
# dias_int= -5.89 -0.022idade + 0.007Poderdecompra - 0.05Taxadeemprego + 6.66severidade + 2.67mortalidade
# our R-quared values are low
