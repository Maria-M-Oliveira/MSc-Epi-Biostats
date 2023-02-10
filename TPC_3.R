# Libraries
library(tidyverse)
library(ggplot2)
library(foreign)
library(qqplotr)
library(car)
library(emmeans)
library(DescTools)
library(FSA)
library(PMCMRplus)

#Files
BD <- read.spss("TPC_3_Caracterizacao_Concelhos_Saude.sav", to.data.frame = TRUE)
#########Exercise 1
# Compare region Norte and Lisbon according to number of doctors
# So what we have here is a quantitative variable (number of doctors) and a categorical variable (NUTS), meaning I want a t-test for independent samples
NL_BD <- BD %>% 
  subset(NUTS_II == "Norte" | NUTS_II == "Área Metropolitana de Lisboa")

#Produce boxplots and visually check for outliers
ggplot(NL_BD, aes(x = NUTS_II, y = Médicos, fill = NUTS_II)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  stat_summary(fun.y=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplots of Lisbon and North Region Groups") + 
  theme_bw() + theme(legend.position="none")

#Produce descriptive statistics by group
NL_BD %>% select(Médicos, NUTS_II) %>% group_by(NUTS_II) %>% 
  summarise(n = n(), 
            mean = mean(Médicos, na.rm = TRUE), 
            sd = sd(Médicos, na.rm = TRUE),
            stderr = sd/sqrt(n), 
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median=median(Médicos, na.rm = TRUE),
            min=min(Médicos, na.rm = TRUE), 
            max=max(Médicos, na.rm = TRUE),
            IQR=IQR(Médicos, na.rm = TRUE))

# QQ plots
ggplot(data = NL_BD, mapping = aes(sample = Médicos, color = NUTS_II, fill = NUTS_II)) +
  stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "ts") +
  stat_qq_line(identity=TRUE) +
  stat_qq_point(col="black") +
  facet_wrap(~ NUTS_II, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() 

# First, we need to test for assumptions:
# Normal distribuition in each group, and homogeneity of variances
# Starting with Shapiro-Wilk to test for normality
NL_BD %>%
  group_by(NUTS_II) %>%
  summarise(`W Statistic` = shapiro.test(Médicos)$statistic,
            `p-value` = shapiro.test(Médicos)$p.value)
# Norte p-value 8.15e-18 and Lisbon p-value 2.20e-7
# Both p-values are <0.05, meaning we reject the null hypothesis, so this data is not normally distributed
# We then need to do a non-parametric test: Mann-Whitney U 

# Before that, and for the sake of practice, lets test for homogeneity of variances
lev1<-leveneTest(Médicos ~ NUTS_II, data=NL_BD)
print(lev1)

# we get a p-value of 0.01281, meaning we reject the null hypothesis, so between group variances are unequal

# moving on to the Mann whitney U test
mann_whitney <- wilcox.test(Médicos ~ NUTS_II, data=NL_BD, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
mann_whitney
# We get a p-value of 3.405e-06, meaning we rejet the null hypothesis that distributions are equal and conclude that there is a significant difference in the number of doctors 


########## Exercise 2
# diferencas estatisticamente significativas entre os valores medios por concelho do número de medicos e do numero de enfermeiros
#  We want to compare the number of doctors and nurses, a paired sample

BD <- BD %>% 
  mutate(diff = Médicos - Enfermeiros)

BD %>% select(Médicos)  %>% 
  summarise(n = n(),
            mean = mean(Médicos, na.rm = TRUE), 
            sd = sd(Médicos, na.rm = TRUE),
            stderr = sd/sqrt(n), 
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median=median(Médicos, na.rm = TRUE),
            min=min(Médicos, na.rm = TRUE), 
            max=max(Médicos, na.rm = TRUE),
            IQR=IQR(Médicos, na.rm = TRUE))

BD %>% select(Enfermeiros) %>% 
  summarise(n = n(),
            mean = mean(Enfermeiros, na.rm = TRUE), 
            sd = sd(Enfermeiros, na.rm = TRUE),
            stderr = sd/sqrt(n), 
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median=median(Enfermeiros, na.rm = TRUE),
            min=min(Enfermeiros, na.rm = TRUE), 
            max=max(Enfermeiros, na.rm = TRUE),
            IQR=IQR(Enfermeiros, na.rm = TRUE))

# Testing for normality in diff
shapiro.test(BD$diff) #p-value <2.2e-16, not normally distributed

# QQ plot of diff
ggplot(data = BD, mapping = aes(sample = diff)) +
  stat_qq_band(alpha=0.25, conf=0.95, qtype=1, bandType = "boot", B=5000, fill="red") +
  stat_qq_line(identity=TRUE) +
  stat_qq_point(col="black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() 

# Boxplot of diff
ggplot(BD, aes(x = "", y = diff)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  stat_summary(fun.y=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplot of doctors - nurses differences") + 
  theme_bw() + theme(legend.position="none")

# Since we rejected normality, we must perform a non parametric test: wilcoxon 
wilcox <- wilcox.test(BD$Médicos, BD$Enfermeiros,na.rm=TRUE, paired=TRUE, exact=FALSE, conf.int=TRUE)
wilcox #p-value <2.2e-16, we reject the null hypothesis

######## Exercise 3
# comparar todas as regioes quanto ao numero de medicos
Med_BD <- BD %>% 
  select(Médicos, NUTS_II)

ggplot(Med_BD, aes(x = NUTS_II, y = Médicos, fill = NUTS_II)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  stat_summary(fun.y=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplots of Lisbon and North Region Groups") + 
  theme_bw() + theme(legend.position="none")

#Produce descriptive statistics by group
Med_BD %>% select(Médicos, NUTS_II) %>% group_by(NUTS_II) %>% 
  summarise(n = n(), 
            mean = mean(Médicos, na.rm = TRUE), 
            sd = sd(Médicos, na.rm = TRUE),
            stderr = sd/sqrt(n), 
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median=median(Médicos, na.rm = TRUE),
            min=min(Médicos, na.rm = TRUE), 
            max=max(Médicos, na.rm = TRUE),
            IQR=IQR(Médicos, na.rm = TRUE))

# QQ plots
ggplot(data = Med_BD, mapping = aes(sample = Médicos, color = NUTS_II, fill = NUTS_II)) +
  stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "ts") +
  stat_qq_line(identity=TRUE) +
  stat_qq_point(col="black") +
  facet_wrap(~ NUTS_II, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() 

# Since we want to compare multiple groups, we want to perform an ANOVA test or Kruskall-Wallis
# First, we need to test for assumptions:
# Normal distribuition in each group, and homogeneity of variances
# Starting with Shapiro-Wilk to test for normality
Med_BD %>%
  group_by(NUTS_II) %>%
  summarise(`W Statistic` = shapiro.test(Médicos)$statistic,
            `p-value` = shapiro.test(Médicos)$p.value)
# All of them present a p-value <0.05,we reject the null hypothesis, so this data is not normally distributed
# We then need to do a non-parametric test: Kruskall Wallis 

# Before that, and for the sake of practice, lets test for homogeneity of variances
lev2<-leveneTest(Médicos ~ NUTS_II, data=Med_BD)
print(lev2)

# we get a p-value of 0.0001606, meaning we reject the null hypothesis, so between group variances are unequal

# Moving on to the ANOVA test
m1<-lm(Médicos~ NUTS_II, data=Med_BD, contrasts = c("contr.sum", "contr.poly"))
Anova(m1, type=3)

#Compute expected marginal means post-hoc tests
posthocs<-emmeans(m1, pairwise ~ NUTS_II, adjust="tukey")
posthocs

#Plot estimated marginal means
emm <- summary(posthocs)$emmeans
ggplot(emm, aes(NUTS_II)) +
  geom_line(aes(y = emmean, group = 1)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_point(aes(y = emmean), size = 2, color = "blue") +
  labs(x = "Treatment", y = "Estimated marginal mean", 
       title = "Estimated marginal means with 95% confidence intervals") +
  theme_bw()

#Plot contrasts
plot(posthocs$contrasts) +
  geom_vline(xintercept = 0) + 
  theme_bw() +
  labs(y = "Contrast", 
       x = "Estimated marginal mean difference", 
       title = "Estimated marginal mean differences with 95% confidence intervals")

#Perform the Kruskal-Wallis test
m1<-kruskal.test(Médicos ~ NUTS_II, data=Med_BD)
print(m1) #p-value 1.19e-11, reject null hypotesis, so there are differences in median values between groups

#Dunn's Kruskal-Wallis post-hoc test
posthocs1<-dunnTest(Médicos ~ NUTS_II, data=Med_BD, method="holm")
print(posthocs1)

#Dwass, Steel, Critchlow, Fligner post-hoc test
posthocs2<-dscfAllPairsTest(Médicos ~ NUTS_II, data=Med_BD)
print(posthocs2)
