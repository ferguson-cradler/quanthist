# Start afresh
rm(list=ls()) # Clear de "Global Environment"

# set working directory
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM_2024/session") 

# Install packages (if needed)
# install.packages("tidyverse")
# install.packages("xlsx")
# install.packages("writexl")


# Load the packages you need
library(tidyverse)
library(dplyr)
library(modelr)

data <- read_rds("data/paisley_v2.rds")
data <- data %>%
  mutate(weight_kg = 0.453592*weight)


#################### Dealing with uncertainty ###################

# What we observe is influenced by chance
# What would have happened if we would have observed a different set of records (sample)

# We draw conclusion about the characteristics of the population
  # based on a sample of observations
  # this sample statistic however will deviate from the true value
  # --> sampling error, which depends on:
    # the variability within the population (st. dev.)
    # the number of observations in the sample
  # --> we use the standard error to compute:
    # confidence intervals
      # it will cover the true value with certain probability
    # testing hypothesis:
      # assume a hypothesis to be true
      # assess the probability (p-value) of what is observed
        # based on the previous assumption
          # the probability that the observed outcome would
            # have happened by chance



### Standard error
data %>%
  filter(sex=="male" & age>=20 & countryb!="") %>%
  group_by(countryb) %>%
  summarise(
    obs = sum(!is.na(height)), 
    mean = mean(height, na.rm = TRUE),
    sd = sd(height, na.rm=TRUE),
    se = sd/sqrt(obs)) %>%
  mutate_if(is.numeric, round, 2)
  # larger sample size, lower standard errors

### Confidence intervals

ci <- data %>%
  filter(sex=="male" & age>=20 & age<=50) %>%
  group_by(countryb) %>%
  summarise(
    obs = sum(!is.na(height)), 
    mean = mean(height, na.rm = TRUE),
    st.dev = sd(height, na.rm=TRUE),
    st.error = sd/sqrt(obs), 
    lb_ci = t.test(height, conf.level = 0.95)$conf.int[1],      # CI lower bound
    ub_ci = t.test(height, conf.level = 0.95)$conf.int[2]) %>%  # CI upper bound
  mutate_if(is.numeric, round, 1) # add 1 decimal place
ci

# Visually
ci %>%  
  ggplot(aes(x = countryb, y = mean)) +
    geom_point() +
    geom_errorbar(aes(x=countryb, ymin=lb_ci, ymax= ub_ci), width = 0.1)

  # using sex instead
ci_sex <- data %>%
  filter(age>=20 & countryb=="england") %>%
  group_by(sex) %>%
  summarise(
    obs = sum(!is.na(height)), 
    mean = mean(height, na.rm = TRUE),
    st.dev = sd(height, na.rm=TRUE),
    st.error = sd/sqrt(obs), 
    lb_ci = t.test(height, conf.level = 0.95)$conf.int[1],   # CI lower bound
    ub_ci = t.test(height, conf.level = 0.95)$conf.int[2])   # CI upper bound

ci_sex %>% ggplot(aes(x = sex, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=sex, ymin=lb_ci, ymax= ub_ci), width = 0.3)



# Confidence level

ci99 <- data %>%
  filter(sex=="male" & age>=20 & age<=50) %>%
  group_by(countryb) %>%
  summarise(
    obs = sum(!is.na(height)), 
    mean = mean(height, na.rm = TRUE),
    lb_ci = t.test(height, conf.level = 0.99)$conf.int[1],      # CI lower bound
    ub_ci = t.test(height, conf.level = 0.99)$conf.int[2]) %>%  # CI upper bound
  mutate(conf_level = "Conf. level = 99")
ci99
ci90 <- data %>%
  filter(sex=="male" & age>=20 & age<=50) %>%
  group_by(countryb) %>%
  summarise(
    obs = sum(!is.na(height)), 
    mean = mean(height, na.rm = TRUE),
    lb_ci = t.test(height, conf.level = 0.90)$conf.int[1],      # CI lower bound
    ub_ci = t.test(height, conf.level = 0.90)$conf.int[2]) %>%  # CI upper bound
  mutate(conf_level = "Conf. level = 90")
ci90
ci90_99 <- bind_rows(ci99, ci90)
ci90_99
ci90_99 %>%  
  ggplot(aes(x = countryb, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=countryb, ymin=lb_ci, ymax= ub_ci), width = 0.1) +
  facet_wrap(~ conf_level, nrow = 1)


# Apply the same idea to changes over time
data %>%
  filter(sex=="male" & age>=18 & age<=50 & year>1846) %>%
  group_by(year) %>%
  summarise(
    mean = mean(weight_kg, na.rm = TRUE),
    lb_ci = t.test(weight_kg, conf.level = 0.95)$conf.int[1],      # CI lower bound
    ub_ci = t.test(weight_kg, conf.level = 0.95)$conf.int[2]) %>%  # CI upper bound
  ggplot(aes(x = year, y = mean)) +
    geom_point() +
    geom_errorbar(aes(x = year, ymin = lb_ci, ymax = ub_ci), width = 0.1)

# this solution is slightly different
data %>%
  filter(sex=="male" & age>=18 & age<=50 & year>1846) %>%
  group_by(year) %>%
  summarise(
    mean = mean(weight_kg, na.rm = TRUE),
    lb_ci = t.test(weight_kg, conf.level = 0.95)$conf.int[1],      # CI lower bound
    ub_ci = t.test(weight_kg, conf.level = 0.95)$conf.int[2]) %>%  # CI upper bound
  ggplot(aes(x = year, y = mean)) +
    geom_smooth(se = TRUE) +
    geom_point() +
    geom_errorbar(aes(x = year, ymin = lb_ci, ymax = ub_ci), width = 0.1)


### Confidence intervals in correlation and regression analysis
  # the results also come from a particular sample (what if...)

# Correlation
data %>%  
  filter(age >= 20) %>%
  group_by(sex) %>%
  summarise(cor = cor(age, weight_kg, method = "pearson", use = "complete.obs"),
            cor_ci_lb = cor.test(age, weight_kg)$conf.int[1],
            cor_ci_ub = cor.test(age, weight_kg)$conf.int[2]) 

# Regression
library(modelr)
library(moderndive)

data %>% 
  mutate(weight_kg = 0.453592*weight) %>%
  filter(age>=20) %>%
  lm(weight_kg ~ age, data = .) %>%
  get_regression_table() %>%
  select(term, estimate, std_error, lower_ci, upper_ci) %>%
  mutate_if(is.numeric, round, 2)

# visually 
data %>% 
  mutate(weight_kg = 0.453592*weight) %>%
  filter(age>=20) %>%
  ggplot(aes(y = weight_kg, x = age)) +
    geom_point() +
    geom_smooth(method = "lm") # se = TRUE


### Hypothesis testing, t-tests, and p-values
  # asking concrete questions and let the statistics reject the hypotheses or not
    # null hypothesis (H0): reflects a conservative attitude towards the potential findings 
      # by specifying the negative form of a proposition. 
        # there is not difference between two means 
        # that there is no relationship between two variables. 
    # research (alternative) hypothesis (H1) specifies the opposite

  # the p-value assess the probability that the observed outcome would be present 
    # if the null hypothesis were true 
    # (the probability that the observed outcome would have happened by chance). 
      # if the probability of having observed that outcome is very low, 
        # we should therefore consider rejecting the null hypothesis, 
          # that there is no difference between the means or 
          # that there is no correlation between two variables). 

  # as with confidence intervals, we reject hypotheses with a certain probability (**confidence level**). 
    # in order to decide whether we reject the null or not, 
      # the *p-value* is compared against α, the **significance level**, 
        # which basically mirrors the confidence level (α = 100 minus the confidence level):
      # -- if p-value is large (>α), the data supports the null hypothesis
      # -- if p-value is small (<α), the evidence is against the null hypothesis

# Example
data %>%
  filter(countryb=="scotland" & sex=="male" & age>=20 & age<=50) %>%
  summarise(
    obs = sum(!is.na(height)), 
    mean = mean(height, na.rm = TRUE)) # average height

# One-sample test
  # Scottish males are 171 cms on average (from other source)
  # are our prisoners similar?

data %>%
  filter(countryb=="scotland" & sex=="male" & age>=20 & age<=50) %>%  
  t.test(height ~ 1, mu = 171, data = .,
         conf.level = 0.95)

# Two-sample test: comparing groups
data %>%
  filter(countryb=="scotland" | countryb=="ireland") %>%  
  filter(sex=="male" & age>=20 & age<=50) %>%
  t.test(height ~ countryb, data = .,
         conf.level = 0.95)



## Hypothesis testing in regression analysis

reg_m <- data %>% 
  filter(age>=20 & sex=="male") %>%
  lm(weight_kg ~ age, data = .) 

reg_f <- data %>% 
  filter(age>=20 & sex=="female") %>%
  lm(weight_kg ~ age, data = .) 

modelsummary(
  list(
    "Males" = reg_m,
    "Females" = reg_f),
  statistic = c("s.e. = {std.error}", "p-value = {p.value}"),
  stars = c("*" = .1, "**" = .05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  fmt = 3,
  coef_omit = "Intercept",
  note = "For simplicity, the intercept is not reported") 

