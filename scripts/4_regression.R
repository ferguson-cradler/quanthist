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
library(ggplot2)
library(moderndive)
library(modelsummary)
library(marginaleffects)

# Import the data
data <- read_rds("data/paisley_v2.rds")


#### MULTIPLE REGRESSION ANALYSIS 

# Regression analysis goes beyond and allows:
# (1) assessing the actual impact of X on Y: coefficient b
# (2) computing how much of the variation in Y is explained by X (or Xs): R-squared
# (3) allows controlling directly for the effect of other variables ***********************

# Prepare the data (just in case)
data <- data %>%
  mutate(
    weight_kg = 0.453592*weight,
    height = 30.48*feet+2.54*inches,
    male = if_else(sex=="male", 1, 0),
    england = if_else(countryb=="england", 1, 0),
    ireland = if_else(countryb=="ireland", 1, 0),
    scotland = if_else(countryb=="scotland", 1, 0),
    overseas = if_else(countryb=="overseas", 1, 0),
    read = recode(lit_adj,
                  "illiterate" = 0,
                  "read" = 1,
                  "write" = 1),
    write = if_else(lit_adj=="write", 1, 0),
    urban = case_when(born=="glasgow" ~ 1,
                           born=="port glasgow" ~ 1,
                           born=="edinburgh" ~ 1, 
                           born=="liverpool" ~ 1,
                           born=="dublin" ~ 1,
                           born=="london" ~ 1,
                           born=="barcelona" ~ 1,
                           born=="birmingham" ~ 1,
                           born=="manchester" ~ 1,
                           TRUE ~ 0))

# Multiple explanatory variables
reg <- data %>%
  filter(age>=20 & countryb!="overseas") %>% 
  lm(weight_kg ~ age + height + male + write, data = .)

get_regression_summaries(reg) %>%
  select(nobs, r_squared, adj_r_squared) # R-squared

reg %>%
  get_regression_table() %>%
  select(term, estimate) # regression coefficients

reg %>%
  get_regression_table() %>%
  select(!statistic) # tables including standard errors, confidence intervals, etc.


# Visualising regression results

data %>%
  filter(age>=20 & countryb!="overseas") %>% 
  ggplot(aes(x=age, y=weight_kg)) +
  geom_point() +
  geom_smooth(method = "lm")
  

## two possibilities
library(marginaleffects)
reg %>%
  plot_predictions("age") # the link between one X and Y 

library(modelsummary)

modelplot(reg, coef_omit = 'Interc') 
  # all the coefficients together
  # all the coefficients simultaneously

### What happens to the model when we add more explanatory variables??
  # (1) it usually increases the explanatory power: R-squared 
  # (2) the effect of the other variables may change 
  # (3) the estimations may become noisier 

# Illustration: three regresssions explaining heights (restricted sample)
sample <- data %>%
  filter(age>=20 & age<=30 & countryb!="overseas" & countryb!="ireland")

reg1 <- sample %>%
  lm(height ~ write, data = .)
reg2 <- sample %>%
  lm(height ~ write + male, data = .)
reg3 <- sample %>%
  lm(height ~ write + male + england, data = .)

modelsummary(
  list(
    "M1" = reg1,
    "M2" = reg2,
    "M3" = reg3),
  statistic = c("s.e. = {std.error}", "p-value = {p.value}"),
  stars = c("*" = .1, "**" = .05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  fmt = 3,
  coef_omit = "Intercept",
  note = "For simplicity, the intercept is omitted.") 

## (1) Adding more variables increase the R-squared providing the new variables are relevant. 

## (2) Adding an additional variables to the model may change the coefficients of the other variables. 
  # omitted variable bias: if an omitted variable (Z) is correlated with both X and Y, 
    # the regression coefficient is unreliable:
      # b not only reflects the effect of X on Y, but it also partly captures the effect of Z on Y
    # controlling for Z (including Z in the model) addresses this issue

# the effect of adding additional variables to the existing coefficients basically reflects:
  # --the correlation between the added variable and the dependent variable (Y)
  # --the correlation between the added variable and the previously existing explanatory variables.

# correlation matrix
data %>%
  filter(age>=20 & age<=30 & countryb!="overseas" & countryb!="ireland") %>%  
  select(height, write, male, england) %>%
  datasummary_correlation()

## (3) The estimations may become noisier

# adding more variables increases the number of coefficients to be estimated with the same information (sample size). 
  # reducing the degrees of freedom

# the regression coefficients are computed using only independent information (exclusive to one particular variable). 
  # If two of the explanatory variables are correlated between each other, they share information. 
  # The procedure behind regression cannot use the shared information to estimate their coefficients, 
  # so that information is discarded and $b$ is estimated using only the *independent* information remaining. 
  # Having less information increases the standard errors, thus reducing the accuracy of the estimations: 
  # confidence intervals wider and p-values become larger


## Regression tables: very useful to compare several models in just one table  
  # assess how our results behave under different specifications: 
    # including more/less variables
    # differences across groups or time-periods, 
    # excluding outliers, 
    # etc. 

reg_all <- data %>%
  filter(age>=20) %>%
  lm(weight_kg ~ age + sex + height + england + ireland + overseas, data = .)
reg_males <- data %>%
  filter(age >= 20 & sex=="male") %>%
  lm(weight_kg ~  age + height + england + ireland + overseas, data = .)
reg_females <- data %>%
  filter(age >= 20 & sex=="female") %>%
  lm(weight_kg ~ age + height + england + ireland + overseas, data = .)
reg_all2 <- data %>%
  filter(age>=20 & age<=50 & countryb!="overseas") %>%
  lm(weight_kg ~ age + sex + height + england + ireland + overseas, data = .)
reg_males2 <- data %>%
  filter(age >= 20 & sex=="male" & age<=50 & countryb!="overseas") %>%
  lm(weight_kg ~  age + height + england + ireland + overseas, data = .)
reg_females2 <- data %>%
  filter(age >= 20 & sex=="female" & age<=50 & countryb!="overseas") %>%
  lm(weight_kg ~ age + height + england + ireland + overseas, data = .)

modelsummary(
  list(
    "1. All" = reg_all,
    "2. Males" = reg_males,
    "3. Females" = reg_females,
    "4. All" = reg_all2,
    "5. Males" = reg_males2,
    "6. Females" = reg_females2),
  statistic = "std.error",
  stars = c("*" = .1, "**" = .05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  coef_omit = "Intercept",
  fmt = 2,
  note = "For simplicity, the intercept is omitted.")



## Quantifying time (controlling for other confounders)

sample <- data %>%
  mutate(year_birth = year - age) %>%
  filter(age>=20 & year_birth>1795)

m1 <- sample %>%
  lm(height ~ year_birth, data = .)

m2 <- sample %>%
  lm(height ~ year_birth + sex + england + ireland + overseas, data = .)

modelsummary(
  list(
    "(1)" = m1,
    "(2)" = m2),
  statistic = "std.error",
  stars = c("*" = .1, "**" = .05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  coef_omit = "Intercept",
  fmt = 2,
  note = "For simplicity, the intercept is omitted.")  



### Choosing which variable introduce in the analysis should be theoretically justified

# Also:
  # introducing additional *relevant* variables (Z) is helpful for two main reasons:
    # -- adds more information, thus improving the accuracy of our estimations 
          # (if X and Z are not correlated)
    # -- it prevents the **omitted variable bias** if X and Z are correlated. 
          # if an omitted variable (Z) is correlated with both X and Y, 
            # the regression coefficient is unreliable
  
  # however, adding more variables may also reduce accuracy due to:
    # -- reducing the degrees of freedom (df=n-k-1)
    # -- multicolinearity problems: 
      # if X and Z are correlated, coefficients are estimated with less *independent* information, 
        # which increases the standard errors and reduces the likelihood of being statistically significant 
          # (confidence intervals also become wider)

  # The omitted variable bias is arguably a much worse problem than multicolinearity, 



### Challenges to regression analysis
  # Non-linear relationships (functional form)
  # Parameter stability
  # Outliers

### Correlation is not causation
# Role of omitted variables
# Reverse causality

### Other issues
# Number of observations (noise)
# Categories employed
# Garbage in, garbage out (the results are as good as the data itself)


# Functional form:
data %>%
  ggplot(aes(x=age, y=weight_kg)) +
  geom_point() +
  geom_smooth(method = "lm")

reg <- data %>%
  mutate(age_sq = age*age) %>%
  lm(weight_kg ~ age + age_sq, data = .)
  
data %>%
  ggplot(aes(x=age, y=weight_kg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))

data %>%
  ggplot(aes(x=age, y=weight_kg)) +
  geom_point() +
  geom_smooth()

