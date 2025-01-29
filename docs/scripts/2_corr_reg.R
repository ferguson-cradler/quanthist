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
library(readxl)
library(writexl)



#################### CORRELATION ANALYSIS ###################

# Introductory graph
x <- (1:30)     # range of x
n <- length(x)  # number of obs.

a = 3         # constant
b = 0.3         # coefficient
sd = 0.5        # standard deviation of the error term
set.seed(1)
y = a + b*x + sd*rnorm(n)   # the error term is normally distributed (mean = =; sd)
sim_data0 <- data.frame(x, y)
sim_data0 <- as_tibble(sim_data0)
sim_data0 <- sim_data0 %>%
  mutate(type = "positive")

a = 7.5         # constant
b = 0         # coefficient
sd = 3        # standard deviation of the error term
set.seed(222)
y = a + b*x + sd*rnorm(n)   # the error term is normally distributed (mean = =; sd)
sim_data1 <- data.frame(x, y)
sim_data1 <- as_tibble(sim_data1)
sim_data1 <- sim_data1 %>%
  mutate(type = "no correlation")

a = 10         
b = -0.2         
sd = 1.3
set.seed(123)
y = a + b*x + sd*rnorm(n)   # the error term is normally distributed (mean = =; sd)
sim_data2 <- data.frame(x, y)
sim_data2 <- sim_data2 %>%
  mutate(type = "negative")
sim_data2 <- as_tibble(sim_data2)

sim_data <- bind_rows(sim_data0, sim_data1, sim_data2) %>% 
  mutate(type = factor(type, 
                       levels = c("positive", "negative", "no correlation"), 
                       ordered = TRUE))

sim_data %>%  
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~ type, nrow = 1)


#### Numerical variables ####

data <- read_rds("data/paisley_v2.rds")

view(data)

# Describing the data first: age, weight
data %>% 
  summarize(count_age = sum(!is.na(age)),   
            mean_age = mean(age, na.rm = TRUE),
            count_weight = sum(!is.na(weight)),   
            mean_weight = mean(weight, na.rm = TRUE))

# Let's express weight in kgs (instead of pounds)
data <- data %>%
  mutate(weight_kg = 0.453592*weight)



### Visually
  # Scatterplot: Plotting the relationship between two variables
data %>%  
  ggplot() +
  geom_point(aes(x = age, y = weight_kg)) 

  # focusing only on those younger than 20
data %>%  
  filter(age <= 19) %>%
  ggplot() + 
    geom_point(mapping = aes(x = age, y = weight_kg)) + 
  scale_x_continuous(limits = c(9,19), breaks = seq(9,19, by = 1)) +
  scale_y_continuous(limits = c(20,80), breaks = seq(20,80, by = 10))
      # editing how the xaxis looks like

  # focusing now on those aged 20 and older
data %>%  
  filter(age >= 20) %>%
  ggplot() + 
  geom_point(mapping = aes(x = age, y = weight_kg)) 


## Pearson correlation coefficient
  # it measures the direction and the strenght of the association between two variables
  # it ranges between -1 to 1 being:
    #  1 - perfect positive correlation
    #     (strong - moderate - weak)
    #  0 - no correlation
    #     (strong - moderate - weak)
    # -1 - perfect negative correlation

  # it can be computed for numerical, ordinal and qualitative variables 
    # but employing different methods

# compute it using age & weight (both numerical)
data %>%  
  filter(age >= 20) %>%
  summarize(cor = cor(age, weight_kg, method = "pearson", use = "complete.obs")) 
    # -0.185
    # "use" indicates how to handle missing values
    # other options: "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"

data %>%  
  filter(age >= 20 & age < 50) %>%
  group_by(sex) %>%
  summarize(cor=cor(age, weight_kg, method = "pearson", use = "complete.obs")) 
    # the negative link between age and weight is much stronger in women

# visually:
data %>%  
  filter(age >= 20) %>%
  ggplot() + 
    geom_point(aes(x = age, y = weight_kg)) +
    facet_wrap(~ sex, nrow = 2)

data %>%  
  filter(age >= 20) %>%
  ggplot() + 
  geom_point(aes(x = age, y = weight_kg, col = sex))


## Illustrative graph - regression
x <- (1:30)     # range of x
n <- length(x)  # number of obs.

a = 0           # constant
b = 6.2         # coefficient
c = -0.2
sd = 0.5        # standard deviation of the error term
set.seed(2222)
y = a + b*x + +c*x^2 + sd*rnorm(n)   # the error term is normally distributed (mean = =; sd)
sim_data3 <- data.frame(x, y)
sim_data3 <- as_tibble(sim_data3)

sim_data3 %>%  
  summarize(cor = cor(x, y, method = "pearson", use = "complete.obs")) %>%
  mutate_if(is.numeric, round, 3)

sim_data3 %>%  
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point() +  
  annotate(x=25, label="r = -0.013", y=41, colour="red", geom = "label", size = 3)


#### Regression analysis

# Correlation analysis assesses the direction and strength of the relationship 
  # within a scale between 0-1 (whether two variables move together)

# Regression analysis goes beyond and allows:
  # (1) assessing the actual impact of X on Y: coefficient b
  # (2) computing how much of the variation in Y is explained by X (or Xs): R-squared
  # (3) allows controlling directly for the effect of other variables

data %>%  
  filter(age >= 20) %>%
  ggplot(mapping = aes(x = age, y = weight_kg)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

# Regression analysis basically finds the best line to fit the data (OLS: Ordinary Least Squares)
  # (the line minimising the sum of the deviations between the observed and predicted values)
  #   (the deviations are squared in order to compare negative and positive deviations)
  # "lm" refers to "linear model": Y = a + b*X   
  # y is the variable we want to "explain" (weight; dependent variable)
  # x is the explanatory variable (age)
  # se refers to standard errors (the confidence intervals) - let's not report then for now

# Estimate the regression line (intercept/slope)  
  # R (or any other statistical software) does the job for you

library(modelr)

lm(weight_kg ~ age, data = filter(data, age>=20))
  # where age is the explanatory variable (x) and weight_kg the dependent variable (y)

# or with pipe
data %>%
  filter(age>=20) %>%
  lm(weight_kg ~ age, data = .)

  # interpret the results:
    # y = a + b*x
    # weight = 66.8 - 0.13*age
    # intercept: if x = 0 -> weight = 66.8
    # slope: one-unit increase in X (age: 1 year) reduces Y (weight) by 0.13 units (kgs.)
    # (always think about of units of measurement both in X and Y)


# Predicted values: estimating what Y will be depending on Y

reg <- data %>% 
  filter(age>=20) %>%
  lm(weight_kg ~ age, data = .) 

data <- data %>%
  add_predictions(reg, var = "pred")
view(data)

data %>%  
  filter(age >= 20) %>%
  ggplot(mapping = aes(x = age, y = weight_kg)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) + # deactivating this line shows only the predicted values
    geom_point(aes(x = age, y = pred), colour = "red", size = 1) 

# Residuals: difference between the observed and predicted values

label <- expression(e[i]) # illustrative graph
data %>%  
  filter(age >= 20) %>%
  ggplot(mapping = aes(x = age, y = weight_kg)) +
    geom_segment(aes(x = 69, xend = 69, y = 73, yend = 58), colour = "red", size = 0.5) +
    annotate(x=71, y=66, colour="red", geom = "label", size = 3, label=label) +
    geom_segment(aes(x = 66, xend = 66, y = 44.5, yend = 58.5), colour = "red", size = 0.5) +
    annotate(x=68, y=51.5, colour="red", geom = "label", size = 3, label=label) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

# Add the predicted values and the residuals to the dataframe
data <- data %>%
  add_predictions(reg, var = "pred") %>%
  add_residuals(reg, var = "resid") 

data %>% 
  filter(age>=20 & !is.na(weight_kg)) %>%
  select(age, weight_kg, pred, resid) 

# Ordinary Least Squares (OLS): finds the line that best fits the data
  # estimates the line that minimises the sum of the deviations between the observed values (the dots) and the predicted values 
    # (the deviations are squared in order to avoid negative and positive deviations cancelling each other out)

data %>%  # Illustrative graph only
  filter(age >= 20) %>%
  ggplot(mapping = aes(x = age, y = weight_kg)) +
    geom_point() +
    geom_abline(intercept = 62, slope = -.04, colour = "red") +
    geom_abline(intercept = 65, slope = -.10, colour = "red") +
    geom_abline(intercept = 63, slope = -.03, colour = "red") +
    geom_abline(intercept = 65, slope = -.04, colour = "red") +
    geom_abline(intercept = 68, slope = -.17, colour = "red") +
    geom_abline(intercept = 68, slope = -.14, colour = "red") +
    geom_abline(intercept = 68, slope = -.10, colour = "red") +
    geom_smooth(method = "lm", se = FALSE, colour = "blue", linewidth = 1)  


## R-squared
  # Fraction of the variation on Y that is explained by the model 
  # It ranges between 0 and 1
    # Low R2 does not neccessarily mean that the model is useless
    # It has to be interpreted based on expectations
    # The coefficient "b" can still provide useful info about the effect of X on Y

# Illustrative graph
x <- (1:30)     # range of x
n <- length(x)  # number of obs.

a = 0.2         # constant
b = 0.3         # coefficient
sd = 1        # standard deviation of the error term
set.seed(11111)
y = a + b*x + sd*rnorm(n)   # the error term is normally distributed (mean = =; sd)
sim_data0 <- data.frame(x, y)
sim_data0 <- as_tibble(sim_data0)
sim_data0 <- sim_data0 %>%
  mutate(type = "high")

a = 0.2         # constant
b = 0.3         # coefficient
sd = 2.5        # standard deviation of the error term
y = a + b*x + sd*rnorm(n)   # the error term is normally distributed (mean = =; sd)
sim_data1 <- data.frame(x, y)
sim_data1 <- as_tibble(sim_data1)
sim_data1 <- sim_data1 %>%
  mutate(type = "low")

sim_data <- bind_rows(sim_data0, sim_data1) # combine both # reorder the levels: pos, neg, no

reg0 <- sim_data %>% 
  filter(type=="high") %>%
  lm(y ~ x, data = .)

reg1 <- sim_data %>% 
  filter(type=="low") %>%
  lm(y ~ x, data = .)

summary(reg0)$r.squared 
summary(reg1)$r.squared 

new_labels <- c("high" = "R-squared = 0.85", "low" = "R-squared = 0.44")
sim_data %>%  
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~ type, nrow = 1, labeller = labeller(type = new_labels))
sim_data %>%  
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ type, nrow = 1, labeller = labeller(type = new_labels)) # report the R2


# Report R-squared
reg <- data %>% 
  filter(age>=20) %>%
  lm(weight_kg ~ age, data = .) # it does not automatically shows up

summary(reg)$r.squared # 0.03 - the model (age) explains 3 per cent of the variation in adult weight

# install.packages("moderndive")
library(moderndive)

data %>% 
  filter(age>=20) %>%
  lm(weight_kg ~ age, data = .) %>%
  get_regression_summaries() %>%
  select(nobs, r_squared)

data %>% 
  filter(age<20) %>%
  lm(weight_kg ~ age, data = .) %>%
  get_regression_summaries() %>%
  select(nobs, r_squared) # 0.695

# Regression tables: how age and adult weight are related for men and women separately?

# install.packages("modelsummary")
library(modelsummary)

reg_males <- data %>%
  filter(age>=20 & sex=="male") %>%
  lm(weight_kg ~ age, data = .)

reg_females <- data %>%
  filter(age>=20 & sex=="female") %>%
  lm(weight_kg ~ age, data = .)

modelsummary(
  list(
    "Males" = reg_males,
    "Females" = reg_females),
  statistic = NULL,
  gof_map = c("nobs", "r.squared"),
  fmt = 2) 

    # obviously these results are contingent on the data we are using:
      # smaller numer of female observations
      # also, are men and women comparable in this setting (who ended up in prison)?
        # controls? ...
      # biological vs social? --> single / married women? diets in prison?
    # non-linearities?
    # outliers


# Visually:

data %>%  
  filter(age >= 20) %>%
  ggplot(mapping = aes(x = age, y = weight_kg, color = sex)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)


### Qualitative variables 
  # categorical (or ordinal) variable can be used in regression analysis as dummy variables (0/1)
    # 1 category acts as a "reference category"
    # so the coefficient is interpreted "against" that category
    # if you have n categories, you need (n-1) dummies
    # i.e. sex has two categories: male/female, so you just need 1 dummy
      # either being male or being female 
    # no need to create dummy variables to include categorical info (R does it for you)

data %>% 
  filter(age>=20) %>%
  group_by(sex) %>%
  summarize(
    mean_height = mean(height, na.rm = TRUE))

data <- data %>% 
  mutate(male = ifelse(sex=="male", 1, 0)) # what to do with missing values, apparently still missing, check
data %>%
  count(male)

data %>% 
  subset(select=c(forename, sex, male)) %>% # check what you have done
  print(n = 20) 

data %>%
  filter(age >= 20) %>%
  lm(height ~ male, data = .) %>%
  get_regression_table() %>%
  select(term, estimate) # men are 12.5 centimeters taller than women

data %>%  
  filter(age >= 20) %>%
  ggplot(aes(x = male, y = height)) + # visually
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = 156.5729, color = "red", linetype = 2) +
  geom_hline(yintercept = 169.0388, color = "blue", linetype = 2) +
  scale_x_continuous(breaks = seq(0, 1, 1))

# doing the same with country of birth
data %>%
  filter(age >= 20 & sex=="male") %>%
  lm(height ~ countryb, data = .) %>%
  get_regression_table() %>%
  select(term, estimate)
  # no need to create the dummies yourself but you lose control of the reference category

# creating the dummies yourself
data <- data %>%  
  mutate(england = ifelse(countryb=="england", 1, 0)) %>%
  mutate(ireland = ifelse(countryb=="ireland", 1, 0)) %>%
  mutate(scotland = ifelse(countryb=="scotland", 1, 0)) %>%
  mutate(overseas = ifelse(countryb=="overseas", 1, 0))

data %>% 
  subset(select=c(forename, countryb, england, ireland, scotland, overseas)) %>%
  print(n = 20) 

data %>%
  filter(age >= 20 & sex=="male") %>%
  lm(height ~ england + ireland + overseas, data = .) %>%
  get_regression_table() %>%
  select(term, estimate)


### Qualitative variables as dependent variables (Y)
data %>%
  count(lit_adj)

data %>%
  group_by(lit_adj) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))

data <- data %>%
  mutate(write = ifelse(lit_adj=="write", 1, 0)) %>%
  mutate(male = ifelse(sex=="male", 1, 0))

data %>%
  count(write)

data %>% summarize(
  mean_write = mean(write, na.rm = TRUE))

data %>%
  lm(write ~ male, data = .) %>%
  get_regression_table() %>%
  select(term, estimate)
  # being male (going from 0, female, to 1, male) is associated with an increase in literacy of 0.07 
    # (or an increase of 7 percentile points if measured as percentages)



## Comparing the relative importance of different variables

reg_height <- data %>% 
  mutate(height = 30.48*feet+2.54*inches) %>% # in case not previously created
  filter(age>=20) %>%
  lm(weight_kg ~ height, data = .) 

reg_sex <- data %>% 
  mutate(male = if_else(sex=="male", 1, 0)) %>% # in case not created before
  filter(age>=20) %>%
  lm(weight_kg ~ male, data = .) 

modelsummary(
  list(
    "M1" = reg_height,
    "M2" = reg_sex),
  statistic = NULL,
  gof_map = c("nobs", "r.squared"),
  fmt = 3)


data %>% 
  mutate(height = 30.48*feet+2.54*inches) %>% 
  mutate(height_mts = height/100) %>%
  filter(age>=20) %>%
  lm(weight_kg ~ height_mts, data = .) %>%
  get_regression_table() %>%
  select(term, estimate)
  # changing the units of measurement does not change the results (only their scale) 


## Time as explanatory variable

data %>%
  mutate(year_birth = year - age) %>%
  filter(age >= 20) %>%
  lm(height ~ year_birth, data = .) %>%
  ggplot(aes(y = height, x = year_birth)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    annotate(x=1780, y=167, label="Y = 118.1 + 0.026*X", colour="red", geom = "label", size = 3)

# individual years as explanatory variables
data %>%
  mutate(year_birth = year - age) %>%
  mutate(year_birth = as_factor(year_birth)) %>%  
  filter(age >= 20) %>%
  lm(height ~ year_birth, data = .) %>%
  get_regression_table() %>%
  select(term, estimate) %>% 
  print(n = Inf)

# grouping years into age groups
data <- data %>%
  mutate(year_birth = year - age) %>%
  mutate(year_birth_10 = cut(year_birth, breaks =  seq(1775, 1885, 10)))
data %>%
  count(year_birth_10)

data %>%
  filter(age>=20 & year_birth>1795) %>%
  lm(height ~ year_birth_10, data = .) %>%
  get_regression_table() %>%
  select(term, estimate)


