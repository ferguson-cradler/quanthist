##### Bi-variate statistics #######

# clear de "Global Environment"
rm(list=ls()) 

# set working directory
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM_2024/session") 

# upload basic packages
library(tidyverse)
library(readxl)
library(ggplot2)

# import data
data <- read_excel("data/paisley_data.xlsx")

data2 <- read_rds("paisley_v2.rds")


### Qualitative variables

# Cross-tabulation / contingency table
view(data)

data %>%
  count(employed)

data %>%  
  count(sex, employed)

data %>%  
  count(sex, employed) %>%
  pivot_wider(names_from = employed, values_from = n)


table3 <- data %>%  
  count(sex, employed) %>%
  pivot_wider(names_from = employed, values_from = n) %>%
  mutate(total = employed + unemployed,
         emp_perc = 100*employed/total,
         unemp_perc = 100*unemployed/total) %>%
  mutate_if(is.numeric, round, 1) 

table3 <- table3 %>%
  relocate(sex, employed, emp_perc, unemployed, unemp_perc, total)

table3


# Visualisation

data %>% 
  filter(countryb == "scotland" & age>=16) %>%  
  ggplot(aes(x = literacy)) + 
    geom_bar() +
    coord_flip() +
    facet_wrap(~ sex, nrow = 1)

data %>% 
  filter(countryb == "scotland" & age>=16) %>%  
  ggplot(mapping = aes(x = literacy, y = after_stat(prop), group = 1)) + 
    geom_bar() +
    coord_flip() +
    facet_wrap(~ sex, nrow = 1)

data %>% 
  filter(countryb == "scotland" & age>=16) %>%
  ggplot(aes(x = literacy, fill = sex)) + 
  geom_bar(position = "dodge") +
  coord_flip()

?ggplot

# 3 variables simultaneously

data <- data %>% 
  mutate(lit_rank = factor(literacy, 
                           levels = c("illiterate", 
                                      "read a little", 
                                      "read tolerably", 
                                      "read well", "cannot write", 
                                      "read & write a little", 
                                      "read & write tolerably", 
                                      "read & write well", 
                                      "superior education"), 
                           ordered = TRUE))

data %>% 
  filter(countryb == "scotland" | countryb == "ireland") %>%
  ggplot(aes(x = lit_rank, fill = sex)) + 
    geom_bar(position = "dodge2") +
    coord_flip() +
    facet_wrap(~ countryb, nrow = 1)


### Comparing numerical variables by group

# Means
data

data <- data %>%
  mutate(feet = replace(feet, feet == 50, 5))

data <- data %>%
  mutate(height = 30.48*feet + 2.54*inches)

data %>%
  ggplot(aes(x = height)) + 
  geom_histogram()

data %>% filter(feet>10) %>% select(feet, inches)



View(data)

data %>% 
  filter(countryb=="ireland" | countryb=="scotland" | countryb=="england") %>%
  group_by(countryb) %>%
  summarize(
    obs = sum(!is.na(height)),
    mean_height = mean(height, na.rm = TRUE)) 

# %>% mutate_if(is.numeric, round, 3)

# Distributions

data %>%   
  filter(countryb=="ireland" | countryb=="scotland" | countryb=="england") %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ countryb, nrow = 1)
# also very different sample sizes

data %>%   
  filter(countryb=="ireland" | countryb=="scotland" | countryb=="england") %>%
  ggplot(mapping = aes(x = height, y = ..density..)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ countryb, nrow = 1)
# difficult to eyeball differences between graphs

# Overlapping histograms (kernel density graphs)
data %>%   
  ggplot(aes(x = height, y = ..density..)) +
  geom_histogram(binwidth = 5) +
  geom_density(kernel = "gaussian", color = "red", size = 1.5)

data %>%   
  filter(countryb=="ireland" | countryb=="scotland" | countryb=="england") %>%  
  ggplot(aes(x = height, colour = countryb)) +
  geom_density()

# Boxplots
# another way of comparing distributions

data %>%   
  filter(countryb=="ireland" | countryb=="scotland" | countryb=="england") %>%  
  ggplot(aes(x = countryb, y = height)) +
  geom_boxplot()

# the solid line depicts the median value (50th percentile)
# the box contains 50 per cent of the observations 
# (those contained between the percentiles 25th and 75th (IQR).
# the vertical lines that extend below and above the box 
# reflect observations falling within 1.5 the interquartile range
# the black dots refer to the extreme values that all outside that range 

data %>%   
  filter(countryb=="ireland" | countryb=="scotland" | countryb=="england") %>%  
  ggplot(aes(x = countryb, y = height)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width=0.10, height=0.05)


## Compositional effects

data %>% 
  filter(countryb=="ireland" | countryb=="scotland" | countryb=="england") %>%
  filter(sex=="male" & age>=18 & age<=50) %>%
  group_by(countryb) %>%
  summarize(
    obs = sum(!is.na(height)),
    mean_height = mean(height, na.rm = TRUE),
  )


## With qualitative variables

lit_sex_countryb <- data %>% 
  mutate(write = ifelse(lit_adj=="write", 1, 0)) %>%
  filter(countryb %in% c("england", "ireland", "scotland")) %>%
  group_by(sex, countryb) %>%
  summarize(
    obs = sum(!is.na(write)), 
    write = mean(write, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 3) %>%
  pivot_wider(names_from = sex, values_from = c("obs", "write")) %>%
  relocate(countryb, obs_male, write_male, obs_female, write_female)
lit_sex_countryb


## Comparing multiple groups

data %>% 
  group_by(sex, countryb) %>%
  summarize(
    obs = sum(!is.na(height)), 
    height = mean(height, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>%
  pivot_wider(names_from = sex, values_from = c("obs", "height")) %>%
  relocate(countryb, obs_male, height_male, obs_female, height_female)


# Visually

data %>% 
  group_by(sex, countryb) %>%
  summarize(
    obs = sum(!is.na(height)), 
    height = mean(height, na.rm = TRUE)) %>% 
  ggplot(aes(x = countryb, y = height, color = sex)) +
  geom_point()

### Numerical variables

# Tables (class intervals)

data <- data %>%
  mutate(weight_kg = 0.453592*weight)

data %>% 
  filter(age>=20) %>%
  mutate(age_class = cut(age, breaks = seq(20, 90, 10))) %>%
  group_by(age_class) %>%
  summarise(obs = sum(!is.na(weight_kg)),
            mean_weight = mean(weight_kg, na.rm = TRUE))

# Line graph
data %>% 
  filter(age>=20) %>%
  mutate(age_class = cut(age, breaks = seq(20, 90, 10))) %>%
  group_by(age_class) %>%
  summarise(obs = sum(!is.na(weight)),
            mean_weight = mean(weight, na.rm = TRUE)) %>%
  ggplot(aes(x = age_class, y = mean_weight, group = 1)) +
  geom_line()

# Scatter plot
data %>%
  filter(age>=20) %>%
  ggplot(aes(x = age, y = weight)) +
  geom_point()

data %>%
  filter(age>=20) %>%
  ggplot(aes(x = age, y = weight_kg)) +
  geom_point() +
  geom_smooth(SE = FALSE) +
  stat_summary(geom = "line", fun.y = "mean", color = "red")   


# distinguishing by a third dimension
data %>%
  filter(age>=20) %>%
  ggplot(aes(x = age, y = weight_kg, color = sex)) +
  geom_point() +
  geom_smooth(se = FALSE)

### Evolution over time (history!)

# a table
height_year <- data %>%
  mutate(year_birth = year - age) %>%
  filter(sex=="male" & age>=20 & age<=50) %>%
  group_by(year_birth) %>%
  summarise(obs = sum(!is.na(height)),   
            av_height = mean(height, na.rm = TRUE))
height_year

# visually
height_year %>%
  ggplot(aes(x = year_birth, y = av_height)) +
  geom_point(col = "red") +
  geom_line()

# creating bins
data %>% 
  mutate(year_birth = year - age) %>%
  mutate(year_birth_5 = cut(year_birth, breaks =  seq(1795, 1885, 5))) %>%
  filter(sex=="male" & age>=20 & age<=50) %>%
  group_by(year_birth_5) %>%
  summarise(
    av_height = mean(height, na.rm = TRUE)) %>%
  ggplot(aes(x = year_birth_5, y = av_height)) +
  geom_point() + geom_line(group = 1)  +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# Given that the variable capturing the 5-year cohorts is a factor, 
# we need `group = 1` to let `ggplot` know that all the observations 
# should be treated as part of the same group, so the line can connect the points.

# Smoothing the trend
height_year %>%
  ggplot(aes(x = year_birth, y = av_height)) +
    geom_smooth(se = FALSE) +
    geom_point() +
    geom_line()

# Comparing groups
data %>% 
  mutate(year_birth = year - age) %>%
  filter(countryb=="scotland" & age>=20 & age<=50) %>%
  group_by(year_birth, sex) %>%
  summarise(
    av_height = mean(height, na.rm = TRUE)) %>%
  ggplot(aes(x = year_birth, y = av_height, color = sex)) +
  geom_point() +
  geom_smooth(se = FALSE)


# Other statistics

data %>%  
  mutate(year_birth = year - age) %>%
  filter(age >= 18 & sex=="male" & year_birth>=1800) %>%
  group_by(year_birth) %>%
  summarise(sd = sd(height, na.rm = TRUE),
            mean = mean(height, na.rm = TRUE)) %>% 
  mutate(cv = sd/mean) %>%
  ggplot(aes(x = year_birth, y = cv)) +
  geom_point() +
  geom_line() + 
  geom_smooth(se = FALSE)

# qualitative variables: dummy variables

data %>%
  mutate(write = ifelse(lit_adj=="write", 1, 0)) %>%
  mutate(year_birth = year - age) %>%  
  filter(age>=18 & age<=50) %>%
  group_by(year_birth, sex) %>%
  summarise(
    mean = mean(write, na.rm = TRUE)) %>%
  ggplot(aes(x = year_birth, y = mean, color = sex)) +
  geom_smooth(se = FALSE) +
  geom_point() 
