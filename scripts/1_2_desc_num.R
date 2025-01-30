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

data <- read_rds("data/paisley_v2.rds")

############## Numerical variables ############

# age, weight, height, ...
data

# Reporting frequencies is not useful when there are many values
data %>%  
  count(age) %>%              
  mutate(perc = 100*n/sum(n)) %>%
  print(n = Inf) # display all rows

paisley_data %>% 
  count(age) %>%
  print(n = Inf) # display all rows

# Potential solution: Create class intervals (group values into bins)
data <- data %>% 
  mutate(age_class = cut(age, breaks = 5)) # Creates groups of equal size

data %>% 
  count(age_class) %>%
  print(n = Inf) # display all rows

# or setting when the breaks happen yourself
data %>% 
  mutate(age_class = cut(age, breaks = c(0, 14, 19, 50, Inf))) %>%
  count(age_class) %>%
  mutate(perc = 100*n/sum(n)) # report relative frequencies as well

# or even assign "labels" to those groups
data %>% 
  mutate(age_class = cut(age, breaks = c(0, 14, 19, 50, Inf), 
                         labels = c("Children", "Youngters", "Adults", "Elderly"))) %>%
  count(age_class)
  


## Histogram (visual representation of the distribution)
data %>%   
  ggplot(mapping = aes(x = age)) +
    geom_histogram(binwidth = 5)

# Play around with the width of the bin: accuracy vs noise
  # ggplot uses the + operator (to add layers)
  # 3 key aspects: data, aesthetics and type

g1 <- data %>%   
  ggplot(aes(x = age)) +
    geom_histogram(binwidth = 1)

g2 <- data %>%   
  ggplot(aes(x = age)) +
    geom_histogram(binwidth = 10)

# put the graphs together
library(patchwork)
g1 + g2

ggsave("output/hist_age.pdf") # save plot

# Focusing on particular subsamples
data %>%   
  filter(sex == "male") %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = age), binwidth = 1)

# Or by group
data %>%   
  ggplot() +
  geom_histogram(mapping = aes(x = age), binwidth = 5) +
  facet_wrap(~ sex, nrow = 1)

### Summarise: report summary statistics

# count: n(), mean, median, min, max, sd, IQR, min, quantile(x, 0.25)...
# first, nth(x, 2), last
# !is.na(x), n_distinct(x)

# average age
data %>% 
  summarize(mean_age = mean(age, na.rm = TRUE))
  # na.rm removes missing values from the computations
  # stands for "NA remove"
  # if not included it results in NA because it cannot be computed
  # instead of yielding missing outputs (check removing that condition)

# first assign a name and then define what you want to do using functions

data %>% 
  summarize(
    count = sum(!is.na(age)),   # "!is.na" indicates "is not na" (not available=missing value)
    mean_age = mean(age, na.rm = TRUE), # the comma allows for asking for more statistics
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    p25_age = quantile(age, 0.25, na.rm = TRUE),
    p75_age = quantile(age, 0.75, na.rm = TRUE)  
  )


# Illustration:
data %>%   
  ggplot(mapping = aes(x = age)) +
    geom_histogram(colour = "grey70", alpha = 0.2) +
    geom_vline(aes(xintercept=29.6)) +
    geom_vline(aes(xintercept=27)) +
    geom_vline(aes(xintercept=9)) +  
    geom_vline(aes(xintercept=89)) +
    geom_vline(aes(xintercept=17)) +  
    geom_vline(aes(xintercept=47)) + 
    geom_segment(aes(x = 29.6-12.1, y = 1, xend=29.6+12.1, yend = 1), color = "red", linewidth = 2) + 
    annotate(x=29.6, label="Mean", y=142, colour="red", geom = "label", size = 3) +
    annotate(x=27, label="Median", y=150, colour="red", geom = "label", size = 3) +
    annotate(x=9, label="Min", y=150, colour="red", geom = "label", size = 3) +
    annotate(x=89, label="Max", y=150, colour="red", geom = "label", size = 3) +
    annotate(x=17, label="p10", y=150, colour="red", geom = "label", size = 3) +  
    annotate(x=47, label="p90", y=150, colour="red", geom = "label", size = 3) +
    annotate(x=29.6, label="Standard deviation", y=5, colour="red", geom = "label", size = 3) +
    scale_x_continuous(name = "Age", breaks = seq(0,90,10)) +
    scale_y_continuous(name = "Frequency")


# Shape of the distribution: differences

library(patchwork)
p1 <- data %>%  
  filter(age>15) %>%
  ggplot(mapping = aes(x = weight)) +
  geom_histogram()

p2 <- data %>%
  filter(age>15) %>%
  ggplot(mapping = aes(x = age)) +
  geom_histogram()

p1 + p2

# Summarise by group(s): 
data %>% 
  group_by(countryb) %>%
  summarize(
    count = sum(!is.na(age)),
    mean_age = mean(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  )

# by several groups
paisley %>% 
  group_by(sex, countryb) %>%
  summarize(
    count = sum(!is.na(age)), 
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE)
  )

# store the results in a new object
by_sex_countryb <- paisley_data %>%   
  group_by(sex, countryb) %>%
  summarize(count = sum(!is.na(age)), 
            mean_age = mean(age, na.rm = TRUE),
            median_age = median(age, na.rm = TRUE)
  )

view(by_sex_countryb)

# Export to excel (note that is appended as a differet sheet in the previous file)
library("writexl")
writexl(by_sex_countryb, path = "table2.xlsx")


### Modifying and creating numerical variables

# Inspect info on heights: feet, inches

g1 <- data %>%   
  ggplot(aes(x = feet)) +
  geom_histogram()

g2 <- data %>%   
  ggplot(aes(x = inches)) +
  geom_histogram()

# install.packages("patchwork")
library(patchwork)
g1 + g2 # put the graphs together

data %>%   
  ggplot(aes(x = age)) +
  geom_histogram()


# something is going on

data %>% 
  subset(age>100) # Identify this case

## Correct numerical value (typo)
data <- data %>%
  mutate(age = replace(age, age == 160, 16))


# check you dit it right
data %>% 
  subset(forename=="FRANCIS" & surname=="GILFILLAN", 
         select=c(forename, surname, age))




# Create nex variable (height) combining both variables (feet & inches)
data <- data %>%
  mutate(height = 30.48*feet+2.54*inches)

view(data)
# a new variable has been added (last column on the right)

# visualise new variable
data %>%  
  ggplot() +
  geom_histogram(mapping = aes(x = height), binwidth = 5)


# Group a numerical variable into different bins
data <- data %>%
  mutate(height_bins = case_when(height < 145 ~ 'low',
                                 height >=145 & height< 175 ~ 'med',
                                 height >=175 ~ 'high'))
# check
data %>% 
  group_by(height_bins) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))


### Dummy variables 

# Allow quantifying qualitative variables

data %>%
  count(literacy)

data <- data %>%
  mutate(lit_adj = recode(literacy, 
                          "superior education" = "write", 
                          "read well" = "read", 
                          "read tolerably" = "read", 
                          "read a little" = "read",
                          "read & write well" = "write", 
                          "read & write tolerably" = "write", 
                          "read & write a little" = "read", 
                          "cannot write" = "read")
  )

data %>%
  count(countryb)


data <- data %>%
  mutate(scotland = ifelse(countryb=="scotland", 1, 0)) 

data %>%
  select(countryb, scotland) 

# summary statistics
data %>% 
  summarize(
    obs = sum(!is.na(scotland)),   
    mean = mean(scotland, na.rm = TRUE), 
    sd = sd(scotland, na.rm = TRUE), 
    min = min(scotland, na.rm = TRUE),
    max = max(scotland, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 3)

  # the average of a dummy variable reflects the fraction (proportion) of 
    # observations belonging to that category 
  # it can also be interpreted as the probability of belonging to that group 
    # if an observation were selected at random.








### More on graphing with ggplot
# 3 key aspects: data, aesthetics (mapping) and type
paisley_data %>%  
  filter(age >= 18) %>%
  group_by(countryb) %>%
  summarize(mean_height = mean(height, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = countryb, y = mean_height)) +
  geom_bar()
# you can run chunks of code to see what the do so far
# and identify where the problem lies
# always consider what the unit of analysis is (when different levels)


