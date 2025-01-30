#### Quantification in History ####

# These scripts provide the code that help exploring the "Paisley dataset".
# They are intended as an introductory guide to how quantification works
# They can also be easily replicated

# Notice that the symbol "#" allows creating "comments" (in green) to the code


## Getting ready (preparatory commands)

# Start afresh
rm(list=ls()) # Clear de "Global Environment"

# Set working directory
getwd() # Provides the current working directory
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM_2024/session")
  # Sets the working directory(THIS IN MINE; ***CHANGE IT TO YOURS***)

  # it is sometimes difficult to type the correct directory path (the one above is mine)
  # setting it manually through the menu helps (Session/Set working directory/Choose directory)
  # copy and paste it into the script later, so you don't need to do it again

  # once the working directory is set
  # we use "relative paths" within this environment
  # vreate a folder structure that makes working on this project easy
    # data, results, etc.


### Install packages (if needed; only once)
# install.packages("tidyverse")
# install.packages("knitr")
# install.packages("xlsx")
# install.packages("writexl")

    # R works using different tools that are contained
    # in different packages
    # they need to be installed (only once)
    # and open before using (each session)

### Load the packages you need (you will need to do it every session)
library(tidyverse)
library(knitr)
library(readxl)

# Import data
data <- read_excel("data/paisley_data.xls")
  # notice that we are using a "relative path"  
  # if the data is in the working directory, you don't need the path (just the file name)
  # if the data is somewhere else (not within the project environment), you need to use the absolute path
    # i.e.-- paisley_data <- read_excel("~/FRAN/Teaching/QM Research School/Datasets/paisley_data.xls")
    # going back in the folder structure: paisley_data <- read_excel("../Datasets/paisley_data.xls")
  
  # notice the "<-" 
    # it creates a (temporary) object that is now in the "environment" we are working with
      # We could properly save it (as a separate file) if we wished 
    # you can load as many "objects" as you wish (and they can be treated separately)

# Inspecting the data
data       # A peek at the data
    # Notice that some values are missing (NA; not available; the source did not provide info)
    # It also indicates whether categorical (character) or numerical (double...)
    # Types of variables:
        # numeric: dbl (double; there are other types: integer...)
        # string: chr (character)
        # factor, ...

view(data) # The whole dataset
head(data) # the first observations
tail(data) # the last observations

tbl_vars(data) # list the variables (fields) in the dataset

## We will be using the pipe (%>%) though

data %>%  
  head(15)

## NO NEED TO LEARN THE COMMANDS BY HEART
  # copy and paste from the templates
  # you will get better with practice
  # google is always your friend (r table frequency table)
  # we now focus on make the commands work but we will later focus on interpreting the results
  # (you will be able to work on the code on your own)
  # in any case, we will only be scraping the surface (a brave new world out there)


#################### DESCRIPTIVE STATISTICS (1 variable) ###################

############## Categorical (qualitative) variables ############
# sex, literacy, employed ...
# each of these variables can exhibit certain values (categories)
  # e.g. sex: male / female

#### Frequency table (tabulate)
  # number of observations falling in each category 

  # number of males and females

data %>%  # returns a table
  count(sex)
  # the pipe (also |>) meaning "and then"
  # it takes the output of a line of code and uses it as input to the next line

data %>%  # returns a table
  count(employed)
  # notice that is also reports the number of missing values

data %>%  # returns a table
  count(occup) %>%
  print(n = 20) # display 20 rows

data %>%  # returns a table
  count(occup) %>%
  print(n = Inf) # display all rows

data %>%  
  count(occup, sort = TRUE) # present the data in order
                            # important to quickly identify the most important categories

## Absolute and relative frequencies
data %>% 
  count(occup, sort = TRUE) %>%              
  mutate(proportion = n/sum(n))  # adding relative frequency 

data %>% 
  count(occup, sort = TRUE) %>%              
  mutate(prop = n/sum(n)) %>% # fractions
  mutate(perc = 100*n/sum(n)) # percentages 

# or
data %>% 
  group_by(sex) %>%       # dimension we focus on
  summarize(n = n()) %>%  # reporting number of observations in each group
  mutate(rel_freq = n/sum(n)) # creating a new variable with the relative frequency 

  # notice that the code may look tricky but is easily replicable
  # with literacy
data %>% 
  group_by(literacy) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))

# Adding another row with the totals:
library(janitor)

data %>%  
  count(countryb, sort = TRUE) %>%              
  mutate(perc = 100*n/sum(n)) %>%
  adorn_totals("row") %>%
  as_tibble()



### Focusing on subsamples (smaller groups) 
  # filter()
data %>% 
  filter(sex == "male") %>%   
  count(literacy, sort = TRUE) %>%
  mutate(prop = n/sum(n))
  
  # "filter" restricts the analysis to those observations fulfilling that condition
  # filter() allows for complex selections using different operators:
    # >, <, ==, != (distinct)
    # Notice also that you can create more complex conditions using:
      # and: &
      # or: |
      # except: - 

data %>% 
  filter(sex=="male" & age>=18 & age<=50) %>%   
  count(literacy, sort = TRUE) %>%              
  mutate(prop = n/sum(n)) 

data %>% 
  filter(countryb=="ireland" | countryb=="scotland") %>%
  filter(sex=="male" & age>=18 & age<=50) %>%   
  count(literacy, sort = TRUE) %>%              
  mutate(prop = n/sum(n)) %>%
  mutate(prop = round(prop, 2)) # reporting only up two decimals

# This allows paying attention to what it is both common and rare
data %>% 
  filter(countryb=="ireland" | countryb=="scotland") %>%
  filter(sex=="male" & age>=18 & age<=50) %>%   
  filter(literacy=="superior education") %>%
  select(year, forename, surname, age, born, countryb, reside, occup)


### Create an output that you can use later
  # the output is not saved unless it is assigned to an object
  # which can then be exported (saved) into different formats later

table1 <- data %>% 
  filter(sex == "male") %>%   
  group_by(literacy) %>%      
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) 

# Export the output (object) into excel 
library(writexl)

write_xlsx(occ, "output/table1.xlsx")
  # notice that we are saving this "object" as an excel file in the folder "output"



###### Plotting frequencies (graph bar)
  # ggplot
  # use "+" to add features to the graph

# sex
ggplot(data = data) +       
  geom_bar(aes(x = sex))
    # "aes" goes for aesthetics

  # or
data %>% 
  ggplot() + 
    geom_bar(aes(x = sex))

# literacy
data %>% 
  ggplot() + 
    geom_bar(aes(x = literacy))

# horizontal to facilitate reading the categories
data %>% 
  ggplot() + 
    geom_bar(aes(x = literacy)) +
    coord_flip()

# narrowing down the analysis  
data %>% 
  filter(countryb == "scotland" | countryb == "ireland") %>%
  filter(sex == "male" & age>18) %>%
  ggplot() + 
    geom_bar(mapping = aes(x = literacy)) +
    coord_flip()


# proportions (instead of counts)
data %>% 
  count(literacy, sort = TRUE) %>%              
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = literacy, y = prop)) + 
    geom_col() + # instead of geom_bar()
    coord_flip()
  
    # geom_col() is a more general way of depicting graph bars
    # we explicitly indicate what we want to depict (prop in this case)

# editing the graph
g1 <- data %>% 
  count(literacy, sort = TRUE) %>%              
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = literacy, y = prop)) + 
    geom_col() + 
    coord_flip() + 
    ylab("Proportion") + 
    xlab("Literacy")
  # or all together: 
  # labs(title ="", subtitle = "", x = "", etc etc)
  # many options for editing graphs... 

g1  
# save the graph
g1 %>%
  ggsave("output/lit.png", dpi = 320)

?ggsave
?ggplot


### Re-categorising qualitative information 
# typos, aggregating categories, etc.

data %>%  
  count(occup) %>%              
  mutate(prop = n/sum(n)) 

# recoding
data <- data %>%
  mutate(occup_adj = recode(occup, 
                            "blacksmith" = "black smith", 
                            "block print" = "block printer")
         )

View(data)

data %>% 
  filter(occup_adj=="black smith" | occup_adj=="block printer") %>%
  select(occup, occup_adj)

data %>%  
  count(literacy)

  # checking what we have done

# another example with literacy: iliterate, read, write
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
    # no need to recode "illiterate" because it is the same category

data %>% 
  subset(select=c(forename, surname, literacy, lit_adj))

data %>%
  count(lit_adj)

# removing leading and trailing spaces
data <- data %>% 
  mutate(occup_adj = str_trim(occup_adj))

# ranking "qualitative" information
data %>%  
  count(literacy) %>%              
  mutate(prop = n/sum(n)) 
  # listed in alphabetical order (not always informative enough)


# factor variables: ranking categories
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
  count(lit_rank)
  
  # or visually:
data %>% 
  filter(!is.na(lit_rank)) %>% # exclude missing values
  ggplot() +
    geom_bar(aes(x = lit_rank)) +
    coord_flip()  

# re-categorising more complicate patterns

data %>%
  count(marks)

# install.packages("stringr")
library(stringr)

data <- data %>%
  mutate(marks_adj = case_when(
    str_detect(marks, "scar") ~ "scar",
    str_detect(marks, "cut") ~ "cut", 
    str_detect(marks, "burn") ~ "burn",  
    str_detect(marks, "blind") ~ "blind",      
    str_detect(marks, "no mark") ~ "none",
    str_detect(marks, "nomark") ~ "none",    
    str_detect(marks, "mark") ~ "mark",
    is.na(marks) ~ NA,
    TRUE ~ "other"))

# checking what you have done
data %>%
  select(marks, marks_adj)

data %>% 
  count(marks_adj)

data %>% 
  filter(marks_adj=="cut") %>% 
  count(marks) %>% 
  print(n = Inf)


# Regular expressions


##### Save the (modified) data ####
# The data has somewhat change: correct typos, add new variables...
# We can save the new dataset as a R file, so we can come back to that later

write_rds(data, "paisley_v2.rds")



data2 <- read_rds("paisley_v2.rds")