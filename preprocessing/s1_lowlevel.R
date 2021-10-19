#Kidbandit Data Cleaning Script
#Date: 05/13/2019
#Last edited by AXL 19/10/2021

## This script does basic low-level cleaning of raw data and outputs two separate CSV files
## corresponding to data from the first 24 adult and child participants.

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(here)

# load in study 1 data

c<- read_csv(here("data_raw","child-001-024.csv"))  
a<- read_csv(here("data_raw","adult-001-024.csv"))         

a <- a %>% 
  rename(subjID = turkcode) %>%
  select(c(subjID, 
           age,
           dob, 
           gender,
           language,
           country,
           condition, 
           trial_type, 
           trial_index,
           stimulus, 
           pointvals_firsthalf,
           button_pressed,
           earnedThis,
           earnedCumulative,
           posttest,
           responses,
           rt,
           time_elapsed)) %>%
  arrange(subjID)

c <- c %>% 
  select(-c(turkID)) %>%
  select(c(subjID, 
           age,
           dob, 
           dot,
           gender,
           language,
           condition, 
           trial_type, 
           trial_index,
           stimulus, 
           pointvals_firsthalf,
           button_pressed,
           earnedThis,
           earnedCumulative,
           posttest,
           responses,
           rt,
           time_elapsed)) %>%
  arrange(subjID)

# write.csv(a,
#           here("data_tidy","bandit-adult-001-024-lowlevel.csv"),
#           row.names=FALSE,
#           na = "")
# 
# write.csv(c,
#           here("data_tidy","bandit-child-001-024-lowlevel.csv"),
#           row.names=FALSE,
#           na = "")
