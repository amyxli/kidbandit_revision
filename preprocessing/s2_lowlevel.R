#Kidbandit Data Cleaning Script
#Date: 07/27/2019
#Last edited by ESS

## This script does basic low-level cleaning of raw data and outputs two separate CSV files
## corresponding to data from the replication adult and child participants.

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(here)

c<-read_csv(here("data_raw","child-raw-replication.csv"))
a<-read_csv(here("data_raw","adult-raw-replication.csv"))


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
 #          rt,
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
           #rt,
           time_elapsed)) %>%
  arrange(subjID)

#write.csv(a,
#          here("data_tidy","bandit-adult-rep-lowlevel.csv"),
#          row.names=FALSE,
#          na = "")

#write.csv(c,
#          here("data_tidy","bandit-child-rep-lowlevel.csv"),
#          row.names=FALSE,
#          na = "")
 