#Kidbandit Combined Additional Analyses Script: Frequentist version
### October 21 AXL

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(here)
library(BayesFactor)
library(janitor)
library(effsize)

# Load in data, add any variables ####

study1 <- read_csv(here("data_tidy","study1_trialsInfo.csv")) # data has switch/non-max choice variables      
study2 <- read_csv(here("data_tidy","study2_trialsInfo.csv"))      

study1 <- study1 %>% 
  dplyr::select(-c(rt))

study1$study <- 1
study2$study <- 2

# combine data from both studies

all_trials <- rbind(study1, study2)
all_trials$group <- as.factor(all_trials$group)
  
all_trials <- all_trials %>%
  arrange(group, subjID, trial_index) %>%
  group_by(subjID) %>%
  mutate(trial =  row_number())%>% # trial number formula for kids
  ungroup()

# add column coding if choice was the "best" one possible
all_trials <- all_trials %>%
  mutate(best = ifelse(trial < 41 & earnedThis == 6, 1,
                       ifelse(trial >= 41 & condition == "static" & earnedThis == 6 ,1,
                              ifelse(trial >= 41 & condition == "dynamic" & earnedThis == 8, 1, 0))))

# add trial bins <-  every 20 trials
bins<- 20 #How many trials per bin - 20 trials
all_trials$bin<-floor((all_trials$trial-1)/bins) # make a new column
bestByBin<-aggregate(data = all_trials, best~bin+subjID+group+condition, FUN = "mean") # prop "best choices" by bin
rewByBin<-aggregate(data = all_trials, earnedThis~bin+subjID+group+condition, FUN = "sum") # stars earned by bin

# add trial "halves"
half<- 40 #How many trials per bin
all_trials$half<-floor((all_trials$trial-1)/half)
            
#-------------------------------------#
# analysis of prop. "best" choices ####
#-------------------------------------#

# for both static and dynamic conditions, all subjects
prop_best_all <-aggregate(data = all_trials, best~half+subjID+group+condition, FUN = "mean")

# descriptives of prop best choice
aggregate(data = prop_best_all, best~group+condition+half, FUN = "mean")

# analysis of best choices, adults vs. children broken down by 1st and 2nd half ####

## t-tests to compare age groups
dy1<-prop_best_all[prop_best_all$condition=="dynamic"&prop_best_all$half==0,]
dy2<-prop_best_all[prop_best_all$condition=="dynamic"&prop_best_all$half==1,]
st1<-prop_best_all[prop_best_all$condition=="static"&prop_best_all$half==0,]
st2<-prop_best_all[prop_best_all$condition=="static"&prop_best_all$half==1,]

# dynamic, trials 1-40, t-test adult vs child in prop best 
t.test(best~group, data=dy1)
#t = 12.718, df = 85.83, p-value < 2.2e-16
#95 percent confidence interval: 0.3684283 0.5049510
cohen.d(formula = best ~ group, data = dy1) #regular cohen's d 2.518311

# dynamic, trials 40-80, t-test adult vs child in prop best 
t.test(best~group, data=dy2)
# t = -2.4407, df = 95.261, p-value = 0.01651  95% CI  -0.1618265 -0.0166563
cohen.d(formula = best ~ group, data = dy2) #regular cohen's d  -0.4547433

# static, trials 1-40
t.test(best~group, data=st1)
#t = 7.9796, df = 26.411, p-value = 1.657e-08
#95% CI 0.850000 0.440625 
cohen.d(formula = best ~ group, data = st1) #regular cohen's d  2.624509 

# static, trials 40-80
t.test(best~group, data=st2)
#t = 8.097, df = 25.684, p-value = 1.544e-08
#95% CI 0.3432617 0.5770315
cohen.d(formula = best ~ group, data = st2) #regular cohen's d  2.781302

#--------------------------------------------------------------------#
## Switching Bin analysis ("Switching behavior across trials")    ####
#--------------------------------------------------------------------#
switchByBin<-aggregate(data = all_trials, switch~bin+subjID+group+condition, FUN = "mean",na.rm=TRUE)

kidsSwitch<- switchByBin %>% filter(group == "child")
adultSwitch<- switchByBin %>% filter(group == "adult")

#* reported: paper #
mod1 <- lm(data=kidsSwitch, switch~bin)
summary(mod1) #ns

mod2 <- lm(data=adultSwitch, switch~bin) 
summary(mod2) # p<2e-16 ***
