## Last edited 19/10/21 AXL

## This script analyzes data from replication adult and child participants.
## ESS added analysis on post-tests 1/24/2020.

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(ggpubr)
library(here)
library(effsize)

data_sum <- read_csv(here("data_tidy", "study2_data_sum.csv"))      
age_info<-read_csv(here("data_tidy","study2_ageinfo.csv"))
data_sum<-merge(data_sum,age_info, by.x="subjID")

#-------------------------------------------------------------------------------------#
##                    Analysis for switching and explore* and stars                ####
##              *Note that explore is called 'non-maximizing' in the paper           ##
#-------------------------------------------------------------------------------------#
library(BayesFactor)

data_sum = data_sum[data_sum$group %in% c("child", "adult"),]
data_sum$group = factor(data_sum$group)

################################ Switching ################################

plot(switch ~ group, data = data_sum, main = "% switching choices")

# Frequentist equivalent
t.test(formula = switch ~ group, data = data_sum)
#t = -13.468, df = 56.477, p-value < 2.2e-16; 95% CI -0.7154756 -0.5302261
cohen.d(formula = switch ~ group, data = data_sum) #regular cohen's d
# -3.043196; CI -3.513635 -2.572757 

############################# 'Explore' choices #############################

plot(explore ~ group, data = data_sum, main = "% 'explore' choices")

t.test(formula = explore ~ group, data = data_sum)
#t = -13.518, df = 55.945, p-value < 2.2e-16; 95% CI -0.5698783 -0.4227744
cohen.d(formula = explore ~ group, data = data_sum) #regular cohen's d


################################ Stars Won ################################

aggregate(data = data_sum, totalEarn~group, FUN = "mean")

t.test(formula = totalEarn ~ group, data = data_sum) #t = 11.925, df = 54.2, p-value < 2.2e-16
# 95% CI   97.27606 136.59177

cohen.d(formula = totalEarn ~ group, data = data_sum) #regular cohen's d
#d estimate: 2.78887 (large) 95 percent confidence interval: 2.337442 3.240299 

#-------------------------------------------------------------------------------------#
############    Between-group comparisons for post-test performance  ##################
#-------------------------------------------------------------------------------------#

###      8-star      ####

# Preliminary glimpse at diff in proportion of correctly identifying 8-star option between 
# adult and child groups

# % of participants child vs adult who correctly ID'ed 8-star monster
dataDynamic <- data_sum %>% filter(condition == "dynamic")

dataDynamic %>%     #* reported: paper*#
  group_by(group) %>% 
  summarise(mean(correct_8)) %>%
  ungroup()

# group `mean(correct_8)`
# <fct>             <dbl>
# 1 adult             0.349
# 2 child             0.771

dynamic <- xtabs( ~ correct_8 + group, dataDynamic ) 
chisq.test(dynamic) # X-squared = 12.217, df = 1, p-value = 0.0004736

###      Overall      ####

# mean prop overall correct in posttest, broken up by condition and age group
aggregate(data = data_sum, correct~ condition + group, FUN = "mean") #* reported: paper*#

#  across both conditions, children only
subset(data_sum, group == "child")$correct %>% mean() #* reported: paper*#

# t-test for children vs. chance, by condition
dataChild<- data_sum %>% filter(group == "child")

t.test(subset(dataChild, condition=="dynamic")$correct, mu=.2) 
# t = 15.963, df = 34, p-value < 2.2e-16; 95% CI  0.7535344 0.9150370; mu = 0.8342857 

t.test(subset(dataChild, condition=="static")$correct, mu=.2) 
# t = 23.008, df = 14, p-value = 1.597e-12; 95% CI 0.8649731 1.0016936; mu = 0.9333333 

##################################
#####      LINEAR MODELS      ####
##################################

dataDynamic$group<-as.factor(dataDynamic$group)

mod1<-glm(correct_8~group, dataDynamic, family= "binomial") 
summary(mod1)
mod2<-glm(correct_8~switch, dataDynamic, family= "binomial") 
summary(mod2)
mod3<-glm(correct_8~explore, dataDynamic, family= "binomial")
summary(mod3)

mod4<-glm(correct_8~switch+group, dataDynamic, family= "binomial")
mod5<-glm(correct_8~explore+group, dataDynamic, family= "binomial")
mod6<-glm(correct_8~switch+explore+group, dataDynamic, family= "binomial")

# how much better is switch vs. group
anova(mod1, mod3)
# how much better is explore vs. group
anova(mod2, mod3)

# Comparing the child-only model to one that also includes switching
anova(mod4, mod1, test = "LRT") #switch+group vs group-only. 
# 2.14e-06 ***

# Check for relationship between age and exploration within children
dataChild<- data_sum %>% filter(group == "child")

plot(dataChild$AgeYear,dataChild$explore)
plot(dataChild$AgeYear,dataChild$switch)
switchAge<-lm(switch~AgeYear, dataChild)
summary(switchAge) # nonsig

exploreAge<-lm(explore~AgeYear, dataChild)
summary(exploreAge) # nonsig
