## This script analyzes data from the first 24 adult and child participants.

## Last edited 20/10/21 AXL
## Frequentist analysis

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(ggpubr)
library(here)
library(effsize)

data_sum <- read_csv(here("data_tidy", "study1_data_sum.csv"))      
age_info<-read_csv(here("data_tidy","study1_ageinfo.csv"))
data_sum<-merge(data_sum,age_info, by.x="subjID")

#-------------------------------------------------------------------------------------#
##                    Analysis for switching and explore* and stars                ####
##              *Note that explore is called 'non-maximizing' in the paper           ##
#-------------------------------------------------------------------------------------#

data_sum = data_sum[data_sum$group %in% c("child", "adult"),]
data_sum$group = factor(data_sum$group)

################################ Switching ################################

plot(switch ~ group, data = data_sum, main = "% switching choices")

#* reported: SI *#
# Frequentist equivalent
t.test(formula = switch ~ group, data = data_sum)
# t = -10.552, df = 26.474, p-value = 5.556e-11 95% CI  -0.7920063 -0.5339430
cohen.d(formula = switch ~ group, data = data_sum) #regular cohen's d
# -3.046179 (large); CI 3.900159 -2.192200 

############################# 'Explore' choices #############################

plot(explore ~ group, data = data_sum, main = "% 'explore' choices")

# Frequentist equivalent
t.test(formula = explore ~ group, data = data_sum)
# t = -8.0297, df = 44.021, p-value = 3.645e-10 95% CI  -0.6125612 -0.3667637
cohen.d(formula = explore ~ group, data = data_sum) #regular cohen's d

################################ Stars Won ################################

# Frequentist equivalent
t.test(formula = totalEarn ~ group, data = data_sum)  #t = 7.3218, df = 39.968, p-value = 6.742e-09
#95 percent confidence interval: 85.51732 150.73268
cohen.d(formula = totalEarn ~ group, data = data_sum) #regular cohen's d
#d estimate: 2.113609 (large) 95 percent confidence interval: 1.388217 2.839001 

#-------------------------------------------------------------------------------------#
############    Between-group comparisons for post-test performance  ##################
#-------------------------------------------------------------------------------------#

###      8-star      ####

# Preliminary glimpse at diff in proportion of correctly identifying 8-star option between 
# adult and child groups

# % of participants child vs adult who correctly ID'ed 8-star monster
dataDynamic <- data_sum %>% filter(condition == "dynamic")

dynamic <- xtabs( ~ correct_8 + group, dataDynamic ) 

chisq.test(dynamic)
# X-squared = 13.575, df = 1, p-value = 0.0002293

###      Overall      ####

# t-test for children vs. chance, by condition
dataChild<- data_sum %>% filter(group == "child")

t.test(subset(dataChild, condition=="dynamic")$correct, mu=.2) 
#t = 10.267, df = 14, p-value = 6.733e-08, CI  0.7063037 0.9736963

t.test(subset(dataChild, condition=="static")$correct, mu=.2) 
# data constant since everyone got 100%

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
anova(mod1, mod2, test = "LRT") # LR = 19.587  
# how much better is explore vs. group
anova(mod1, mod3, test = "LRT") # LR = 14.551 

# Comparing the child-only model to one that also includes switching
anova(mod1, mod4, test = "LRT") #switch+group vs group-only.  2.26e-06 ***
anova(mod1, mod5, test = "LRT") #explore+group vs group-only. 0.0001135 ***

# Check for relationship between age and exploration within children
dataChild<- data_sum %>% filter(group == "child")

plot(dataChild$AgeYear,dataChild$explore)
plot(dataChild$AgeYear,dataChild$switch)
switchAge<-lm(switch~AgeYear, dataChild)
summary(switchAge) # ns 

exploreAge<-lm(explore~AgeYear, dataChild)
summary(exploreAge)

