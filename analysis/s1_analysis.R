## This script analyzes data from the first 24 adult and child participants.

## Last edited 20/10/21 AXL
## ESS added analysis on post-tests.
##### analyses and figures included in manuscript are labeled #* reported *#

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
library(BayesFactor)

data_sum = data_sum[data_sum$group %in% c("child", "adult"),]
data_sum$group = factor(data_sum$group)

################################ Switching ################################

plot(switch ~ group, data = data_sum, main = "% switching choices")

#* reported: paper*#
switchBF = ttestBF(formula = switch ~ group, data = data_sum) 
switchBF ## [1] Alt., r=0.707 : 61998272767 ±0%

switchChains= posterior(ttestBF(formula = switch ~ group, data = data_sum),iterations=1000)

mean(switchChains[,2]) # mean difference
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(switchChains[,4])# effect size estimate
quantile(switchChains[,4],probs=c(0.025,0.975)) # effect size  CI

############################# 'Explore' choices #############################

plot(explore ~ group, data = data_sum, main = "% 'explore' choices")


exploreBF = ttestBF(formula = explore ~ group, data = data_sum)
exploreBF ## [1] Alt., r=0.707 : 25693440 ±0% 

exploreChains= posterior(ttestBF(formula = explore ~ group, data = data_sum),iterations=1000)

mean(exploreChains[,2]) # mean difference 
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(exploreChains[,4])# effect size estimate
quantile(exploreChains[,4],probs=c(0.025,0.975)) # effect size  CI

################################ Stars Won ################################

data_sum %>% filter(group == "adult") %>% summarise(mean(totalEarn))
data_sum %>% filter(group == "child") %>% summarise(mean(totalEarn))

rewardBF = ttestBF(formula = totalEarn ~ group, data = data_sum)
rewardBF ## [1] Alt., r=0.707 : 2679334 ±0%

starChains= posterior(ttestBF(formula = totalEarn ~ group, data = data_sum),iterations=1000)

mean(starChains[,2]) # mean difference 113.5538
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI 79.84202 147.55122 

mean(starChains[,4])# effect size estimate 2.006812
quantile(starChains[,4],probs=c(0.025,0.975)) # effect size  CI 1.294476 2.714886 

#-------------------------------------------------------------------------------------#
############    Between-group comparisons for post-test performance  ##################
#-------------------------------------------------------------------------------------#

###      8-star      ####

# Preliminary glimpse at diff in proportion of correctly identifying 8-star option between 
# adult and child groups

# % of participants child vs adult who correctly ID'ed 8-star monster
dataDynamic <- data_sum %>% filter(condition == "dynamic")
dataStatic <- data_sum %>% filter(condition == "static")

dataDynamic %>%     #* reported: paper*#
  group_by(group) %>% 
  summarise(mean(correct_8)) %>%
  ungroup()

# group `mean(correct_8)`
# <fct>             <dbl>
# 1 adult             0.2  
# 2 child             0.933

dynamic <- xtabs( ~ correct_8 + group, dataDynamic ) #* reported: paper*#
contingencyTableBF(dynamic,sampleType = "poisson") 
# Bayesian test of association Non-indep. (a=1) : 3999.736 ±0%
# in favor of a relationship between age group and answers to this question

###      Overall      ####

# mean prop overall correct in posttest
aggregate(data = data_sum, correct~ condition + group, FUN = "mean") #* reported: paper*#

# condition group   correct
# 1   dynamic adult 0.7200000
# 2    static adult 0.8444444
# 3   dynamic child 0.8400000
# 4    static child 1.0000000

# mean for study 1, dynamic, adults, EXCLUDING 8-star question
study1post <- read_csv(here("data_tidy","study1_posttest.csv"))[-1]

study1post_long <- study1post %>% 
  rename(correctProp = correct) %>%
  pivot_longer(
    cols = c(6:10), 
    names_to = "question", 
    names_prefix = "correct_",
    values_to = "correct"
  )

study1post_long$question <- ifelse(study1post_long$question == "1", paste0(study1post_long$question, " star"), # if "1" then "1 star"
                                     paste0(study1post_long$question, " stars")) # if not "1" then "X stars" (e.g., "8 stars")

tmp <- subset(study1post_long, question != "8 stars" & group == "adult" & condition == "dynamic") %>%
  group_by(subjID) %>%
  summarise(correctProp = mean(correct)) %>%
  ungroup() 

tmp$correctProp %>% mean() # [1] 0.85 for adults in dynamic, w/o 8-star question

##################################
#####      LINEAR MODELS      ####
##################################
dataDynamic$group<-as.factor(dataDynamic$group)

groupBF<-lmBF(correct_8~group, dataDynamic) #* reported: paper*# group : 4489.353 ±0%
groupBF

switchBF<-lmBF(correct_8~switch, dataDynamic)  # switch: 462526.5 ±0.01%
switchBF

exploreBF<-lmBF(correct_8~explore, dataDynamic) # explore : 221674.8 ±0.01%
exploreBF

switchgroupBF<-lmBF(correct_8~switch+group, dataDynamic)
exploregroupBF<-lmBF(correct_8~explore+group, dataDynamic)
segBF<-lmBF(correct_8~switch+explore+group, dataDynamic)
allBF<-c(switchBF,exploreBF,groupBF,switchgroupBF,exploregroupBF,segBF)

allBF[1]/allBF[3] #* reported How much better switch is than group 103.0274 ±0.01%
allBF[2]/allBF[3] #* reported How much better explore is than group 49.37789 ±0.01%

# Comparing the child-only model to one that also includes switching
allBF[4]/allBF[3] #*reported switch+group vs group-only.  70.82535 ±1.08%

# Check for relationship between age and exploration within children
dataChild<- data_sum %>% filter(group == "child")

plot(dataChild$AgeYear,dataChild$explore)
plot(dataChild$AgeYear,dataChild$switch)
switchAgeBF<-lmBF(switch~AgeYear, dataChild)
switchAgeBF # AgeYear : 1.141046 ±0%
exploreAgeBF<-lmBF(explore~AgeYear, dataChild)
exploreAgeBF # AgeYear : 1.438597 ±0%

##################################
#####   DATA VIZ FOR PAPER    ####
##################################

labels <- c(dynamic = "Dynamic condition", static = "Static condition")

theme_custom <- theme(strip.text.x = element_text(size = 28),
                      axis.title.y = element_text(size = 28, angle = 90),
                      axis.title.x = element_text(size = 28),
                      axis.text.x = element_text(size=24),
                      axis.text.y = element_text(size=24)
                      )

##### EARNINGS ######
earn1<-ggplot(data_sum, aes(x=group,y=totalEarn,fill=group))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7, alpha=.3)+
  geom_boxplot(alpha=.5)+
  theme_bw()+
  scale_fill_manual(values = c("#cb77ff", "#4fc9bb"))+
  ylab("Stars won")+
  xlab(" ")+
  theme(legend.position="none")+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  facet_grid(~condition, labeller=labeller(condition = labels))+
  ylim(0,500)+
  scale_x_discrete(labels = c("Adults", "Children"))+
  theme_custom

earn1

# ggsave(here("plots", "exp1_Stars.png"), width = 9.15, height = 5.46)

#### EXPLORE CHOICES ####
explore1<-ggplot(data_sum, aes(x=group,y=explore,fill=group))+
  #geom_jitter(size = 3, alpha = 0.3, width = 0.15, aes(fill=group)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.7, alpha=.3)+
  geom_boxplot(alpha=.5)+
  scale_x_discrete(labels = c("Adults", "Children"))+
  theme_bw()+
  scale_fill_manual(values = c("#cb77ff", "#4fc9bb"))+
  ylab("Proportion of \n non-maximizing choices")+
  xlab(" ")+
  theme(legend.position="none")+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  facet_grid(~condition, labeller=labeller(condition = labels))+
  theme_custom


explore1

# ggsave(here("plots", "exp1_Explore.png"), width = 9.15, height = 5.66)


### SWITCH CHOICES ###
switch1<-ggplot(data_sum, aes(x=group,y=switch,fill=group))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.8, alpha=.3)+
  scale_x_discrete(labels = c("Adults", "Children"))+
  #geom_jitter(size = 3, alpha = 0.3, width = 0.15) +
  geom_boxplot(alpha=.5)+
  theme_bw()+
  scale_fill_manual(values = c("#cb77ff", "#4fc9bb"))+
  ylab("Proportion of switch choices")+
  xlab(" ")+
  theme(legend.position="none")+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  facet_grid(~condition, labeller=labeller(condition = labels))+
  theme_custom

switch1

# ggsave(here("plots", "exp1_Switch.png"), width = 9.15, height = 5.66)

eight1<- ggplot(data_sum, aes(x = group, y = correct_8, fill=group)) +
  stat_summary(fun.y=mean, geom="bar",alpha=.6, colour="black") +
  theme_bw() +
  # stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1)) +
  scale_fill_manual(values = c("#cb77ff", "#4fc9bb"))+
  ylab("Proportion of participants \n correct about 8-star monster")+
  xlab(" ")+
  scale_x_discrete(labels = c("Adults", "Children"))+
  theme(legend.position="none") +
  facet_grid(~condition, labeller=labeller(condition = labels))+
  theme_custom +
  ylim(0,1)

eight1

# ggsave(here("plots", "exp1_8Star.png"), width = 9.15, height = 5.66)

##########################################################
#   Within-condition analysis for the figures         ####
##########################################################

dataDynamic <- data_sum %>% filter(condition == "dynamic")
dataStatic <- data_sum %>% filter(condition == "static")

### Switching Dynamic ###
switchBF = ttestBF(formula = switch ~ group, data = dataDynamic)
switchBF ## [1] Alt., r=0.707 : 8951340

switchChains= posterior(ttestBF(formula = switch ~ group, data = dataDynamic),iterations=1000)

mean(switchChains[,2]) # mean difference  -0.6894053
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(switchChains[,4])# effect size estimate -3.155044
quantile(switchChains[,4]) # effect size  CI

### Switching Static ###
switchBF = ttestBF(formula = switch ~ group, data = dataStatic)
switchBF ## [1] Alt., r=0.707 : 384.6022 ±0%

switchChains= posterior(ttestBF(formula = switch ~ group, data = dataStatic),iterations=1000)

mean(switchChains[,2]) # mean difference  -0.5469376
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(switchChains[,4])# effect size estimate -2.282956
quantile(switchChains[,4]) # effect size  CI

### Non-max Dynamic ###
exploreBF = ttestBF(formula = explore ~ group, data = dataDynamic)
exploreBF ## [1] Alt., r=0.707 : 13424560 ±0%

exploreChains= posterior(ttestBF(formula = explore ~ group, data = dataDynamic),iterations=1000)

mean(exploreChains[,2]) # mean difference  -0.5446166
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI

mean(exploreChains[,4])# effect size estimate  -3.213626
quantile(exploreChains[,4]) # effect size  CI

### Non-max Static ###
exploreBF = ttestBF(formula = explore ~ group, data = dataStatic)
exploreBF ## [1] Alt., r=0.707 : 4.98958±0%

exploreChains= posterior(ttestBF(formula = explore ~ group, data = dataStatic),iterations=1000)

mean(exploreChains[,2]) # mean difference -0.3046656
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI

mean(exploreChains[,4])# effect size estimate -1.092418
quantile(exploreChains[,4]) # effect size  CI
cohen.d(formula = explore ~ group, data = dataStatic) #regular cohen's d -1.363703 


### Reward dynamic ###
rewardBF = ttestBF(formula = totalEarn ~ group, data = dataDynamic)
rewardBF ## [1] Alt., r=0.707 : 433469.1 ±0%
starChains= posterior(ttestBF(formula = totalEarn ~ group, data = dataDynamic),iterations=1000)
mean(starChains[,2]) # mean difference 112.0494
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(starChains[,4])# effect size estimate  1.572077
quantile(starChains[,4]) # effect size  CI  
cohen.d(formula = totalEarn ~ group, data = dataDynamic) #regular cohen's d   1.768504

### Reward static ###

rewardBF = ttestBF(formula = totalEarn ~ group, data = dataStatic)
rewardBF ## [1] Alt., r=0.707 : 12.04404 ±0%
starChains= posterior(ttestBF(formula = totalEarn ~ group, data = dataStatic),iterations=1000)
mean(starChains[,2]) # mean difference  98.46568
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(starChains[,4])# effect size estimate 1.262611
quantile(starChains[,4]) # effect size  CI  


