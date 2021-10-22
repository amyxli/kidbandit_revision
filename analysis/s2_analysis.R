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

#* reported: paper*#
switchBF = ttestBF(formula = switch ~ group, data = data_sum)
switchBF ## [1]Alt., r=0.707 : 7.620799e+36 ±0%

switchChains= posterior(ttestBF(formula = switch ~ group, data = data_sum),iterations=1000)

mean(switchChains[,2]) # mean difference -0.6184066
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI -0.6846195 -0.5512568 
mean(switchChains[,4])# effect size estimite -3.021579
quantile(switchChains[,4],probs=c(0.025,0.975)) # effect size  CI -3.479296 -2.565163 

############################# 'Explore' choices #############################

plot(explore ~ group, data = data_sum, main = "% 'explore' choices")

exploreBF = ttestBF(formula = explore ~ group, data = data_sum)
exploreBF ## 2.588539e+37 ±0%

exploreChains= posterior(ttestBF(formula = explore ~ group, data = data_sum),iterations=1000)

mean(exploreChains[,2]) # mean difference -0.492433
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI -0.5479371 -0.4349280 
mean(exploreChains[,4])# effect size estimite -3.033899
quantile(exploreChains[,4], probs=c(0.025,0.975)) # effect size  CI -3.508251 -2.563191 

################################ Stars Won ################################

aggregate(data = data_sum, totalEarn~group, FUN = "mean")

rewardBF = ttestBF(formula = totalEarn ~ group, data = data_sum)
rewardBF  ## [1] Alt., r=0.707 : 8.857099e+32 ±0%

starChains= posterior(ttestBF(formula = totalEarn ~ group, data = data_sum),iterations=1000)

mean(starChains[,2]) # mean difference 116.0546 
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI 101.9744 131.4045 
mean(starChains[,4])# effect size estimite 2.746324
quantile(starChains[,4],probs=c(0.025,0.975)) # effect size  CI 2.301249 3.208194 

#########################################################################################
################  Between-group comparisons for post-test performance  ##################
#########################################################################################

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
# 1 adult             0.349
# 2 child             0.771

dynamic <- xtabs( ~ correct_8 + group, dataDynamic ) #* reported: paper*#
contingencyTableBF(dynamic,sampleType = "poisson") 
# Non-indep. (a=1) : 585.2952 ±0%

###      Overall      ####

# mean prop overall correct in posttest, broken up by condition and age group
aggregate(data = data_sum, correct~ condition + group, FUN = "mean") #* reported: paper*#

#  across both conditions
subset(data_sum, group == "child")$correct %>% mean() #* reported: paper*#
subset(data_sum, group == "adult")$correct %>% mean() #* reported: paper*#

# mean for study 2, dynamic, adults, EXCLUDING 8-star question
study2post <- read_csv(here("data_tidy","study2_posttest.csv"))[-1]  

study2post_long <- study2post %>% 
  rename(correctProp = correct) %>%
  pivot_longer(
    cols = c(6:10), 
    names_to = "question", 
    names_prefix = "correct_",
    values_to = "correct"
  )

study2post_long$question <- ifelse(study2post_long$question == "1", paste0(study2post_long$question, " star"), # if "1" then "1 star"
                                   paste0(study2post_long$question, " stars")) # if not "1" then "X stars" (e.g., "8 stars")

tmp <- subset(study2post_long, question != "8 stars" & group == "adult" & condition == "dynamic") %>%
  group_by(subjID) %>%
  summarise(correctProp = mean(correct)) %>%
  ungroup() 

tmp$correctProp %>% mean()

# t-test for children vs. chance, by condition
dataChild<- data_sum %>% filter(group == "child")

ttestBF(subset(dataChild, condition=="dynamic")$correct, mu=.2) #*reported: paper* r=0.707 : 2.309175e+14 ±0%
ttestBF(subset(dataChild, condition=="static")$correct, mu=.2) # Alt., r=0.707 : 3633821279 ±0%

##################################
#####      LINEAR MODELS      ####
##################################

dataDynamic$group<-as.factor(dataDynamic$group)


groupBF<-lmBF(correct_8~group, dataDynamic) #* reported: paper*# group : 100.4961 ±0%
groupBF

switchBF<-lmBF(correct_8~switch, dataDynamic)  # reported switch: 13216616 ±0.01%
switchBF

exploreBF<-lmBF(correct_8~explore, dataDynamic) # reported explore : 1632918 ±0.01%
exploreBF

switchgroupBF<-lmBF(correct_8~switch+group, dataDynamic)
exploregroupBF<-lmBF(correct_8~explore+group, dataDynamic)
segBF<-lmBF(correct_8~switch+explore+group, dataDynamic)
allBF<-c(switchBF,exploreBF,groupBF,switchgroupBF,exploregroupBF,segBF)

allBF[1]/allBF[3] # How much better switch is than group [1] switch : 70239.28 ±0.01%
allBF[2]/allBF[3] # How much better explore is than group [1] explore : 8678.091 ±0.01%

# Comparing the child-only model to one that also includes switching
allBF[4]/allBF[3] # switch+group vs group-only. 

# Check for relationship between age and exploration within children
dataChild<- data_sum %>% filter(group == "child")

plot(dataChild$AgeYear,dataChild$explore)
plot(dataChild$AgeYear,dataChild$switch)
switchAgeBF<-lmBF(switch~AgeYear, dataChild)
switchAgeBF # AgeYear : 0.4546462 ±0%

exploreAgeBF<-lmBF(explore~AgeYear, dataChild) # [1] AgeYear : 0.3908611 ±0%

####################################################################
#####                   DATA VIZ FOR PAPER                      ####
####################################################################
labels <- c(dynamic = "Dynamic condition", static = "Static condition")

theme_custom <- theme(strip.text.x = element_text(size = 28),
                      axis.title.y = element_text(size = 28, angle = 90),
                      axis.title.x = element_text(size = 28),
                      axis.text.x = element_text(size=24),
                      axis.text.y = element_text(size=24)
)

##### EARNINGS ######
earn2<-ggplot(data_sum, aes(x=group,y=totalEarn,fill=group))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, alpha=.3)+
  geom_boxplot(alpha=.5)+
  theme_bw()+
  scale_fill_manual(values = c("#f4d221", "#e5263a"))+
  ylab("Stars won")+
  xlab(" ")+
  theme(legend.position="none")+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  facet_grid(~condition, labeller=labeller(condition = labels))+
  ylim(0,500)+
  scale_x_discrete(labels = c("Adults", "Children"))+
  theme_custom

earn2

# ggsave(here("plots","exp2_Stars.png"), width = 9.15, height = 5.66)

#### EXPLORE CHOICES ####
explore2<-ggplot(data_sum, aes(x=group,y=explore,fill=group))+
  #geom_jitter(size = 3, alpha = 0.3, width = 0.15, aes(fill=group)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, alpha=.3)+
  geom_boxplot(alpha=.5)+
  scale_x_discrete(labels = c("Adults", "Children"))+
  theme_bw()+
  scale_fill_manual(values = c("#f4d221", "#e5263a"))+
  ylab("Proportion of \n non-maximizing choices")+
  xlab(" ")+
  theme(legend.position="none")+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  facet_grid(~condition, labeller=labeller(condition = labels))+
  theme_custom +
  ylim(0,1)


explore2
# ggsave(here("plots","exp2_Explore.png"), width = 9.15, height = 5.66)

### SWITCH CHOICES ###
switch2<-ggplot(data_sum, aes(x=group,y=switch,fill=group))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, alpha=.3)+
  scale_x_discrete(labels = c("Adults", "Children"))+
  #geom_jitter(size = 3, alpha = 0.3, width = 0.15) +
  geom_boxplot(alpha=.5)+
  theme_bw()+
  scale_fill_manual(values = c("#f4d221", "#e5263a"))+
  ylab("Proportion of switch choices")+
  xlab(" ")+
  theme(legend.position="none")+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  facet_grid(~condition, labeller=labeller(condition = labels))+
  theme_custom

switch2
# ggsave(here("plots","exp2_Switch.png"), width = 9.15, height = 5.66)

#### Post test ###

eight2<- ggplot(data_sum, aes(x = group, y = correct_8, fill=group)) +
  stat_summary(fun.y=mean, geom="bar",alpha=.6, colour="black") +
  theme_bw() +
  # stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1)) +
  scale_fill_manual(values = c("#f4d221", "#e5263a"))+
  ylab("Proportion of participants \n correct about 8-star monster")+
  xlab(" ")+
  scale_x_discrete(labels = c("Adults", "Children"))+
  theme(legend.position="none") +
  facet_grid(~condition, labeller=labeller(condition = labels))+
  theme_custom +
  ylim(0,1)

eight2

# ggsave(here("plots", "exp2_8Star.png"), width = 9.15, height = 5.66)

# # Final Plot for paper #
# library(patchwork)
(switch1|switch2)/(explore1|explore2)/(earn1|earn2)/(eight1|eight2)

##########################################################
#   Within-condition analysis for the figures         ####
##########################################################

dataDynamic <- data_sum %>% filter(condition == "dynamic")
dataStatic <- data_sum %>% filter(condition == "static")

### Switching Dynamic ###
switchBF = ttestBF(formula = switch ~ group, data = dataDynamic)
switchBF ## [1] Alt., r=0.707 : 2.803773e+13 ±0%

switchChains= posterior(ttestBF(formula = switch ~ group, data = dataDynamic),iterations=1000)

mean(switchChains[,2]) # mean difference  -0.579253
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(switchChains[,4])# effect size estimite -2.341969
quantile(switchChains[,4]) # effect size  CI

### Switching Static ###
switchBF = ttestBF(formula = switch ~ group, data = dataStatic)
switchBF ## [1] Alt., r=0.707 : 1.426302e+20 ±0%

switchChains= posterior(ttestBF(formula = switch ~ group, data = dataStatic),iterations=1000)

mean(switchChains[,2]) # mean difference  -0.6092656
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(switchChains[,4])# effect size estimite -3.860834
quantile(switchChains[,4]) # effect size  CI

### Non-max Dynamic ###
exploreBF = ttestBF(formula = explore ~ group, data = dataDynamic)
exploreBF ## [1] Alt., r=0.707 : 1.067564e+14 ±0%

exploreChains= posterior(ttestBF(formula = explore ~ group, data = dataDynamic),iterations=1000)

mean(exploreChains[,2]) # mean difference  -0.4729586
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI

mean(exploreChains[,4])# effect size estimite  -2.416524
quantile(exploreChains[,4]) # effect size  CI

### Non-max Static ###
exploreBF = ttestBF(formula = explore ~ group, data = dataStatic)
exploreBF ## [1] Alt., r=0.707 : 6.424851e+19 ±0%

exploreChains= posterior(ttestBF(formula = explore ~ group, data = dataStatic),iterations=1000)

mean(exploreChains[,2]) # mean difference -0.4687977
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI

mean(exploreChains[,4])# effect size estimite-3.812591
quantile(exploreChains[,4]) # effect size  CI

### Reward dynamic ###
rewardBF = ttestBF(formula = totalEarn ~ group, data = dataDynamic)
rewardBF ## [1] Alt., r=0.707 : 1.080903e+14 ±0%
starChains= posterior(ttestBF(formula = totalEarn ~ group, data = dataDynamic),iterations=1000)
mean(starChains[,2]) # mean difference 105.374
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(starChains[,4])# effect size estimite  2.396081
quantile(starChains[,4]) # effect size  CI  

### Reward static ###

rewardBF = ttestBF(formula = totalEarn ~ group, data = dataStatic)
rewardBF ## [1] Alt., r=0.707 : 4.486329e+18 ±0%
starChains= posterior(ttestBF(formula = totalEarn ~ group, data = dataStatic),iterations=1000)
mean(starChains[,2]) # mean difference  98.46568
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(starChains[,4])# effect size estimite 141.9224
quantile(starChains[,4]) # effect size  CI  

