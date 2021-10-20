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

#######################################################################################
####                  Analysis for switching and explore* and stars                ####
####            *Note that explore is called 'non-maximizing' in the paper         ####
#######################################################################################
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
mean(switchChains[,4])# effect size estimite
quantile(switchChains[,4],probs=c(0.025,0.975)) # effect size  CI

#* reported: SI *#
# Frequentist equivalent
t.test(formula = switch ~ group, data = data_sum)
# t = -10.552, df = 26.474, p-value = 5.556e-11 95% CI  -0.7920063 -0.5339430
cohen.d(formula = switch ~ group, data = data_sum) #regular cohen's d
# -3.046179 (large); CI 3.900159 -2.192200 

############################# 'Explore' choices #############################

plot(explore ~ group, data = data_sum, main = "% 'explore' choices")


exploreBF = ttestBF(formula = explore ~ group, data = data_sum)
exploreBF ## [1] Alt., r=0.707 : 25693440 ±0% 

exploreChains= posterior(ttestBF(formula = explore ~ group, data = data_sum),iterations=1000)

mean(exploreChains[,2]) # mean difference 
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(exploreChains[,4])# effect size estimite
quantile(exploreChains[,4],probs=c(0.025,0.975)) # effect size  CI

# Frequentist equivalent
t.test(formula = explore ~ group, data = data_sum)
# t = -8.0297, df = 44.021, p-value = 3.645e-10 95% CI  -0.6125612 -0.3667637
cohen.d(formula = explore ~ group, data = data_sum) #regular cohen's d

################################ Stars Won ################################

plot(totalEarn ~ group, data = data_sum, main = "Children win fewer stars than adults")

data_sum %>% filter(group == "adult") %>% summarise(mean(totalEarn))
data_sum %>% filter(group == "child") %>% summarise(mean(totalEarn))

rewardBF = ttestBF(formula = totalEarn ~ group, data = data_sum)
rewardBF ## [1] Alt., r=0.707 : 2679334 ±0%

starChains= posterior(ttestBF(formula = totalEarn ~ group, data = data_sum),iterations=1000)

mean(starChains[,2]) # mean difference 113.5538
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI 79.84202 147.55122 

mean(starChains[,4])# effect size estimite 2.006812
quantile(starChains[,4],probs=c(0.025,0.975)) # effect size  CI 1.294476 2.714886 

# Frequentist equivalent
t.test(formula = totalEarn ~ group, data = data_sum)  #t = 7.3218, df = 39.968, p-value = 6.742e-09
#95 percent confidence interval: 85.51732 150.73268
cohen.d(formula = totalEarn ~ group, data = data_sum) #regular cohen's d
#d estimate: 2.113609 (large) 95 percent confidence interval: 1.388217 2.839001 

#########################################################################################
################  Between-group comparisons for post-test performance  ##################
#########################################################################################

########
#8-star#
########
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
# 1 adult           0.333
# 2 child           0.933

dynamic <- xtabs( ~ correct_8 + group, dataDynamic ) #* reported: paper*#
contingencyTableBF(dynamic,sampleType = "poisson") 
# Bayesian test of association Non-indep. (a=1) : 276.4305 ±0%
# in favor of a relationship between age group and answers to this question


##################################
#####      LINEAR MODELS      ####
##################################
dataDynamic$group<-as.factor(dataDynamic$group)


groupBF<-lmBF(correct_8~group, dataDynamic) #* reported: paper*# group : 100.4961 ±0%
switchBF<-lmBF(correct_8~switch, dataDynamic)  # switch: 1705.60
exploreBF<-lmBF(correct_8~explore, dataDynamic) # explore: 1233.116 ±0%

switchgroupBF<-lmBF(correct_8~switch+group, dataDynamic)
exploregroupBF<-lmBF(correct_8~explore+group, dataDynamic)
segBF<-lmBF(correct_8~switch+explore+group, dataDynamic)
allBF<-c(switchBF,exploreBF,groupBF,switchgroupBF,exploregroupBF,segBF)

allBF[1]/allBF[3] #* reported How much better switch is than group
allBF[2]/allBF[3] #* reported How much better explore is than group

# Comparing the child-only model to one that also includes switching
allBF[4]/allBF[3] #*reported switch+group vs group-only. 

# Check for relationship between age and exploration within children
dataChild<- data_sum %>% filter(group == "child")

plot(dataChild$AgeYear,dataChild$explore)
plot(dataChild$AgeYear,dataChild$switch)
switchAgeBF<-lmBF(switch~AgeYear, dataChild)
exploreAgeBF<-lmBF(explore~AgeYear, dataChild)

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

plt<-ggplot(data_sum, aes(x=switch, y=totalEarn,color=group,shape=condition))+
  scale_color_manual(values = c("#cb77ff", "#4fc9bb"))+
  theme_bw()+
  geom_point(size=4, alpha=.6) +
  #  geom_jitter()+
  ylim(200,500)


plt


##### EARNINGS ######
plt<-ggplot(data_sum, aes(x=group,y=totalEarn,fill=group))+
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

plt

# ggsave(here("plots", "exp1_Stars.png"), width = 9.15, height = 5.46)

#### EXPLORE CHOICES ####
plt<-ggplot(data_sum, aes(x=group,y=explore,fill=group))+
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


plt

# ggsave(here("plots", "exp1_Explore.png"), width = 9.15, height = 5.66)


### SWITCH CHOICES ###
plt<-ggplot(data_sum, aes(x=group,y=switch,fill=group))+
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

plt

# ggsave(here("plots", "exp1_Switch.png"), width = 9.15, height = 5.66)

plt<- ggplot(data_sum, aes(x = group, y = correct_8, fill=group)) +
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

plt

# ggsave(here("plots", "exp1_8Star.png"), width = 9.15, height = 5.66)

######################################
## Bayes factors WITHIN conditions  ##
######################################
dataDynamic <- data_sum %>% filter(condition == "dynamic")
dataStatic <- data_sum %>% filter(condition == "static")

### Switching Dynamic ###
switchBF = ttestBF(formula = switch ~ group, data = dataDynamic)
switchBF ## [1] Alt., r=0.707 : 8951340

switchChains= posterior(ttestBF(formula = switch ~ group, data = dataDynamic),iterations=1000)

mean(switchChains[,2]) # mean difference  -0.6894053
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(switchChains[,4])# effect size estimite -3.155044
quantile(switchChains[,4]) # effect size  CI
cohen.d(formula = switch ~ group, data = dataDynamic) #regular cohen's d -3.3259



### Switching Static ###
switchBF = ttestBF(formula = switch ~ group, data = dataStatic)
switchBF ## [1] Alt., r=0.707 :  384.60 0%

switchChains= posterior(ttestBF(formula = switch ~ group, data = dataStatic),iterations=1000)

mean(switchChains[,2]) # mean difference  -0.5469376
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(switchChains[,4])# effect size estimite -2.282956
quantile(switchChains[,4]) # effect size  CI
cohen.d(formula = switch ~ group, data = dataStatic) #regular cohen's d 2.614426


### Non-max Dynamic ###
exploreBF = ttestBF(formula = explore ~ group, data = dataDynamic)
exploreBF ## [1] Alt., r=0.707 : 13424560 ±0%

exploreChains= posterior(ttestBF(formula = explore ~ group, data = dataDynamic),iterations=1000)

mean(exploreChains[,2]) # mean difference  -0.5446166
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI

mean(exploreChains[,4])# effect size estimite  -3.213626
quantile(exploreChains[,4]) # effect size  CI
cohen.d(formula = explore ~ group, data = dataDynamic) #regular cohen's d -3.394606

### Non-max Static ###
exploreBF = ttestBF(formula = explore ~ group, data = dataStatic)
exploreBF ## [1] Alt., r=0.707 : 4.98958±0%

exploreChains= posterior(ttestBF(formula = explore ~ group, data = dataStatic),iterations=1000)

mean(exploreChains[,2]) # mean difference -0.3046656
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI

mean(exploreChains[,4])# effect size estimite -1.092418
quantile(exploreChains[,4]) # effect size  CI
cohen.d(formula = explore ~ group, data = dataStatic) #regular cohen's d -1.363703 


### Reward dynamic ###
rewardBF = ttestBF(formula = totalEarn ~ group, data = dataDynamic)
rewardBF ## [1] Alt., r=0.707 : 435.5568 ±0%
starChains= posterior(ttestBF(formula = totalEarn ~ group, data = dataDynamic),iterations=1000)
mean(starChains[,2]) # mean difference 106.8679
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(starChains[,4])# effect size estimite  1.572077
quantile(starChains[,4]) # effect size  CI  
cohen.d(formula = totalEarn ~ group, data = dataDynamic) #regular cohen's d   1.768504

### Reward static ###

rewardBF = ttestBF(formula = totalEarn ~ group, data = dataStatic)
rewardBF ## [1] Alt., r=0.707 : 5370.592±0%
starChains= posterior(ttestBF(formula = totalEarn ~ group, data = dataStatic),iterations=1000)
mean(starChains[,2]) # mean difference  116.3409
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(starChains[,4])# effect size estimite 3.079063
quantile(starChains[,4]) # effect size  CI  
cohen.d(formula = totalEarn ~ group, data = dataStatic) #regular cohen's d  3.393819

### Post-test all questions ###
aggregate(data = data_sum, correct~group, FUN="mean")
ttestBF(data=data_sum,formula =  correct~group)


# Check for relationship between age and exploration within children
dataChild<- data_sum %>% filter(group == "child")
plot(dataChild$AgeYear,dataChild$explore)
plot(dataChild$AgeYear,dataChild$switch)
switchAgeBF<-lmBF(switch~AgeYear, dataChild)
exploreAgeBF<-lmBF(explore~AgeYear, dataChild)


# Check if children's answers to all of the questions were signifigantly above what chance would predict (Chance being 20%)
ttestBF(dataChild$correct, mu=.2) #289493888445
