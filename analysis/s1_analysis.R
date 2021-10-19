## This script analyzes data from the first 24 adult and child participants.

## Last edited 19/10/21 AXL
## ESS added analysis on post-tests.

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

switchBF = ttestBF(formula = switch ~ group, data = data_sum)
switchBF ## [1] Alt., r=0.707 : 61998272767 ±0%

switchChains= posterior(ttestBF(formula = switch ~ group, data = data_sum),iterations=1000)

mean(switchChains[,2]) # mean difference
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(switchChains[,4])# effect size estimite
quantile(switchChains[,4],probs=c(0.025,0.975)) # effect size  CI
cohen.d(formula = switch ~ group, data = data_sum) #regular cohen's d

# Let us test normality assumption
data_sum %>%
  filter(group == "adult") %>%
  pull(switch) %>%
  shapiro.test()

data_sum %>%
  filter(group == "child") %>%
  pull(switch) %>%
  shapiro.test()

switchDensity <- ggdensity(data_sum, 
                           x = "switch",
                           fill = "lightgray", 
                           add = "mean",
                           rug = TRUE,
                           color = "group")

# Given the way the data looks, let's try the Bayesian bootstrap

library(bayesboot)

switch_c <- data_sum %>%
  filter(group == "child") %>%
  pull(switch)

switch_a <- data_sum %>%
  filter(group == "adult") %>%
  pull(switch)

# Running the Bayesian bootstrap for both datasets
b_switch_c <- bayesboot(switch_c, weighted.mean, use.weights = TRUE)
b_switch_a <- bayesboot(switch_a, weighted.mean, use.weights = TRUE)

# Calculating the posterior difference and converting back to a bayesboot object for plotting
b_switch_diff <- as.bayesboot(b_switch_c - b_switch_a)
plot(b_switch_diff)

# Was zero a value for any of the 4000 BB replications?
sum(b_switch_diff <= 0)

############################# 'Explore' choices #############################

plot(explore ~ group, data = data_sum, main = "% 'explore' choices")


exploreBF = ttestBF(formula = explore ~ group, data = data_sum)
exploreBF ## [1] Alt., r=0.707 : 25693440 ±0%

exploreChains= posterior(ttestBF(formula = explore ~ group, data = data_sum),iterations=1000)

mean(exploreChains[,2]) # mean difference
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(exploreChains[,4])# effect size estimite
quantile(exploreChains[,4],probs=c(0.025,0.975)) # effect size  CI
cohen.d(formula = explore ~ group, data = data_sum) #regular cohen's d

# Let us test normality assumption
data_sum %>%
  filter(group == "adult") %>%
  pull(explore) %>%
  shapiro.test()

data_sum %>%
  filter(group == "child") %>%
  pull(explore) %>%
  shapiro.test()

exploreDensity <- ggdensity(data_sum, 
                            x = "explore",
                            fill = "lightgray", 
                            add = "mean",
                            rug = TRUE,
                            color = "group")

# Switch to a more robust analysis procedure - let's try the Bayesian bootstrap

explore_c <- data_sum %>%
  filter(group == "child") %>%
  pull(explore)

explore_a <- data_sum %>%
  filter(group == "adult") %>%
  pull(explore)

# Running the Bayesian bootstrap for both datasets
b_explore_c <- bayesboot(explore_c, weighted.mean, use.weights = TRUE)

b_explore_a <- bayesboot(explore_a, weighted.mean, use.weights = TRUE)

# Calculating the posterior difference and converting back to a bayesboot object for plotting
b_explore_diff <- as.bayesboot(b_explore_c - b_explore_a)
plot(b_explore_diff)

# Was zero a value for any of the 4000 BB replications?
sum(b_explore_diff <= 0) 



################################ Stars Won ################################

plot(totalEarn ~ group, data = data_sum, main = "Children win fewer stars than adults")

data_sum %>% filter(group == "adult") %>% summarise(mean(totalEarn))
data_sum %>% filter(group == "child") %>% summarise(mean(totalEarn))

rewardBF = ttestBF(formula = totalEarn ~ group, data = data_sum)
rewardBF ## [1] Alt., r=0.707 : 2679334 ±0%

starChains= posterior(ttestBF(formula = totalEarn ~ group, data = data_sum),iterations=1000)

mean(starChains[,2]) # mean difference
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI

mean(starChains[,4])# effect size estimite
quantile(starChains[,4],probs=c(0.025,0.975)) # effect size  CI
cohen.d(formula = totalEarn ~ group, data = data_sum) #regular cohen's d

# Switch to a more robust analysis procedure - let's try the Bayesian bootstrap

stars_c <- data_sum %>%
  filter(group == "child") %>%
  pull(totalEarn)

stars_a <- data_sum %>%
  filter(group == "adult") %>%
  pull(totalEarn)

# Running the Bayesian bootstrap for both datasets
b_stars_c <- bayesboot(stars_c, weighted.mean, use.weights = TRUE)

b_stars_a <- bayesboot(stars_a, weighted.mean, use.weights = TRUE)

# Calculating the posterior difference and converting back to a bayesboot object for plotting
b_stars_diff <- as.bayesboot(b_stars_a - b_stars_c)
plot(b_stars_diff)

# Was zero a value for any of the 4000 BB replications?
sum(b_stars_diff <= 0) #Output is 0





#########################################################################################
################  Between-group comparisons for post-test performance  ##################
#########################################################################################

########
#8-star#
########
# Preliminary glimpse at diff in proportion of correctly identifying 8-star option between 
# adult and child groups

data_sum %>% 
  filter(condition=="dynamic") %>% 
  group_by(group) %>% 
  summarise(mean(correct_8))

dataDynamic <- data_sum %>% filter(condition == "dynamic")
dataDynamic$group <- factor(dataDynamic$group)
glm.fit <- glm(correct_8 ~ group, data = dataDynamic, family= "binomial")
summary(glm.fit)
##### NEED ADDITIONAL PREDICTORS

# use the confint function to obtain confidence intervals for the coefficient estimate
# CIs using profiled log-likelihood
confint(glm.fit) # does not contain 0

#  test for an overall effect of rank using the wald.test function 
library(aod)
wald.test(b = coef(glm.fit), Sigma = vcov(glm.fit), Terms=1:1)

#########
#OVERALL#
#########

library(ordinal)

data_sum$correct <- as.factor(data_sum$correct)

levels(data_sum$correct) # check factor levels

fit.switch <- clm2(correct~switch,data=data_sum)
fit.group <- clm2(correct~group,data=data_sum)
fit.group_switch <- clm2(correct~group+switch,data=data_sum)

summary(fit.switch)
summary(fit.group) #p = 0.0339430
summary(fit.group_switch)

######### CHECK INTERPRETATION OF OUTPUT. And check which models to include - is having 
# just the group predictor enough?

confint(fit.group, level=0.95, type = c("profile"))

## BACKWARD AIC PROCEDURE
library(MASS)

stepAIC(clm(correct~group+switch+explore, data=data_sum), scope=~group+switch+explore, direction="backward")

## best predictor of correct overall seems to be 'explore' (i.e., non-maximising choices)?

#######################################################################################
####                           Time-to-event analysis                              ####
#######################################################################################

library(survival)
library(survminer)

################  Visualise the data using KM curves ################ 

fit <- survfit(
  Surv(time, status) ~ group, 
  data = data_sum)
fit

## Plotting survival 
groupKM <-  ggsurvplot(fit, 
                       data = data_sum, 
                       censor.shape = "|", 
                       censor.size = 3,
                       conf.int = TRUE,
                       legend.labs = 
                         c("adult", "child"),
                       font.x = c(12),
                       font.y = c(12),
                       font.tickslab = c(10)
) +
  ylab("probability of not discovering change")

#####################   Cox Regression Analysis #####################  

# First list the models.

## 1. Are children better at detecting change than adults?

discovery.cox.1 <- 
  coxph(
    Surv(time, status) ~ group, 
    data = data_sum
  )
summary(discovery.cox.1) 
# Child group sig lower time-to-event, wald statistic=7.4e-5

## 2. Does a higher proportion of 'explore' choices lead to better change detection?

discovery.cox.2 <- 
  coxph(
    Surv(time, status) ~ explore, 
    data = data_sum
  )
summary(discovery.cox.2)

## 3. Does a higher proportion of switching lead to better change detection?

discovery.cox.3 <- 
  coxph(
    Surv(time, status) ~ switch, 
    data = data_sum
  )
summary(discovery.cox.3)

## 4. Group + 'explore' choices as covariates. 

discovery.cox.4 <- 
  coxph(
    Surv(time, status) ~ group + explore, 
    data = data_sum
  )
summary(discovery.cox.4)

## 5. Group + switching as covariates.

discovery.cox.5 <- 
  coxph(
    Surv(time, status) ~ group + switch, 
    data = data_sum
  )
summary(discovery.cox.5)

## 6. Group, switching, and 'explore' choices as covariates.

discovery.cox.6 <- 
  coxph(
    Surv(time, status) ~ group + explore + switch, 
    data = data_sum
  )
summary(discovery.cox.6) ## switch is best predictor

## 7. Is there a difference in relationship between detection of change and exploration for adults versus children?

##  a) When looking at 'explore' choices...

discovery.cox.7a <- 
  coxph(
    Surv(time, status) ~ explore + group + explore:group, 
    data = data_sum
  )
summary(discovery.cox.7a) 

## b) When looking at switching...

discovery.cox.7b <- 
  coxph(
    Surv(time, status) ~ switch + group + switch:group, 
    data = data_sum
  )
summary(discovery.cox.7b)

# We then test assumptions of Cox regression.

## Testing proportional hazards assumption
cox.zph(discovery.cox.1)
cox.zph(discovery.cox.2)
cox.zph(discovery.cox.3)
cox.zph(discovery.cox.4)
cox.zph(discovery.cox.5)
cox.zph(discovery.cox.6)
cox.zph(discovery.cox.7a)
cox.zph(discovery.cox.7b)

## Example plot
ggcoxzph(cox.zph(discovery.cox.1)) # Assumptions obeyed.


########### Exploratory correlations  #################
# 1) Is there a negative relationship between exploration and number of stars won?
cor.test(data_sum$explore,data_sum$totalEarn)          #For Explore,  yes there is: -0.619
correlationBF(data_sum$explore,data_sum$totalEarn)     #r=0.333 : 6380.822 ±0%


cor.test(data_sum$switch,data_sum$totalEarn)           #For Switch,  yes there is: -0.672
correlationBF(data_sum$switch,data_sum$totalEarn)      #Alt., r=0.333 : 69012.02 ±0%


# 2) Is there a positive relationship between exploration & post-test questions?
cor.test(data_sum$explore,data_sum$correct)           #For Explore, 0.319435, moderate correlation 
correlationBF(data_sum$explore,data_sum$correct)      # Alt., r=0.333 : 2.925557 ±0%


cor.test(data_sum$switch,data_sum$correct)           #For switch prop, 0.2828351 , moderate correlation 
correlationBF(data_sum$switch,data_sum$correct)      #Alt r=0.333 : 1.782831 ±0%

# 3) Is there a negative relationship between age & exploration 
cor.test(data_sum$switch,as.numeric(data_sum$AgeYear))         # -0.7983154 
correlationBF(data_sum$switch,as.numeric(data_sum$AgeYear))    #  287385195 ±0%

cor.test(data_sum$explore,as.numeric(data_sum$AgeYear))        #-0.7266096 
correlationBF(data_sum$explore,as.numeric(data_sum$group))    # 17644869 ±0%


# 4) Is there a negative relationship between the number of stars won and performance on post-test questions?
cor.test(data_sum$totalEarn,data_sum$correct_8)     #-0.2255333       
correlationBF(data_sum$totalEarn,data_sum$correct_8)   # 0.9429884 ±0%

#5) Is there are a negative relationship between age and performance on post-test?
cor.test(data_sum$AgeYear,data_sum$correct)           #-0.2604015
correlationBF(data_sum$AgeYear,data_sum$correct)      #  1.362568 ±0%

#6) What about the 8 star monster question in the dynamic version?
cor.test(dataDynamic$AgeYear,dataDynamic$correct_8)      #-0.706895 
correlationBF(dataDynamic$AgeYear,dataDynamic$correct_8) #Alt., r=0.333 : 1189.322 ±0%




##################
#### Post test ###
##################
dataDynamic <- data_sum %>% filter(condition == "dynamic")
glm.fit <- glm(correct_8 ~ group, data = dataDynamic, family= "binomial")


summary(glm.fit)
dataStatic <- data_sum %>% filter(condition == "static")


static <- xtabs( ~ correct_8 + group, dataStatic )
zero<-c(0,0) #Because no child or kid got this question in correctly, we need a new row of zeros
static<-rbind(static,zero)


dynamic <- xtabs( ~ correct_8 + group, dataDynamic )
contingencyTableBF(dynamic,sampleType = "poisson")
contingencyTableBF(static,sampleType = "poisson")


##################################
#####      LINEAR MODELS      ####
##################################
dataDynamic$group<-as.factor(dataDynamic$group)

switchBF<-lmBF(correct_8~switch, dataDynamic)
exploreBF<-lmBF(correct_8~explore, dataDynamic)
groupBF<-lmBF(correct_8~group, dataDynamic)
switchgroupBF<-lmBF(correct_8~switch+group, dataDynamic)
exploregroupBF<-lmBF(correct_8~explore+group, dataDynamic)
segBF<-lmBF(correct_8~switch+explore+group, dataDynamic)
allBF<-c(switchBF,exploreBF,groupBF,switchgroupBF,exploregroupBF,segBF)

allBF[1]/allBF[3] #How much better switch is than group
allBF[2]/allBF[3] #How much better explore is than group

allBF
allBF[1]/allBF[4]


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
  theme(strip.text.x = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24)
  )

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
  theme(strip.text.x = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24)
  )


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
  theme(strip.text.x = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24)
  )
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
  theme(strip.text.x = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24)
  )+
  ylim(0,1)

plt

eight1<-plt
#ggsave("exp1_8Star.png", width = 9.15, height = 5.46)




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
