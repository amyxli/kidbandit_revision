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
switchBF ## [1]Alt., r=0.707 : 7.620799e+36 ±0%

switchChains= posterior(ttestBF(formula = switch ~ group, data = data_sum),iterations=1000)

mean(switchChains[,2]) # mean difference -0.6184066
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI -0.6846195 -0.5512568 
mean(switchChains[,4])# effect size estimite -3.021579
quantile(switchChains[,4],probs=c(0.025,0.975)) # effect size  CI -3.479296 -2.565163 

# Frequentist equivalent
t.test(formula = switch ~ group, data = data_sum)
#t = -13.468, df = 56.477, p-value < 2.2e-16; 95% CI -0.7154756 -0.5302261
cohen.d(formula = switch ~ group, data = data_sum) #regular cohen's d
# -3.043196; CI -3.513635 -2.572757 

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
exploreBF ## 2.588539e+37 ±0%

exploreChains= posterior(ttestBF(formula = explore ~ group, data = data_sum),iterations=1000)

mean(exploreChains[,2]) # mean difference -0.492433
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI -0.5479371 -0.4349280 
mean(exploreChains[,4])# effect size estimite -3.033899
quantile(exploreChains[,4], probs=c(0.025,0.975)) # effect size  CI -3.508251 -2.563191 

t.test(formula = explore ~ group, data = data_sum)
#t = -13.518, df = 55.945, p-value < 2.2e-16; 95% CI -0.5698783 -0.4227744
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



################################ reward ################################

plot(totalEarn ~ group, data = data_sum, main = "Children win fewer stars than adults")
aggregate(data = data_sum, totalEarn~group, FUN = "mean")

rewardBF = ttestBF(formula = totalEarn ~ group, data = data_sum)
rewardBF  ## [1] Alt., r=0.707 : 8.857099e+32 ±0%

starChains= posterior(ttestBF(formula = totalEarn ~ group, data = data_sum),iterations=1000)

mean(starChains[,2]) # mean difference 116.0546 
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI 101.9744 131.4045 
mean(starChains[,4])# effect size estimite 2.746324
quantile(starChains[,4],probs=c(0.025,0.975)) # effect size  CI 2.301249 3.208194 

# Frequentist equivalent
t.test(formula = totalEarn ~ group, data = data_sum) #t = 11.925, df = 54.2, p-value < 2.2e-16
# 95% CI   97.27606 136.59177

cohen.d(formula = totalEarn ~ group, data = data_sum) #regular cohen's d
#d estimate: 2.78887 (large) 95 percent confidence interval: 2.337442 3.240299 


# Switch to a more robust analysis procedure - let's try the Bayesian bootstrap

stars_c <- data_sum %>%
  filter(group == "child") %>%
  pull(totalEarn)

mean(stars_c)

stars_a <- data_sum %>%
  filter(group == "adult") %>%
  pull(totalEarn)

mean(stars_a)

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

# Preliminary glimpse at diff in proportion of correctly identifying 8-star option between 
# adult and child groups

data_sum %>% 
  filter(condition=="dynamic") %>% 
  group_by(group) %>% 
  summarise(mean(correct_8))

dataDynamic <- data_sum %>% filter(condition == "dynamic")
dataDynamic$group <- factor(dataDynamic$group)

subset(dataDynamic, group == "adult") %>% summarise(correct_8 = mean(correct_8))
subset(dataDynamic, group == "child") %>% summarise(correct_8 = mean(correct_8))

# For all questions including 8-star
dataStatic <- data_sum %>% filter(condition == "static")

dataStatic$group <- factor(dataStatic$group)
dataStatic$correct <- as.numeric(as.character(dataStatic$correct))

subset(dataStatic, group == "adult") %>% summarise(correct = mean(correct))
subset(dataStatic, group == "child") %>% summarise(correct = mean(correct))

glm.fit <- glm(correct_8 ~ group, data = dataDynamic, family= "binomial")
summary(glm.fit)

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

fit.group <- clm2(correct~group,data=data_sum)
summary(fit.group) 

## BACKWARD AIC PROCEDURE
library(MASS)

stepAIC(clm(correct~group+switch+explore, data=data_sum), scope=~group+switch+explore, direction="backward")

## best predictor of correct overall seems to be 'switch' 

#######################################################################################
####                           Time-to-event analysis                              ####
#######################################################################################

#install.packages("survival")
#install.packages("survminer")
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
# Child group sig lower time-to-event, wald statistic=8.661e-07

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
cor.test(data_sum$explore,data_sum$totalEarn)          #For Explore, -0.6139454 
correlationBF(data_sum$explore,data_sum$totalEarn)     # Alt., r=0.333 : 2.56535e+15 ±0%


cor.test(data_sum$switch,data_sum$totalEarn)           #For Switch,   -0.610618 
correlationBF(data_sum$switch,data_sum$totalEarn)      #[1] Alt., r=0.333 : 1.536948e+15


# 2) Is there a positive relationship between exploration & post-test questions?
cor.test(data_sum$explore,data_sum$correct)           #For Explore, 0.1507385 
correlationBF(data_sum$explore,data_sum$correct)      # Alt., r=0.333 : 1.1002 ±0%


cor.test(data_sum$switch,data_sum$correct)           # 0.1587075
correlationBF(data_sum$switch,data_sum$correct)      #[1] Alt., r=0.333 : 1.34202 ±0%

cor.test(data_sum$explore,data_sum$correct_8)           #For Explore, 0.2385161 
correlationBF(data_sum$explore,data_sum$correct_8)      # [1] Alt., r=0.333 : 18.06661 ±0%


cor.test(data_sum$switch,data_sum$correct_8)           # 0.253036
correlationBF(data_sum$switch,data_sum$correct_8)      #[[1] Alt., r=0.333 : 32.83287 ±0%


# 3) Is there a negative relationship between age & exploration 
cor.test(data_sum$switch,as.numeric(data_sum$AgeYear))         # -.6948858 
correlationBF(data_sum$switch,as.numeric(data_sum$AgeYear))    #  5.880063e+21 ±0%

cor.test(data_sum$explore,as.numeric(data_sum$AgeYear))        #-0.6984207 
correlationBF(data_sum$explore,as.numeric(data_sum$group))    # 1.10257e+37 ±0%


# 4) Is there a negative relationship between the number of stars won and performance on post-test questions?
cor.test(data_sum$totalEarn,data_sum$correct_8)                   #-0.1159607  
correlationBF(data_sum$totalEarn,data_sum$correct_8)             #[1] Alt., r=0.333 : 0.5234265 ±0%

#5) Is there are a negative relationship between age and performance on post-test?
cor.test(data_sum$AgeYear,data_sum$correct)           #-.1089
correlationBF(data_sum$AgeYear,data_sum$correct)      # [1] Alt., r=0.333 : 0.4618303 ±0%

#6) What about the 8 star monster question in the dynamic version?
cor.test(dataDynamic$AgeYear,dataDynamic$correct_8)      #-0.3729988 
correlationBF(dataDynamic$AgeYear,dataDynamic$correct_8) #Alt., r=0.333 : 51.03052 ±0%



######################################
#####          POST TEST           ###
######################################
dataDynamic <- data_sum %>% filter(condition == "dynamic")
dataStatic <- data_sum %>% filter(condition == "static")
glm.fit <- glm(correct_8 ~ group, data = dataDynamic, family= "binomial")
summary(glm.fit)

#Contingency tablesa
static <- xtabs( ~ correct_8 + group, dataStatic )
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
ageBF<-lmBF(correct_8~AgeYear, dataDynamic)
switchgroupBF<-lmBF(correct_8~switch+group, dataDynamic)
exploregroupBF<-lmBF(correct_8~explore+group, dataDynamic)
segBF<-lmBF(correct_8~switch+explore+group, dataDynamic)
rep_allBF<-c(switchBF,exploreBF,groupBF,ageBF,switchgroupBF,exploregroupBF,segBF)

rep_allBF


rep_allBF[1]/rep_allBF[3] #How much better switch is than group
rep_allBF[2]/rep_allBF[3] #How much better explore is than group


# Check for relationship between age and exploration
dataChild<- data_sum %>% filter(group == "child")
plot(dataChild$AgeYear,dataChild$explore)
plot(dataChild$AgeYear,dataChild$switch)
switchAgeBF<-lmBF(switch~AgeYear, dataChild)
exploreAgeBF<-lmBF(explore~AgeYear, dataChild)



####################################################################
#####                   DATA VIZ FOR PAPER                      ####
####################################################################
labels <- c(dynamic = "Dynamic condition", static = "Static condition")

plt<-ggplot(data_sum, aes(x=switch, y=totalEarn,color=group,shape=condition))+
  scale_fill_manual(values = c("#f4d221", "#e5263a"))+
  theme_bw()+
  geom_point(size=4, alpha=.6) +
  #  geom_jitter()+
  ylim(200,500)


plt


##### EARNINGS ######
plt<-ggplot(data_sum, aes(x=group,y=totalEarn,fill=group))+
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
  theme(strip.text.x = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24))

plt

# ggsave(here("plots","exp2_Stars.png"), width = 9.15, height = 5.66)

#### EXPLORE CHOICES ####
plt<-ggplot(data_sum, aes(x=group,y=explore,fill=group))+
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
  theme(strip.text.x = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24))+
  ylim(0,1)


plt
# ggsave(here("plots","exp2_Explore.png"), width = 9.15, height = 5.66)

### SWITCH CHOICES ###
plt<-ggplot(data_sum, aes(x=group,y=switch,fill=group))+
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
  theme(strip.text.x = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24))
plt
# ggsave(here("plots","exp2_Switch.png"), width = 9.15, height = 5.66)
#ggsave("exp2_Switch_FULL.png", width = 9.15, height = 5.46)


#### Post test ###

plt<- ggplot(data_sum, aes(x = group, y = correct_8, fill=group)) +
  stat_summary(fun.y=mean, geom="bar",alpha=.6, colour="black") +
  theme_bw() +
  # stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1)) +
  scale_fill_manual(values = c("#f4d221", "#e5263a"))+
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
eight2<-plt

#ggsave("exp2_8Star.png", width = 9.15, height = 5.46)
#ggsave("exp2_8Star_FULL.png", width = 9.15, height = 5.46)



######################################
## Bayes factors WITHIN conditions  ##
######################################
dataDynamic <- data_sum %>% filter(condition == "dynamic")
dataStatic <- data_sum %>% filter(condition == "static")

### Switching Dynamic ###
switchBF = ttestBF(formula = switch ~ group, data = dataDynamic)
switchBF ## [1] Alt., r=0.707 :  2.803773e+13
switchChains= posterior(ttestBF(formula = switch ~ group, data = dataDynamic),iterations=1000)
mean(switchChains[,2]) # mean difference -0.5801773
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI

mean(exploreChains[,4])# effect size estimite  -1.092418
quantile(exploreChains[,4]) # effect size  CI
cohen.d(formula = explore ~ group, data = dataDynamic) #regular cohen's d -2.480212


### Switching Static ###
switchBF = ttestBF(formula = switch ~ group, data = dataStatic)
switchBF ## [1] Alt., r=0.707 :  1.426302e+20
switchChains= posterior(ttestBF(formula = switch ~ group, data = dataStatic),iterations=1000)
mean(switchChains[,2]) # mean difference  -0.6142476
quantile(switchChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(switchChains[,4])# effect size estimite -3.905164
quantile(switchChains[,4]) # effect size  CI
cohen.d(formula = switch ~ group, data = dataStatic) #regular cohen's d -3.945355 

### Non-max Dynamic ###
exploreBF = ttestBF(formula = explore ~ group, data = dataDynamic)
exploreBF ## [1] Alt., r=0.707 :  1.067564e+14 ±0%
exploreChains= posterior(ttestBF(formula = explore ~ group, data = dataDynamic),iterations=1000)
mean(exploreChains[,2]) # mean difference -0.4755698
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI

mean(exploreChains[,4])# effect size estimite   -2.442152
quantile(exploreChains[,4]) # effect size  CI
cohen.d(formula = explore ~ group, data = dataDynamic) #regular cohen's d  -2.480212

### Non-max Static ###
exploreBF = ttestBF(formula = explore ~ group, data = dataStatic)
exploreBF ## [1] Alt., r=0.707 : 6.424851e+19
exploreChains= posterior(ttestBF(formula = explore ~ group, data = dataStatic),iterations=1000)
mean(exploreChains[,2]) # mean difference -0.4658341
quantile(exploreChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(exploreChains[,4])# effect size estimite  -3.780948
quantile(exploreChains[,4]) # effect size  CI
cohen.d(formula = explore ~ group, data = dataStatic) #regular cohen's d  -3.891021 


### Reward dynamic ###
rewardBF = ttestBF(formula = totalEarn ~ group, data = dataDynamic)
rewardBF ## [1] Alt., r=0.707 :  2.444043e+12 
starChains= posterior(ttestBF(formula = totalEarn ~ group, data = dataDynamic),iterations=1000)
mean(starChains[,2]) # mean difference  109.6937
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(starChains[,4])# effect size estimite  2.237296
quantile(starChains[,4]) # effect size  CI  
cohen.d(formula = totalEarn ~ group, data = dataDynamic) #regular cohen's d   2.273776


### Reward static ###

rewardBF = ttestBF(formula = totalEarn ~ group, data = dataStatic)
rewardBF ## [1] Alt., r=0.707 :  2.050653e+14 ±0%
starChains= posterior(ttestBF(formula = totalEarn ~ group, data = dataStatic),iterations=1000)
mean(starChains[,2]) # mean difference 119.4293
quantile(starChains[,2],probs=c(0.025,0.975)) # mean difference CI
mean(starChains[,4])# effect size estimite  2.963368
quantile(starChains[,4]) # effect size  CI  
cohen.d(formula = totalEarn ~ group, data = dataStatic) #regular cohen's d   3.058838 


# Check for relationship between age and exploration within children
dataChild<- data_sum %>% filter(group == "child")
plot(dataChild$AgeYear,dataChild$explore)
plot(dataChild$AgeYear,dataChild$switch)
switchAgeBF<-lmBF(switch~AgeYear, dataChild)
exploreAgeBF<-lmBF(explore~AgeYear, dataChild)


# Check if children's answers to all of the questions were signifigantly above what chance would predict (Chance being 20%)
ttestBF(dataChild$correct, mu=.2) #289493888445




# Final Plot for paper #
library(patchwork)
(switch1|switch2)/(explore1|explore2)/(earn1|earn2)/(eight1|eight2)
