#Kidbandit Combined Additional Analyses Script

# Contains code for prop. best choices, + other exploratory analyses
# Data collapsed across study 1 & 2
## Edit history
### 27/08/19 Creation - prop best, stars earned pre-post disc & mean increase, stars earned 1st vs 2nd half (axl)
### 9/11/19 -- ESS looked at
### October 21 -- AXL revision
##### update data loaded to latest version, script organisation, clean-up
##### analyses and figures included in manuscript are labeled #* reported *#

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
            

# Define other dfs needed later ####
dynamicTrials<-all_trials[all_trials$condition=="dynamic",] # dynamic only

# Define variables for plotting labels etc ####

cond.labs <- c("Dynamic version", "Static version")
names(cond.labs) <- c("dynamic", "static")
group.labs<-c("Adults", "Children")
names(group.labs)<- c("adult","child")

cond.labs2 <- c("Dynamic", "Static")
names(cond.labs2) <- c("dynamic", "static")

status.labs <- c("Not discovered", "Discovered")
names(status.labs) <- c(0, 1)

#------------------------------------#
# visualisation of "best" choices ####
#------------------------------------#

# for both static and dynamic conditions
prop_best_all <-aggregate(data = all_trials, best~half+subjID+group+condition, FUN = "mean")

# Proportion of "best" choices
ggplot(
  prop_best_all,
  aes(x = group, y = best, fill=group)
) +
  geom_boxplot(alpha=.5) +
  theme_bw() +
  scale_fill_colorblind() +
  ylab("prop best choices") + 
  xlab("Group") +
  theme(legend.position="none") +
  facet_wrap(~condition+half)

# Plot frequency of people who chose best option in dynamic, trials 41-80

prop_best_dyn<-aggregate(data = dynamicTrials, best~half+subjID+group, FUN = "mean") # dynamic only, both halves
prop_best_dyn$half<-as.factor(prop_best_dyn$half)

tmp <- subset(prop_best_dyn, half == 1) # data for dynamic, trials 41-80

## density plot of frequencies at all values of prop best
ggplot(tmp, 
       aes(x=best, colour=group)) +
  geom_density() 

## however not possible to report count statistics since prop_best is continuous
## so divide prop_best into 4 levels (prop_best_lvl)

tmp <- tmp %>% mutate(prop_best_lvl = ifelse(best <= 0.2, 1,
                                             ifelse(best > 0.2 & best <= 0.4, 2,
                                                    ifelse(best > 0.4 & best <= 0.6, 3,
                                                           ifelse(best > 0.6 & best <= 0.8, 4,
                                                                  5)))))
tmp$prop_best_lvl <- as.factor(tmp$prop_best_lvl)

## now count how many adults vs kids in each bin
tabyl(tmp, group, prop_best_lvl)

# manuscript plot figure 2I: proportion of time choosing the best monster,
# both static and dynamic, broken down by halves of experiment

plt<- ggplot(data= prop_best_all, aes(x=as.factor(half), y = best, fill=group, group=group))+
  geom_bar(stat = "summary", fun.y = "mean", position="dodge", width = .85, color="black", alpha=.5)+
  scale_fill_manual(values = c("#396AB1","#ed9523"), name = "", labels = c("Adults", "Children"))+
  theme_bw()+
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1), position=position_dodge(.85))+   #error bar
  ylim(0,1)+
  ylab("Proportion of time \n choosing the best monster")+
  xlab(" ")+
  scale_x_discrete(breaks = c(0,1), labels = c("Trials 1-40", "Trials 41-80")) +
  theme(strip.text.x = element_text(size = 28),
        strip.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24),
        legend.text = element_text(size=24),
        text = element_text(size=20)) +
  facet_wrap(~condition, labeller=labeller(condition=cond.labs)) 

plt

# ggsave(here("plots", "propBest.png"), width = 13.15, height = 5.86)

#-------------------------------------#
# analysis of prop. "best" choices ####
#-------------------------------------#
#* reported: paper #

# descriptives of prop best choice
aggregate(data = prop_best_all, best~group+condition+half, FUN = "mean")

# analysis of best choices, adults vs. children broken down by 1st and 2nd half ####

## t-tests to compare age groups
dy1<-prop_best_all[prop_best_all$condition=="dynamic"&prop_best_all$half==0,]
dy2<-prop_best_all[prop_best_all$condition=="dynamic"&prop_best_all$half==1,]
st1<-prop_best_all[prop_best_all$condition=="static"&prop_best_all$half==0,]
st2<-prop_best_all[prop_best_all$condition=="static"&prop_best_all$half==1,]

# dynamic, trials 1-40, t-test adult vs child in prop best 
dy1BF<-ttestBF(data=dy1,formula = best~group)
dy1Chains= posterior(ttestBF(formula = best ~ group, data = dy1),iterations=1000)
mean(dy1Chains[,2]) # mean difference 0.4340019
quantile(dy1Chains[,2],probs=c(0.025,0.975)) # mean difference CI 0.3654670 0.5013863 
mean(dy1Chains[,4])# effect size estimite 2.480047

# dynamic, trials 40-80, t-test adult vs child in prop best 
dy2BF<-ttestBF(data=dy2, formula = best~group)
dy2Chains= posterior(ttestBF(formula = best ~ group, data = dy2),iterations=1000)
mean(dy2Chains[,2]) # mean difference -0.08148977
quantile(dy2Chains[,2],probs=c(0.025,0.975)) # mean difference CI -0.156260544 -0.005062601 
mean(dy2Chains[,4])# effect size estimite -0.4159093
quantile(dy2Chains[,4],probs=c(0.025,0.975)) # effect size  CI -0.78719831 -0.02623713

# static, trials 1-40
st1BF<-ttestBF(data=st1, formula = best~group)
st1Chains= posterior(ttestBF(formula = best ~ group, data = st1),iterations=1000)
mean(st1Chains[,2]) # mean difference 0.4050065
quantile(st1Chains[,2],probs=c(0.025,0.975)) # mean difference CI 0.3362364 0.4767578  
mean(st1Chains[,4])# effect size estimite 2.573828
quantile(st1Chains[,4],probs=c(0.025,0.975)) # effect size  CI 2.009807 3.201157 

# static, trials 40-80
st2BF<-ttestBF(data=st2, formula = best~group)
st2Chains= posterior(ttestBF(formula = best ~ group, data = st2),iterations=1000)
mean(st2Chains[,2]) # mean difference 0.4546788
quantile(st2Chains[,2],probs=c(0.025,0.975)) # mean difference CI 0.3730642 0.5333622 
mean(st2Chains[,4])# effect size estimite 2.732036
quantile(st2Chains[,4],probs=c(0.025,0.975)) # effect size  CI 2.149687 3.316281 

dy1BF #[1] Alt., r=0.707 : 2.957546e+20 ±0%
dy2BF #[1] Alt., r=0.707 : 2.365633 ±0%
st1BF #[1] Alt., r=0.707 : 2.854948e+16 ±0%
st2BF #[1] Alt., r=0.707 : 7.850566e+17 ±0%


#-----------------------------------------------------#
# visualisation of switching choices, by trial bin ####
#-----------------------------------------------------#

switchByBin<-aggregate(data = all_trials, switch~bin+subjID+group+condition, FUN = "mean",na.rm=TRUE)

# manuscript plot figure 2J
plt<-ggplot(data = switchByBin, aes(x = bin, y = switch, group=bin, fill =group))+
  geom_bar(stat = "summary", fun.y = "mean", position="dodge", width = 0.9, color="black", alpha=.5)+
  facet_grid(condition~group, labeller=labeller(condition=cond.labs2, group = group.labs))+
  scale_fill_manual(values = c("#396AB1","#ed9523"))+
  theme_bw()+
  xlab("Trial")+
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1, 2, 3),
                     labels = c("1-20", "21-40", "41-60", "61-80"))+ 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1), position=position_dodge(.9))+  #error bar
  theme(strip.text.x = element_text(size = 28),
        strip.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24))+
  ylab("Proportion of switch choices")+
  ylim(0,1)

plt

# ggsave(here("plots", "propSwitchTrials_bybin.png"), width = 18.3, height = 5.46)

#--------------------------------------------------------------------#
## Switching Bin analysis ("Switching behavior across trials")    ####
#--------------------------------------------------------------------#

kidsSwitch<- switchByBin %>% filter(group == "child")
adultSwitch<- switchByBin %>% filter(group == "adult")

#* reported: paper #
lmBF(data=kidsSwitch, switch~bin) #bin : 0.5873183 ±0%
lmBF(data=adultSwitch, switch~bin) #1.810572e+23 ±0.01%

#* reported: figure #
# kids only, by condition
lmBF(data= subset(kidsSwitch, condition == "dynamic"), switch~bin) # [1] bin : 0.2886507 ±0%
lmBF(data= subset(kidsSwitch, condition == "static"), switch~bin) # [1] bin : 0.54382 ±0%

# adults only, by condition
lmBF(data= subset(adultSwitch, condition == "dynamic"), switch~bin) # [1] bin : 29274.07 ±0%
lmBF(data= subset(adultSwitch, condition == "static"), switch~bin) # [1] bin : 1.428513e+24 ±0%
#-----------------------------------------------------------------------------------------##
# visualisation of exploration (non-max + switch as conditionalised on change discovery ####
#-----------------------------------------------------------------------------------------##

dynamicTrials  # dynamic condition data only, since that's condition where change happens

tmp <- dynamicTrials %>%
  dplyr::select(c(subjID, group, status, trial, explore)) %>% 
  arrange(subjID) %>%
  mutate(half=ifelse(trial<41,"trial 1-40","trial 41-80")) %>%
  group_by(subjID, half, group, status) %>%
  summarise(meanNonMax = mean(explore, na.rm = TRUE)) %>% # need na.rm = TRUE since 1st trial will have explore=NA
  ungroup()

ggplot(
  tmp,
  aes(x = half, y = meanNonMax, fill=group)
) +
  geom_boxplot(alpha=.5) +
  theme_bw() +
  scale_fill_colorblind() +
  ylab("Prop non-max") + 
  xlab("Half") +
  theme(legend.position="none") +
  facet_grid(status~group)

# non-maximising (explore) choices, grouped by status (did they discover 8-star?) and age group
nonMax_StatusBin<-aggregate(data = dynamicTrials, explore~bin+subjID+group+status, FUN = "mean",na.rm=TRUE)

ggplot(data = nonMax_StatusBin, aes(x = bin, y = explore, group=bin, fill =group))+
  geom_bar(stat = "summary", fun.y = "mean", position="dodge", width = 0.9, color="black", alpha=.5)+
  facet_grid(status~group, labeller=labeller(status=status.labs, group = group.labs))+
  scale_fill_manual(values = c("#396AB1","#ed9523"))+
  theme_bw()+
  xlab("Trial")+
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1, 2, 3),
                     labels = c("1-20", "21-40", "41-60", "61-80"))+ 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1), position=position_dodge(.9))+  #error bar
  theme(axis.text.y = element_text(face="bold", size=18), strip.text.x = element_text(size = 28),
        strip.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24))+
  ylab("Proportion of non-maximizing choices")+
  ylim(0,1)

plt

# ggsave(here("plots", "propNonMax_byStatusBin.png"), width = 14.3, height = 7.46)

switch_StatusBin<-aggregate(data = dynamicTrials, switch~bin+subjID+group+status, FUN = "mean",na.rm=TRUE)

ggplot(data = switch_StatusBin, aes(x = bin, y = switch, group=bin, fill =group))+
  geom_bar(stat = "summary", fun.y = "mean", position="dodge", width = 0.9, color="black", alpha=.5)+
  facet_grid(status~group, labeller=labeller(status=status.labs, group = group.labs))+
  scale_fill_manual(values = c("#396AB1","#ed9523"))+
  theme_bw()+
  xlab("Trial")+
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1, 2, 3),
                     labels = c("1-20", "21-40", "41-60", "61-80"))+ 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1), position=position_dodge(.9))+  #error bar
  theme(axis.text.y = element_text(face="bold", size=18), strip.text.x = element_text(size = 28),
        strip.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24))+
  ylab("Proportion of switching choices")+
  ylim(0,1)

# ggsave(here("plots", "switch_byStatusBin.png"), width = 14.3, height = 7.46)

#--------------------------------------------------------------------#
##                  Post-test combined analysis                   ####
#--------------------------------------------------------------------#

# Correct by monster

study1post <- read_csv(here("data_tidy","study1_posttest.csv"))
study1post$study <- 1

study2post <- read_csv(here("data_tidy","study2_posttest.csv"))  
study2post$study <- 2

combinedPost <- rbind(study1post, study2post)[-1]

# broken down by monster

combinedPost_long <- combinedPost %>% 
  rename(correctProp = correct) %>%
  pivot_longer(
    cols = c(6:10), 
    names_to = "question", 
    names_prefix = "correct_",
    values_to = "correct"
    )

combinedPost_long$question <- ifelse(combinedPost_long$question == "1", paste0(combinedPost_long$question, " star"), # if "1" then "1 star"
                                     paste0(combinedPost_long$question, " stars")) # if not "1" then "X stars" (e.g., "8 stars")

# plot of correct post-test broken down by monster question
ggplot(data = combinedPost_long,
       mapping = aes(x = question, 
                     y = correct,
                     fill = group)
       ) +
  geom_bar(stat = "summary", fun.y = "mean", position="dodge", width = 0.9, color="black", alpha=.5)+
  scale_fill_manual(values = c("#396AB1","#ed9523"))+
  theme_bw()+
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1), position=position_dodge(.9))+   #error bar
  #  scale_x_discrete(breaks = c(0,1), labels = c("Trials 1-40", "Trials 41-80"))+
  theme(axis.text.y = element_text(face="bold", size=12))+
  theme(axis.text.x = element_text(face="bold", size=12))+
  theme(legend.position = c("none"))+
  ylim(0,1)+
  ylab("Proportion of correct \n post-test answers")+
  xlab(" ")+
  theme(text = element_text(size=20))+
  facet_grid(group~condition) +
  labs(x = "Question")

