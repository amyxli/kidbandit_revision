#Kidbandit Additional Analyses Script
# Contains code for prop. best choices, + other exploratory analyses
# Data collapsed across study 1 & 2
## Edit history
### 27/08/19 Creation - prop best, stars earned pre-post disc & mean increase, stars earned 1st vs 2nd half (axl)
### 9/11/19 -- ESS looked at
### October 21 -- AXL revision
##### update data loaded to latest version, script organisation, clean-up

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(here)
library(BayesFactor)
library(janitor)


# Load in data, add any variables ####

study1 <- read_csv(here("data_tidy","study1_trialsInfo.csv"))      
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
dynamicTrials<-all_trials[all_trials$condition=="dynamic",]
dynamicSecondHalf<-dynamicTrials[dynamicTrials$trial>40,]

propHalf<-aggregate(data = dynamicTrials, best~half+subjID+group+study, FUN = "mean")
propHalfAll<-aggregate(data = all_trials, best~half+subjID+group+study+condition, FUN = "mean")

propHalf$half<-as.factor(propHalf$half)

# Define variables for plotting labels etc ####

cond.labs <- c("Dynamic version", "Static version")
names(cond.labs) <- c("dynamic", "static")
group.labs<-c("Adults", "Children")
names(group.labs)<- c("adult","child")

cond.labs2 <- c("Dynamic", "Static")
names(cond.labs2) <- c("dynamic", "static")

######################################
# visualisation of "best" choices ####
#####################################

prop_best_info <- all_trials %>%
  dplyr::select(c(subjID,condition,group,trial,earnedThis, best, half)) %>% 
  arrange(subjID) %>%
  distinct %>%
  group_by(subjID, half) %>%
  add_tally (best == 1) %>%
  filter(trial==40 | trial == 80) %>%
  mutate(prop_best=(n/40)) %>%
  dplyr::select(c(subjID,condition,group,half,n, prop_best))

# Proportion of "best" choices
ggplot(
  prop_best_info,
  aes(x = group, y = prop_best, fill=group)
) +
  geom_boxplot(alpha=.5) +
  theme_bw() +
  scale_fill_colorblind() +
  ylab("prop_best") + 
  xlab("Group") +
  theme(legend.position="none") +
  facet_wrap(~condition+half)

# plot frequency of people who chose best option in dynamic, trials 41-80
tmp <- prop_best_info %>% filter(condition == "dynamic" & half == 1)

## density plot of frequencies at all values of prop_best
ggplot(tmp,
       aes(x=prop_best, colour=group)) +
  geom_density() 

## however not possible to report count statistics since prop_best is continuous
## so divide prop_best into 4 levels (prop_best_lvl)

tmp <- tmp %>% mutate(prop_best_lvl = ifelse(prop_best <= 0.2, 1,
                                             ifelse(prop_best > 0.2 & prop_best <= 0.4, 2,
                                                    ifelse(prop_best > 0.4 & prop_best <= 0.6, 3,
                                                           ifelse(prop_best > 0.6 & prop_best <= 0.8, 4,
                                                                  5)))))
tmp$prop_best_lvl <- as.factor(tmp$prop_best_lvl)

## now count how many adults vs kids in each bin
tabyl(tmp, group, prop_best_lvl)

# descriptives of best choice

tmp <- prop_best_info %>% filter(condition == "dynamic")

## first half, dynamic only

subset(tmp, tmp$half == 0 & group == "adult")$prop_best %>% mean() # adults only, 1st half
subset(tmp, tmp$half == 0 & group == "child")$prop_best %>% mean() # children only, 1st half

## second half, dynamic

subset(tmp, tmp$half == 1 & group == "adult")$prop_best %>% mean() # adults only, 2nd half
subset(tmp, tmp$half == 1 & group == "child")$prop_best %>% mean() # children only, 2nd half

tmp <- prop_best_info %>% filter(condition == "static")
## first half, static only

subset(tmp, tmp$half == 0 & group == "adult")$prop_best %>% mean() # adults only, 1st half
subset(tmp, tmp$half == 0 & group == "child")$prop_best %>% mean() # children only, 1st half

## second half, static

subset(tmp, tmp$half == 1 & group == "adult")$prop_best %>% mean() # adults only, 2nd half
subset(tmp, tmp$half == 1 & group == "child")$prop_best %>% mean() # children only, 2nd half

# manuscript plot figure
plt<- ggplot(data= propHalfAll, aes(x=half, y = best, fill=group, group=group))+
  geom_bar(stat = "summary", fun.y = "mean", position="dodge", width = .85, color="black", alpha=.5)+
  scale_fill_manual(values = c("#396AB1","#ed9523"), name = "", labels = c("Adults", "Children"))+
  theme_bw()+
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1), position=position_dodge(.85))+   #error bar
  scale_x_discrete(breaks = c(0,1), labels = c("Trials 1-40", "Trials 41-80"))+
  theme(axis.text.y = element_text( size=12))+
  theme(axis.text.x = element_text(face="bold", size=12))+
  #theme(legend.position =  c(0.92, 0.85))+
  ylim(0,1)+
  ylab("Proporion of time \n choosing the best monster")+
  xlab(" ")+
  # theme(text = element_text(size=20), axis.text.y = element_text(face="bold", size=12))+
  theme(axis.text.y = element_text(face="bold", size=18), strip.text.x = element_text(size = 28),
        strip.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24),
        text = element_text(size=20))+
  facet_wrap(~condition, labeller=labeller(condition=cond.labs))

plt

#ggsave("propBest.png", width = 13.15, height = 5.46)


aggregate(propHalfAll$best~propHalfAll$group+propHalfAll$condition+propHalfAll$half, FUN="mean") #get percents

##########################################################################################
# visualisation of "explore" (non-max) choices as conditionalised on change discovery ####
##########################################################################################

tmp <- subset(all_trials, condition == "dynamic") # dynamic condition only since that's condition where change happens

tmp <- tmp %>%
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

##################################################################################################

data_clean_S1 <- read_csv(here("data_tidy", "bandit-data_sum-clean.csv"))  # study 1 data
data_clean_rep <- read_csv(here("data_tidy","bandit-rep-data_sum-clean.csv"))         # study 2 data
data_clean_rep <- data_clean_rep %>% dplyr::select(c(subjID, condition, switch, group, correct, correct_8, explore, status, time, totalEarn))

data_clean_S1S2 <- rbind(data_clean_S1, data_clean_rep)       # combine study 1 and 2 data

# total points earned by those who discovered the 8-star monster
cumsum_discovered <- 
  data_clean_S1S2 %>% 
  arrange(subjID) %>%
  filter(status==1) %>%  # only those who have discovered 8-star monster
  dplyr::select(c(subjID,condition,group,time)) %>% 
  merge (all_trials) %>%
  arrange(subjID,trial)  %>%
  dplyr::select(c(subjID,condition,group,time, trial, earnedCumulative)) %>%
  filter(time==trial) %>%
  dplyr::select(-c(time)) %>%
  rowwise() %>%
  mutate(avgEarnedPreDisc = (earnedCumulative/trial))
        
cumsum_discovered <- distinct(cumsum_discovered)    # remove duplicate rows/observations

cumsum_final <- 
  data_clean_S1S2 %>% 
  filter(status==1) %>% 
  dplyr::select(c(subjID,condition,group,time)) %>% 
  arrange(subjID) %>%
  merge(all_trials) %>%
  filter(trial==80) %>% 
  dplyr::select(c(subjID,condition,group,trial,earnedCumulative)) 

cumsum_final$avgEarnedPreDisc <- NA

cumsum_final <- distinct(cumsum_final)              # remove duplicate rows/observations

earned_afterDisc <- 
  rbind(cumsum_discovered,cumsum_final) %>% 
  arrange(subjID) %>%
  mutate(earnedLagged = lag(earnedCumulative), 
         trialDiscovered=lag(trial),
         avgEarnedPreDiscLagged=lag(avgEarnedPreDisc)) %>%
  rowwise() %>%
  mutate(earnedafterDisc = earnedCumulative-earnedLagged, # total no. of stars earned by the end of the task - total no. of stars up until discovering 8 stars
         trialsPostDisc = trial-trialDiscovered) %>% # no. of trials after discovering 8 stars
  ungroup() %>%
  filter(trial==80) %>% # keep only rows where useful data is
  dplyr::select(-c(avgEarnedPreDisc, trial, condition)) %>%
  rowwise() %>%
  mutate (avgEarnedPostDisc = earnedafterDisc/trialsPostDisc) %>% # mean no. of stars per trial after discovery
  mutate (avgDiff = avgEarnedPostDisc - avgEarnedPreDiscLagged) %>%
  ungroup()

earned_after_40 <- 
  all_trials %>%
  filter(trial==40 | trial==80) %>%
  select(c(subjID,condition,group,trial,earnedCumulative)) %>% 
  arrange(subjID) %>%
  mutate (earnedLagged = lag(earnedCumulative),
          avg_1sthalf = earnedLagged/40) %>%
  rowwise() %>%
  mutate(earned_2ndhalf=(earnedCumulative-earnedLagged),
         avg_2ndhalf = (earned_2ndhalf/40),
         diff_2nd_1st = (avg_2ndhalf-avg_1sthalf)) %>%
  filter(c(trial==80)) 

# Number of stars earned after discovery of the 8-star option in the dynamic condition 
ggplot(
  earned_afterDisc,
  aes(x = group, y = earnedafterDisc, fill=group)
) +
  geom_boxplot(alpha=.5) +
  theme_bw() +
  scale_fill_colorblind() +
  ylab("earned after discovery") + 
  xlab("Group") +
  theme(legend.position="none") 

library(BayesFactor)

temp_sum = earned_afterDisc[earned_afterDisc$group %in% c("child", "adult"),]
temp_sum$group = factor(temp_sum$group)

plot(earnedafterDisc ~ group, data = temp_sum)
starsEarnedPostDisc = ttestBF(formula = earnedafterDisc ~ group, data = temp_sum)
starsEarnedPostDisc

# [1] Alt., r=0.707 : 0.8929882 ±0.01%

# Comparing the increase in number of stars earned per trial post and pre discovery of 8-star option
ggplot(
  earned_afterDisc,
  aes(x = group, y = avgDiff, fill=group)
) +
  geom_boxplot(alpha=.5) +
  theme_bw() +
  scale_fill_colorblind() +
  ylab("avg earned per trial post-discovery - pre-discovery") + 
  xlab("Group") +
  theme(legend.position="none") 

plot(avgDiff ~ group, data = temp_sum)
test_avgDiff = ttestBF(formula = avgDiff ~ group, data = temp_sum)
test_avgDiff
# [1] Alt., r=0.707 : 0.2798457 ±0.01%

# Total number of stars earned in the second half of the game (trials 40–80)
ggplot(
  earned_after_40,
  aes(x = group, y = earned_2ndhalf, fill=group)
) +
  geom_boxplot(alpha=.5) +
  theme_bw() +
  scale_fill_colorblind() +
  ylab("earned after trial 40") + 
  xlab("Group") +
  theme(legend.position="none") +
  facet_wrap(~condition)

# Increase on the mean number of stars earned per trial in the 2nd half of the task (trials 40–80)
ggplot(
  earned_after_40,
  aes(x = group, y = diff_2nd_1st, fill=group)
) +
  geom_boxplot(alpha=.5) +
  theme_bw() +
  scale_fill_colorblind() +
  ylab("avg stars per trial 2nd half - 1st half") + 
  xlab("Group") +
  theme(legend.position="none") +
  facet_wrap(~condition)


# dynamic trials only, prop best choices analysis

model<-glm(dynamicTrials$best~dynamicTrials$group*dynamicTrials$half)
model<-glm(dynamicSecondHalf$best~dynamicSecondHalf$group+dynamicSecondHalf$trial)


model<-lm(propHalf$best~propHalf$group*propHalf$half)
summary(model)
model<-lm(propHalfAll$best~propHalfAll$group*propHalfAll$half*propHalfAll$condition)


lmBF(data=propHalf, best~group*half)
lmBF(data=propHalf, best~group*half+group+half)

## T-tetsts to compare 
dy1<-propHalfAll[propHalfAll$condition=="dynamic"&propHalfAll$half==0,]
dy2<-propHalfAll[propHalfAll$condition=="dynamic"&propHalfAll$half==1,]
st1<-propHalfAll[propHalfAll$condition=="static"&propHalfAll$half==0,]
st2<-propHalfAll[propHalfAll$condition=="static"&propHalfAll$half==1,]

dy1BF<-ttestBF(data=dy1,formula = best~group)
dy1Chains= posterior(ttestBF(formula = best ~ group, data = dy1),iterations=1000)
mean(dy1Chains[,2]) # mean difference 0.4070531
quantile(dy1Chains[,2],probs=c(0.025,0.975)) # mean difference CI 0.3260756 0.4852745
mean(dy1Chains[,4])# effect size estimite 2.050025
quantile(dy1Chains[,4],probs=c(0.025,0.975)) # effect size  CI 1.575224 2.546720 
cohen.d(formula = best ~ group, data = dy1) #regular cohen's d 2.083907 


dy2BF<-ttestBF(data=dy2, formula = best~group)
dy2Chains= posterior(ttestBF(formula = best ~ group, data = dy2),iterations=1000)
mean(dy2Chains[,2]) # mean difference -0.1423074
quantile(dy2Chains[,2],probs=c(0.025,0.975)) # mean difference CI -0.19920299 -0.08445589 
mean(dy2Chains[,4])# effect size estimite -0.9365979
quantile(dy2Chains[,4],probs=c(0.025,0.975)) # effect size  CI -1.3411924 -0.5245647 
cohen.d(formula = best ~ group, data = dy2) #regular cohen's d  -0.9793778 




st1BF<-ttestBF(data=st1, formula = best~group)
st1Chains= posterior(ttestBF(formula = best ~ group, data = st1),iterations=1000)
mean(st1Chains[,2]) # mean difference 0.4449034
quantile(st1Chains[,2],probs=c(0.025,0.975)) # mean difference CI 0.3685996 0.5190715 
mean(st1Chains[,4])# effect size estimite 2.805344
quantile(st1Chains[,4],probs=c(0.025,0.975)) # effect size  CI 2.192346 3.400605
cohen.d(formula = best ~ group, data = st1) #regular cohen's d  2.877231 




st2BF<-ttestBF(data=st2, formula = best~group)
st2Chains= posterior(ttestBF(formula = best ~ group, data = st2),iterations=1000)
mean(st2Chains[,2]) # mean difference 0.4138434
quantile(st2Chains[,2],probs=c(0.025,0.975)) # mean difference CI 0.3391911 0.4929328 
mean(st2Chains[,4])# effect size estimite 2.491782
quantile(st2Chains[,4],probs=c(0.025,0.975)) # effect size  CI 1.930437 3.022049 
cohen.d(formula = best ~ group, data = st2) #regular cohen's d  2.521786



dy1BF #[1] Alt., r=0.707 : 4.01179e+15 ±0%
dy2BF #[1] Alt., r=0.707 : 8807.573 ±0%
st1BF #5.891429e+18 ±0%
st2BF #Alt., r=0.707 : 3.22587e+15 ±0%

###################
###Switch data#####
###################

all_trials$choice<-all_trials$earnedThis
prevChoice<-all_trials$choice
prevChoice<-c('',prevChoice)
n<-length(prevChoice)
prevChoice<-prevChoice[1:n-1]
all_trials$prevChoice<-prevChoice
all_trials$prevChoice[all_trials$trial==1]<- 0
all_trials$switch<-0
all_trials[all_trials$prevChoice!=all_trials$choice,]$switch<-1

switchByBin<-aggregate(data = all_trials, switch~bin+subjID+group+condition, FUN = "mean",na.rm=TRUE)

# manuscript plot figure
plt<-ggplot(data = switchByBin, aes(x = bin, y = switch, group=bin, fill =group))+
  #geom_line(aes(group=subjID), size=.74,color="black", alpha=.1)+
  #geom_dotplot(binaxis='y', stackdir='centerwhole',dotsize=.7)+
  geom_bar(stat = "summary", fun.y = "mean", position="dodge", width = 0.9, color="black", alpha=.5)+
  # facet_grid(~condition, labeller=labeller(condition=cond.labs2))+
 facet_grid(condition~group, labeller=labeller(condition=cond.labs2, group = group.labs))+
  #geom_jitter(width = .1, alpha = .2)+
  scale_fill_manual(values = c("#396AB1","#ed9523"))+
  theme_bw()+
  xlab("Trial")+
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1, 2, 3),
               labels = c("Trials 1-20", "Trials 21-40", "Trials 41-60", "Trials 61-80"))+ 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1), position=position_dodge(.9))+  #error bar
  theme(axis.text.y = element_text(face="bold", size=18), strip.text.x = element_text(size = 28),
        strip.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 28, angle = 90),
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size=24))+
  ylab("Proporion of switch choices")+
  ylim(0,1)

plt

#ggsave(here("plots", "propSwitchTrials_bybin.png"), width = 18.3, height = 5.46)


switchByBin$bin<-as.factor(switchByBin$bin)

summary(lm(data = switchByBin, switch~bin*group))

switchByBin$kidf<-as.numeric(switchByBin$group)

switchBinBF<-anovaBF(data = switchByBin, formula = switch~bin+kidf)

switchBinBF<-lmBF(data = switchByBin, formula = switch~bin+group)
switchGroupBF<-lmBF(data = switchByBin, formula = switch~group)
binOnlyBFF<-lmBF(data = switchByBin, formula = switch~bin)

switchBinBF/switchGroupBF



switchBinBF[3]/switchBinBF[2]
################################
###Correct on all post-test#####
################################

study1 <- read_csv(here("data_tidy","bandit-data_sum-clean-withreward.csv"))      
age_info<-read_csv("study1_ageinfo.csv")
study1<-merge(study1,age_info, by.x="subjID")
study2 <- read_csv(here("data_tidy","bandit-rep-data_sum-clean.csv"))     
age_info<-read_csv("study2_ageinfo.csv")
study2<-merge(study2,age_info, by.x="subjID")
allDataSum<-rbind(study1,study2)

 
plt<- ggplot(data= allDataSum, aes(x=group, y = correct, fill=group))+
  geom_bar(stat = "summary", fun.y = "mean", position="dodge", width = 0.9, color="black", alpha=.5)+
  scale_fill_manual(values = c("#396AB1","#ed9523"))+
  theme_bw()+
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", aes(width=0.1), position=position_dodge(.9))+   #error bar
#  scale_x_discrete(breaks = c(0,1), labels = c("Trials 1-40", "Trials 41-80"))+
  theme(axis.text.y = element_text(face="bold", size=12))+
  theme(axis.text.x = element_text(face="bold", size=12))+
  theme(legend.position = c("none"))+
  ylim(0,1)+
  ylab("Proporion of correct \n post-test answers")+
  xlab(" ")+
  theme(text = element_text(size=20))+
  facet_wrap(~condition, labeller=labeller(condition=cond.labs, group=group.labs))

plt


dataDynamic <- allDataSum %>% filter(condition == "dynamic")
dataStatic <- allDataSum %>% filter(condition == "static")

t.test(dataDynamic$correct~dataDynamic$group)
t.test(dataStatic$correct~dataStatic$group)

dyCorrect<-ttestBF(data = dataDynamic, formula = correct~group)
stCorrect<-ttestBF(data = dataStatic, formula=correct~group)
summary(aov(allDataSum$correct~allDataSum$group+allDataSum$condition))

a<-lmBF(data=allDataSum, formula=correct~condition+group)
summary(lm(allDataSum$correct~allDataSum$group+allDataSum$condition))

summary(lm(all_trials$switch~all_trials$group+all_trials$trial))

# Correct by monster

study1post <- read_csv(here("data_tidy","study1_posttest.csv"))
study1post$study <- 1

study2post <- read_csv(here("data_tidy","study2_posttest.csv"))  
study2post$study <- 2

combinedPost <- rbind(study1post, study2post)[-1]

# mean overall for adults, study 2
subset(combinedPost, study == 2 & group == "adult")$correct %>% mean()
subset(combinedPost, study == 2 & group == "adult" & condition == "dynamic")$correct %>% mean()
subset(combinedPost, study == 2 & group == "adult" & condition == "static")$correct %>% mean()

# mean overall for children, study 2
subset(combinedPost, study == 2 & group == "child")$correct %>% mean()
subset(combinedPost, study == 2 & group == "child" & condition == "dynamic")$correct %>% mean()
subset(combinedPost, study == 2 & group == "child" & condition == "static")$correct %>% mean()


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


# mean for study 1, dynamic, adults, incl. 8-star question (should replicate prev)
tmp <- subset(combinedPost_long, study == 1 & group == "adult" & condition == "dynamic") %>%
  group_by(subjID) %>%
  summarise(correctProp = mean(correct)) %>%
  ungroup() 

tmp$correctProp %>% mean()

# mean for study 1, dynamic, adults, excl. 8-star question
tmp <- subset(combinedPost_long, study == 1 & group == "adult" & condition == "dynamic") %>%
  filter(question != "8 stars") %>%
  group_by(subjID) %>%
  summarise(correctProp = mean(correct)) %>%
  ungroup() 

tmp$correctProp %>% mean()

# mean for study 2, dynamic, adults,incl. 8-star (should replicate previous)
tmp <- subset(combinedPost_long, study == 2 & group == "adult" & condition == "dynamic") %>%
  group_by(subjID) %>%
  summarise(correctProp = mean(correctProp)) %>%
  ungroup() 

tmp$correctProp %>% mean()

# mean for study 2, dynamic, adults, excl. 8-star
tmp <- subset(combinedPost_long, study == 2 & group == "adult" & condition == "dynamic") %>%
  filter(question != "8 stars") %>%
  group_by(subjID) %>%
  summarise(correctProp = mean(correct)) %>%
  ungroup() 

tmp$correctProp %>% mean()

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
  ylab("Proporion of correct \n post-test answers")+
  xlab(" ")+
  theme(text = element_text(size=20))+
  facet_grid(group~condition) +
  labs(x = "Question")

############################
## Switching Bin analysis ##
############################
kidsSwitch<- switchByBin %>% filter(group == "child")
adultSwitch<- switchByBin %>% filter(group == "adult")

lmBF(data=kidsSwitch, switch~bin)
lmBF(data=adultSwitch, switch~bin)
