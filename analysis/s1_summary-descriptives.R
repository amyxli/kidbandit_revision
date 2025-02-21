#Kidbandit Descriptives and Analysis Script
#Date: 06/2/2019
#Last edited by AXL 20/10/21

## This script does higher level cleaning and summarizing of data from the first 24
## adult and child participants. It outputs a summary file containing descriptives of
## main DVs of interest: bandit-data_sum-clean.csv.
## ESS added summary stats for stars won 

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(here)

c <- read_csv(here("data_tidy","bandit-child-001-024-lowlevel.csv"))      
a <- read_csv(here("data_tidy","bandit-adult-001-024-lowlevel.csv"))     

# basic demographics ####
tmp <- a %>%
  select(c(subjID, gender)) %>%
  unique()

tmp <- c %>% 
  select(c(subjID, gender)) %>%
  unique()
#########################################################################################
##                                  Switch trials                                      ## 
#########################################################################################

##Children first
# pick just the trials where feedback is given for stars earned
c_trials<- c %>% 
  filter(trial_type=="audio-keyboard-response")  %>% 
  arrange(subjID)

c_trials$earnedThis<-as.numeric(c_trials$earnedThis)
c_trials$group <- "child"

c_switchTrials <- c_trials %>%
  group_by(subjID) %>%
  mutate(earnedPrev = lag(earnedThis)) %>%
  rowwise() %>%
  mutate(same = as.numeric(earnedThis == earnedPrev)) %>%
  ungroup() %>%
  mutate(switch = 1-same)

c_switch <- aggregate(data=c_switchTrials, switch~subjID+condition+group, FUN=mean)

## Adults
# same process as for children
a_trials<- a %>% filter(trial_type=="audio-keyboard-response")

a_trials$earnedThis<-as.numeric(a_trials$earnedThis)
a_trials$group <- "adult"

a_switchTrials <- a_trials %>%
  group_by(subjID) %>%
  mutate(earnedPrev = lag(earnedThis)) %>%
  rowwise() %>%
  mutate(same = as.numeric(earnedThis == earnedPrev)) %>%
  ungroup() %>%
  mutate(switch = 1-same)

a_switch <- aggregate(data=a_switchTrials, switch~subjID +condition+group, FUN=mean)

#######################################################################################
##                               'Explore' trials                                    ## 
#######################################################################################
## Children 
c_exploreTrials <- c_trials %>%
  group_by(subjID) %>%
  mutate(highestVal = accumulate(earnedThis,max)) %>%
  mutate(highestValLag = lag(highestVal)) %>%
  rowwise() %>%
  mutate(exploit = as.numeric(earnedThis == highestValLag)) %>%
  ungroup() %>%
  mutate(explore = 1-exploit)

c_explore <- aggregate(data=c_exploreTrials, explore~subjID+condition+group, FUN=mean)

## Adults
a_exploreTrials <- a_trials %>%
  group_by(subjID) %>%
  mutate(highestVal = accumulate(earnedThis,max)) %>% 
  mutate(highestValLag = lag(highestVal)) %>%
  mutate(exploit = as.numeric(earnedThis == highestVal)) %>%
  ungroup() %>%
  mutate(explore = 1-exploit)

a_explore <- aggregate(data=a_exploreTrials, explore~subjID+condition+group, FUN=mean)

## pre-aggregation data for exploratory analysis later
glimpse(c_exploreTrials)
glimpse(a_exploreTrials)

glimpse(c_switchTrials)
glimpse(a_switchTrials)

c_exploreTrials <- c_exploreTrials %>% dplyr::select(-c(dot))
c_switchTrials <- c_switchTrials %>% dplyr::select(-c(dot))

a_exploreTrials <- a_exploreTrials %>% dplyr::select(-c(country))
a_switchTrials <- a_switchTrials %>% dplyr::select(-c(country))

exploreTrials <- rbind(c_exploreTrials, a_exploreTrials)
switchTrials <- rbind(c_switchTrials, a_switchTrials)

trialsInfo <- left_join(exploreTrials, switchTrials)

trialsInfo <- trialsInfo %>% arrange(subjID, trial_index)

#######################################################################################
##                                Posttest performance                               ## 
#######################################################################################

## Children
postTestC <- c %>%
  drop_na(posttest) %>%
  filter(posttest != "posttest")

postTestC$correct<-1
postTestC$correct[postTestC$posttest=="incorrect"]<-0
postTestC$group <- "child"
stars8Child <- postTestC[postTestC$stimulus=="Which, if any, of these monsters ever gave you <b>8 stars</b>?</p>",]
stars1Child <- postTestC[postTestC$stimulus=="Which, if any, of these monsters ever gave you <b>1 star</b>?</p>",]
stars2Child <- postTestC[postTestC$stimulus=="Which, if any, of these monsters ever gave you <b>2 stars</b>?</p>",]
stars3Child <- postTestC[postTestC$stimulus=="Which, if any, of these monsters ever gave you <b>3 stars</b>?</p>",]
stars6Child <- postTestC[postTestC$stimulus=="Which, if any, of these monsters ever gave you <b>6 stars</b>?</p>",]



pTC_correct<-aggregate(data=postTestC, correct~subjID, FUN=mean)
pTC_8<-aggregate(data=stars8Child, correct~subjID, FUN=identity)
pTC_1<-aggregate(data=stars1Child, correct~subjID, FUN=identity)
pTC_2<-aggregate(data=stars2Child, correct~subjID, FUN=identity)
pTC_3<-aggregate(data=stars3Child, correct~subjID, FUN=identity)
pTC_6<-aggregate(data=stars6Child, correct~subjID, FUN=identity)


## adults
postTestA <- a %>%
  drop_na(posttest) %>%
  filter(posttest != "posttest" & posttest != "\\" & posttest != "posttest\\") 
# some weird thing happened in data writing for the adults such that there were random backslashes
# remove these rows

postTestA$posttest <- str_remove(postTestA$posttest, "\\\\") # remove trailing backslashes from posttest correct data

postTestA$group <- "adult"
postTestA$correct<-1
postTestA$correct[postTestA$posttest=="incorrect"]<-0

stars8Adult<-postTestA[postTestA$stimulus=="Which, if any, of these monsters ever gave you <b>8 stars</b>?</p>",]
stars1Adult <- postTestA[postTestA$stimulus=="Which, if any, of these monsters ever gave you <b>1 star</b>?</p>",]
stars2Adult <- postTestA[postTestA$stimulus=="Which, if any, of these monsters ever gave you <b>2 stars</b>?</p>",]
stars3Adult <- postTestA[postTestA$stimulus=="Which, if any, of these monsters ever gave you <b>3 stars</b>?</p>",]
stars6Adult <- postTestA[postTestA$stimulus=="Which, if any, of these monsters ever gave you <b>6 stars</b>?</p>",]


pTA_correct<-aggregate(data=postTestA, correct~subjID, FUN=mean)
pTA_8<-aggregate(data=stars8Adult, correct~subjID, FUN=identity)
pTA_1<-aggregate(data=stars1Adult, correct~subjID, FUN=identity)
pTA_2<-aggregate(data=stars2Adult, correct~subjID, FUN=identity)
pTA_3<-aggregate(data=stars3Adult, correct~subjID, FUN=identity)
pTA_6<-aggregate(data=stars6Adult, correct~subjID, FUN=identity)

#######################################################################################
##                         Change discovery in dynamic                               ## 
#######################################################################################

## Children
c_trials$trial_index <- as.integer(c_trials$trial_index)

c_trials <- c_trials %>% mutate(trial = trial_index/2 - 2.5) 

c_PostChange <- c_trials %>% filter(condition == "dynamic" & trial >= 41)

c_PostChange1 <- c_PostChange %>%
  group_by(subjID) %>%
  rowwise() %>%
  mutate(chose8 = as.numeric(earnedThis == 8)) %>%
  ungroup() %>%
  dplyr::select(c(trial_index, trial, subjID, condition, earnedThis, chose8)) %>%
  group_by(subjID) %>% ## trial number formula
  add_tally (chose8 == 1) %>% ## column tallying number of trials for subj on which they chose 8
  ungroup() %>% 
  dplyr::select (-c(trial_index)) %>%
  mutate (
    status = case_when(
      n == 0 ~ 0,
      n != 1 ~ 1
      ) 
    ) %>%
  dplyr::select(-c(n))

c_DiscoveryTrials <- c_PostChange1 %>% 
  filter(chose8 == 1) %>% 
  group_by(subjID) %>% 
  mutate(time = min(trial)-1) %>% ## time is number of trials it takes to discover 8
  ungroup() 

c_DiscoveredSubjs <- c_DiscoveryTrials %>% 
  dplyr::select(c(subjID, condition, status, time)) %>% 
  unique() %>% 
  dplyr::select(-c(status))

c_DiscoveryAll <- right_join(c_DiscoveredSubjs, c_PostChange1)
c_DiscoveryAll <- c_DiscoveryAll %>% 
  mutate(
    time = replace(
      time, which(
        is.na(time)
        ), 80
      )
    )

c_DiscoveryAll <- c_DiscoveryAll %>% 
  dplyr::select(c(subjID, condition, status, time)) %>% 
  unique() %>% 
  mutate(group="child")

## Adults
a_trials$trial_index <- as.integer(a_trials$trial_index)

a_trials <- a_trials %>% arrange(subjID, trial_index)

a_trials <- a_trials %>% 
  group_by(subjID) %>% 
  mutate(trial= row_number()) %>% ## need to assign trial numbers first not conditional on trial_index due to attn check
  ungroup()

a_PostChange <- a_trials %>% 
  filter(condition == "dynamic" & trial >= 41) 

a_PostChange1 <- a_PostChange %>%
  group_by(subjID) %>%
  rowwise() %>%
  mutate(chose8 = as.numeric(earnedThis == 8)) %>%
  ungroup() %>%
  dplyr::select(c(subjID, trial, condition, earnedThis, chose8)) %>%
  group_by(subjID) %>%
  add_tally (chose8 == 1) %>% ## column tallying number of trials for subj on which they chose 8
  ungroup() %>% 
  mutate (
    status = case_when(
      n == 0 ~ 0,
      n != 1 ~ 1
    ) 
  )  %>%
  dplyr::select(-c(n))

a_DiscoveryTrials <- a_PostChange1 %>% 
  filter(chose8 == 1) %>% 
  group_by(subjID) %>% 
  mutate(time = min(trial)-1) %>% ## time is number of trials it takes to discover 8
  ungroup() 

a_DiscoveredSubjs <- a_DiscoveryTrials %>% 
  dplyr::select(c(subjID, condition, status, time)) %>% 
  unique() %>% 
  dplyr::select(-c(status))

a_DiscoveryAll <- right_join(a_DiscoveredSubjs, a_PostChange1)
a_DiscoveryAll <- a_DiscoveryAll %>% 
  mutate(
    time = replace(
      time, which(
        is.na(time)
      ), 80
    )
  )

a_DiscoveryAll <- a_DiscoveryAll %>% 
  dplyr::select(c(subjID, condition, status, time)) %>% 
  unique() %>% 
  mutate(group="adult")

# add discovery status to exploration_trials

tmp <- rbind(c_DiscoveryAll, a_DiscoveryAll) %>% 
  unique() %>% 
  arrange(subjID)

trialsInfo <- left_join(trialsInfo, tmp) 

# Write dataframe that has explore(non-max), switch, status, and time discovered data
# write.csv(trialsInfo, here("data_tidy","study1_trialsInfo.csv"), row.names = FALSE)  

#######################################################################################
####                           Putting it together                                 #### 
#######################################################################################

child_sum <- merge(c_switch,pTC_correct, by = "subjID")
colnames(pTC_8)[colnames(pTC_8)=="correct"]<-"correct_8"
child_sum <- merge(child_sum, pTC_8, by="subjID")

child_sum <- merge(c_switch,pTC_correct, by = "subjID")
colnames(pTC_8)[colnames(pTC_8)=="correct"]<-"correct_8"
colnames(pTC_1)[colnames(pTC_1)=="correct"]<-"correct_1"
colnames(pTC_2)[colnames(pTC_2)=="correct"]<-"correct_2"
colnames(pTC_3)[colnames(pTC_3)=="correct"]<-"correct_3"
colnames(pTC_6)[colnames(pTC_6)=="correct"]<-"correct_6"


child_sum <- merge(child_sum, pTC_8, by="subjID")
child_sum <- merge(child_sum, pTC_1, by="subjID")
child_sum <- merge(child_sum, pTC_2, by="subjID")
child_sum <- merge(child_sum, pTC_3, by="subjID")
child_sum <- merge(child_sum, pTC_6, by="subjID")
child_posttest<-child_sum

child_sum <- left_join(child_sum, c_explore)
child_sum <- left_join(child_sum, c_DiscoveryAll)

adult_sum<-merge(a_switch,pTA_correct, by = "subjID")
colnames(pTA_8)[colnames(pTA_8)=="correct"]<-"correct_8"
adult_sum <- merge(adult_sum, pTA_8, by="subjID")

colnames(pTA_1)[colnames(pTA_1)=="correct"]<-"correct_1"
colnames(pTA_2)[colnames(pTA_2)=="correct"]<-"correct_2"
colnames(pTA_3)[colnames(pTA_3)=="correct"]<-"correct_3"
colnames(pTA_6)[colnames(pTA_6)=="correct"]<-"correct_6"
adult_sum <- merge(adult_sum, pTA_1, by="subjID")
adult_sum <- merge(adult_sum, pTA_2, by="subjID")
adult_sum <- merge(adult_sum, pTA_3, by="subjID")
adult_sum <- merge(adult_sum, pTA_6, by="subjID")
adult_posttest<-adult_sum

all_posttest<-rbind(child_posttest,adult_posttest)
all_posttest<-as.data.frame(all_posttest)

# write.csv(all_posttest,here("data_tidy", "study1_posttest.csv"))

adult_sum <- left_join(adult_sum, a_explore)
adult_sum <- left_join(adult_sum, a_DiscoveryAll)

data_sum<-rbind(child_sum,adult_sum)

#######################################################################################
##                                Total Stars Won                                    ## 
#######################################################################################
## Children 
c_earned<- c %>% 
  filter(trial_type=="audio-keyboard-response")  %>%                          #Feedback trials only, arranged by subjid
  arrange(subjID)
c_earned$earnedThis<-as.numeric(c_earned$earnedThis)    
c_totalEarn <- aggregate(data=c_earned, earnedThis~subjID+condition, FUN=sum) #Sum of total stars earned
c_totalEarn$group<-"child"

a_earned<- a %>% 
  filter(trial_type=="audio-keyboard-response")  %>%                          #Feedback trials only, arranged by subjid
  arrange(subjID)
a_earned$earnedThis<-as.numeric(a_earned$earnedThis)    
a_totalEarn <- aggregate(data=a_earned, earnedThis~subjID+condition, FUN=sum) #Sum of total stars earned
a_totalEarn$group<-"adult"

totalEarn<-rbind(c_totalEarn,a_totalEarn)                                     #Combine both

data_sum <- left_join(data_sum, totalEarn) %>% rename(totalEarn = earnedThis)

# write.csv(data_sum,
#           here("data_tidy","study1_data_sum.csv"),
#           row.names=FALSE)

#a_age<-aggregate(data = a, age~subjID+gender+country, mean)
#write.csv(a_age,"Study1Adults_SubjInfo.csv")
