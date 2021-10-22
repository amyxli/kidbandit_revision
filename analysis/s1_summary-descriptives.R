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
## largely same as Emily's code for 'posttest performance' section

## Children
postTestC <- c %>%
  drop_na(posttest) %>%
  filter(posttest != "posttest")

postTestC$group <- "child"
postTestC$correct<-1
postTestC$correct[postTestC$posttest=="incorrect"]<-0
stars8Child <- postTestC[postTestC$stimulus=="Which, if any, of these monsters ever gave you <b>8 stars</b>?</p>",]

## adults
postTestA <- a %>%
  drop_na(posttest) %>%
  filter(posttest != "posttest")

postTestA$group <- "adult"
postTestA$correct<-1
postTestA$correct[postTestA$posttest=="incorrect"]<-0
stars8Adult<-postTestA[postTestA$stimulus=="Which, if any, of these monsters ever gave you <b>8 stars</b>?</p>",]

pTA_correct<-aggregate(data=postTestA, correct~subjID, FUN=mean)
pTA_8<-aggregate(data=stars8Adult, correct~subjID, FUN=identity)

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
child_sum <- left_join(child_sum, c_explore)
child_sum <- left_join(child_sum, c_DiscoveryAll)

adult_sum<-merge(a_switch,pTA_correct, by = "subjID")
colnames(pTA_8)[colnames(pTA_8)=="correct"]<-"correct_8"
adult_sum <- merge(adult_sum, pTA_8, by="subjID")
adult_sum <- left_join(adult_sum, a_explore)
adult_sum <- left_join(adult_sum, a_DiscoveryAll)

data_sum<-rbind(child_sum,adult_sum)


#######################################################################################
##                                Basic descriptives plots                           ## 
#######################################################################################

## Switching
switchPlot <- ggplot(
  data_sum, 
  aes(x = group, y = switch, fill = group)
) +
  geom_violin (alpha=.5) + ## change to violin
  theme_bw()+
  scale_fill_colorblind()+
  labs  (x = "group", y = "switch prop", color = "group\n") +
  facet_grid(~condition) +
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)

## Exploratory choice
explorePlot <- ggplot(
  data_sum,
  aes(x = group, y = explore, fill = group)
) +
  geom_violin (alpha=.5) +
  theme_bw()+
  scale_fill_colorblind()+
  labs  (x = "group", y = "% 'explore' choices", color = "group\n") +
  facet_grid(~condition) +
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)

## Correct answers in static and dynamic
correctPlot <-
  ggplot(
    data_sum,
    aes(x = group, y = correct, fill=group)
  ) +
  geom_boxplot(alpha=.5) +
  theme_bw() +
  scale_fill_colorblind() +
  ylab("prop correct posttest response") + 
  xlab("Group") +
  theme(legend.position="none") +
  facet_grid(~condition)

## Correct answers for 8 stars in dynamic
propCorrect8 <- 
  data_sum %>% 
  filter(condition=="dynamic") %>% 
  group_by(group) %>% 
  summarise(mean(correct_8))



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
