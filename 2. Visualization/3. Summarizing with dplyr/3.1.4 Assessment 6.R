rm(list=ls())
library(NHANES)
library(dplyr)
data(NHANES)

view(NHANES)

#--------------------1---------------------------
#Filter the NHANES dataset so that only 20-29 year old females are included and 
#assign this new data frame to the object tab

## fill in what is needed
tab <- NHANES %>% filter(Gender=="female") %>% filter(AgeDecade==" 20-29") 
tab

#--------------------2---------------------------
#You will determine the average and standard deviation of systolic blood pressure, 
#which are stored in the BPSysAve variable in the NHANES dataset

## complete this line of code.
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average=mean(BPSysAve,na.rm=TRUE),standard_deviation=sd(BPSysAve,na.rm=TRUE))

#--------------------3---------------------------
#Now we will repeat the exercise and generate only the average blood 
#pressure for 20-29 year old females

ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%.$average

#--------------------4---------------------------
#Let's continue practicing by calculating two other data summaries: the minimum and the maximum

NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(min=min(BPSysAve,na.rm = TRUE),max=max(BPSysAve,na.rm = TRUE))

#--------------------5---------------------------
#We will compute the average and standard deviation of systolic blood pressure for
#females for each age group separately

NHANES %>% 
  filter(Gender=="female") %>%
  group_by(AgeDecade) %>%
  summarize(average=mean(BPSysAve,na.rm = TRUE),standard_deviation=sd(BPSysAve,na.rm = TRUE))

#--------------------6---------------------------
#Calculate the average and standard deviation of systolic blood pressure for males 
#for each age group separately using the same methods as in the previous exercise

NHANES %>% 
  filter(Gender=="male") %>%
  group_by(AgeDecade) %>%
  summarize(average=mean(BPSysAve,na.rm = TRUE),standard_deviation=sd(BPSysAve,na.rm = TRUE))

#--------------------7---------------------------
#Create a single summary table for the average and standard deviation of systolic blood pressure 
#using group_by(AgeDecade, Gender)
NHANES %>% 
  group_by(AgeDecade, Gender) %>%
  summarize(average=mean(BPSysAve,na.rm = TRUE), standard_deviation=sd(BPSysAve,na.rm = TRUE))

#--------------------8---------------------------
#Compute the average and standard deviation for each value of Race1 for males in the age decade 40-49
#Order the resulting table from lowest to highest average systolic blood pressure

NHANES %>% 
  filter(Gender=="male" & AgeDecade == " 40-49") %>%
  group_by(Race1) %>%
  summarize(average=mean(BPSysAve,na.rm=TRUE),standard_deviation=sd(BPSysAve,na.rm=TRUE)) %>%
  arrange((average))
