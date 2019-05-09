#------------------------------cofounders (alson known as covariates)-------------------------
#Admission data from six U.C. Berkeley majors, from 1973, showed that more men were being admitted than
#women: 44% men were admitted compared to 30% women.

rm(list = ls())
library(dslabs)
data(admissions)
admissions

#we calculate a weighted average:

admissions <- admissions %>% 
  rename(acceptance_rate=admitted) #the column admitted represents the accpetance rate, so let's change its name

admissions %>% group_by(gender) %>%
  summarize(percentage =
              round(sum(acceptance_rate*applicants)/sum(applicants),1)) #how it shows in the book 

admissions %>% group_by(gender) %>% 
  mutate(weight=applicants/sum(applicants)) %>%
  summarize(weighted_average=sum(acceptance_rate*weight)) #how it is clearer for me

#-----------------------doing a chi-square test---------------------------
#A statistical test clearly rejects the hypothesis that gender and admission are independent:

admissions %>% group_by(gender) %>%
  summarize(total_admitted = round(sum(acceptance_rate / 100 * applicants)), #why do they devide by 100
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>%
  do(tidy(chisq.test(.)))

#But closer inspection shows a paradoxical result. Here are the percent admissions by major:
admissions %>% select(major, gender, acceptance_rate) %>%
  spread(gender, acceptance_rate) %>%
  mutate(women_minus_men = women - men)

#-----------------------taking the cofounder into account---------------------------
data<- admissions %>%
  group_by(major) %>%
  mutate(weight=applicants/sum(applicants)) %>%
  summarize(major_selectivity = sum(acceptance_rate * weight), #this tells the weighter number of admitted applicants
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) #this is the number of women who applied for each 100 applicants
  
data %>% 
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text() #we can see more women applied to the majors with higher selectivity. An they didn't apply to major with less selectivity

#-----------------------cofounder explained graphically----------------------------

admissions %>%
  mutate(yes = round(acceptance_rate/100*applicants), no = applicants - yes) %>% #we calculate the number of applicants that were accepted
  select(-applicants, -acceptance_rate) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>% #this is the opposite of the spread function. First, we need to select what data we do not want to alter -c("major", "gender"), then  we create a column called "admission" and we fill it with the information in the table that has not been excluded (in this case "yes" and "no"), then we create another column called "number_of_students" with the information that were in the "yes" and "no" columns. 
  ggplot(aes(x=gender, y=number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") + #By default, geom_bar uses stat="bin". This makes the height of each bar equal to the number of cases in each group, and it is incompatible with mapping values to the y aesthetic. If you want the heights of the bars to represent values in the data, use stat="identity" and map a value to the y aesthetic.
  facet_wrap(. ~ major)

admissions %>%
  mutate(percent_admitted = acceptance_rate * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

#-----------------------average after stratifying----------------------------
admissions %>%
  ggplot(aes(major, acceptance_rate, col = gender, size = applicants)) +
  geom_point()
