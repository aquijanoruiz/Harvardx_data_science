#----------------1---------------
library(dslabs)
data(heights)
male<- heights$height[heights$sex=="Male"]
female<- heights$height[heights$sex=="Female"]
#When analyzing data it's often important to know the number of measurements 
#you have for each category

library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)

#----------------2---------------
#Suppose we can't make a plot and want to compare the distributions side by side
#If the number of data points is large, listing all the numbers is inpractical
#A more practical approach is to look at the percentiles

quantile(heights$height, seq(0.05,0.95,0.05))

#Create two five row vectors showing the 10th, 30th, 50th, 70th, and 90th percentiles
#for the heights of each sex called these vectors female_percentiles and male_percentiles.
#Then create a data frame called df with these two vectors as columns. The column names 
#should be female and male and should appear in that order

male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]

female_percentiles<- quantile(female,seq(0.10,0.90,0.2))
male_percentiles<- quantile(male,seq(0.10,0.90,0.2))
df<- data.frame(female=female_percentiles,male=male_percentiles)
df