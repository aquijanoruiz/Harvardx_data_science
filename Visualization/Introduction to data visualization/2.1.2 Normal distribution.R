#Somooth density: Computes frequencies rather than counts

#While the histogram is an assumption free summary, the smooth density is based on assumptions and choices
#that the data analyst makes

library(dslabs)
data(heights)

index<- heights$sex=="Male"
x<- heights$height[index]
x
mean(x)
sd(x)
