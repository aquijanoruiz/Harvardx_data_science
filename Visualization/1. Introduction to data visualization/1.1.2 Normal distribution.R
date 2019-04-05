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

#standard unit
#z = (x - average)/sd
z<- scale(x) #To covert to standard units
z

mean(abs(z)<2) #it is almost exactly as 95% which is what the normal distribution predicts

