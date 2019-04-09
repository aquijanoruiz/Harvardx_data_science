#CREATING DATA FRAMES
grades <- data.frame(names=c("John","Juan","Jean","Yao"), exam_1= c(95,80,90,85),exam_2=c(90,85,85,90))
class(grades$names) #the variable is a factor
grades <- data.frame(names=c("John","Juan","Jean","Yao"), exam_1= c(95,80,90,85),exam_2=c(90,85,85,90)
                     ,stringsAsFactors = FALSE)
class(grades$names) #now the variable is a character
