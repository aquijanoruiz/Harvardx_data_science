bead <- rep( c("red","blue"), times=c(2,3)) #replicate events of vectors and lists
bead
sample(bead, 1) #picks up one at random

#Monte Carlo simulation: We repeat the experiment a large enough number of times to make the results 
#practically #equivalent to doing it over and over forever.

B <- 10000
events <- replicate(B, sample(bead,1)) #replicate function allows to repeat any task any number of times
#we want

tab <- table(events)
prop.table(tab)

sample (bead, 5) #picks up five at random and it does it without replacement. This means the beads are
#not put back in the urn. This is an arraangement that always have 2 reds and 3 blues.

sample (bead, 6) #it gives us an error message as there are only 5 beads in the sample space
sample (bead, 6, replace=TRUE) #by changing the replace argument we can sample with replacement

sample (bead, B, replace=TRUE) 
replicate(B, sample(bead,1)) #now these two functions can provide similar answers