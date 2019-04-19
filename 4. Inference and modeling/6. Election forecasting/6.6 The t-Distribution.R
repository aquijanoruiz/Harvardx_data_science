#Above we made use of the CLT with a sample size of 15. Because we are estimating a second parameters sigma,
#further variability is introduced into our confidence interval which result in intervals that are too small.

#ES: cuando usamos la fórmula Z =     (X - miu)
#                                 --------------
#                                 sigma/ sqrt(N)
#y no sabemos sigma, podemos usar s (la desviación estándar de la muestra), pero esto añade variabilidad
#a nuestro modelo cuando la muestra es muy pequeña y hace que los intervalos de confianza sean muy cortos 
#(porque usamos la distribución normal)

#En vez de eso podemos utilizar la distribución t que tiene intervalos de confianza más largos (colas más gordas)
#y que toman en cuenta la variabilidad que surge por utilzar sigma en muestras pequeñas. Esta es:

#ES: cuando usamos la fórmula Z =     (X - miu)
#                                 --------------
#                                    s/ sqrt(N)
#La teoría nos dice que Z tiene una distribución t con N-1 grados de libertad y las colas son más gordas.

qt(0.975, 14) #aquí podemos ver que Z es 2.1444 para tener un intervalo de confianza de 95%, con 14 grados de libertad
#es decir (N-1= 15-1). Esto es mucho mayor a:
qnorm(0.975) #que es 1.96 y se puede utilizar cuando sabemos que la muestra es grande

library(dplyr)
library(dslabs)
data(polls_us_election_2016)

library(tidyverse)

polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

one_poll_per_pollster

one_poll_per_pollster %>%
  ggplot(aes(sample=spread)) + stat_qq() #produce quantile-quantile plots

#---------------------------confidence interval using t distribution----------------------------
z <- qt(x=0.975, df=nrow(one_poll_per_pollster)-1) #we caluculate the degrees of freedom counting the rows and substracting one
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>% #z*sd(spread)/sqrt(length(spread)) this is the z value multiplied by the standard error
  mutate(start = avg - moe, end = avg + moe)



