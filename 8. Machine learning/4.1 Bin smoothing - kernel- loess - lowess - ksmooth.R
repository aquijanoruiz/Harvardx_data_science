#We try to estimate the time trend in the 2008 US popular vote poll margin (difference between Obama and McCain).

library(tidyverse)
library(dslabs)
data("polls_2008")

qplot(day, margin, data = polls_2008)

#In this section:

#1. Bin smoothing using the ksmooth() function using the kernel=box argument.
#2. Bin smoothing using the ksmooth() function using the kernel=normal argument.
#3. Fitting a line using the loess() function and the lowess() function.
#4. Fitting a parabola using the loess() function.
#5. ggplot geom_smoth. Adjusting the arguments for a better fit.

#-------------------------------using least square regression------------------------
model <- lm(margin ~ day, data=polls_2008)
model

coefficients <- model$coefficients
B_0 <- coefficients[[1]]
B_1 <- coefficients[[2]]

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_abline(intercept = B_0, slope = B_1)

#-------------------------------1. bin smoothing (box kernel)------------------------
#When we use bin smoothing, we choose a bandwidth. In this case, it is 7. Then we take the the ys that fall into that 
#bandwidth and average them. If we use box kernel, all the ys have the same weight. However, if we choose the normal kernel,
#the weighted weight will take the sape of a Gaussian normal distribution, putting more weight to the local point, and the 
#points closest to the local point.

#We used the argument kernel=box in our call to the function ksmooth. This is because the weight function looks like a box.
head(polls_2008)
span <- 7 #this is the bandwidth (a 7 day span)
fit <- with(polls_2008,
            ksmooth(day, margin, x.points = day, kernel="box", bandwidth = span)) #this is the fuction for the kernel. The default kernel is the "box" kernel. (You can also use the normal kernel)

fit$x #this is the original day data. Equal to polls_2008$day
fit$y #this is the estimated "y"

polls_2008 %>% mutate(smooth = fit$y) %>% 
  ggplot(aes(x=day, y=margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red") #we specify another aesthetic for the estimated y

#Note that the final result for the bin smoother is quite wiggly. One reason for this is that each time the window moves, 
#two points change. So if we start with seven points and change two, that's a substantial percentage of points that are changing.

#-------------------------------2. bin smooting (normal kernel)------------------------
#The ksmooth function provides a “smoother” option which uses the normal density (Gaussian) to assign weights. 
#This means the points closest to the local point have more weight than the points far away from the local point.
#This makes the line smoother. Each point receives a weight between 0 for points that are outside the window and 1
#divided by N0 for points inside the window.

span <- 7 #this is the bandwidth (a 7 day span)
fit_2 <- with(polls_2008,
            ksmooth(x=day, y=margin, x.points = day, bandwidth = span)) #this is the fuction for the kernel

polls_2008 %>% mutate(smooth = fit_2$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

#-------------------------------3. fitting a line (local weighted regression - loess)--------------------------
#https://www.youtube.com/watch?v=Vf7oJ6z2LCc This provides an explanation of this kernel.
#The loess function can fit a line or a parabola. But the default implements a parabola.

total_days <- diff(range(polls_2008$day))

span <- 21/total_days #This can take a value from 0 to 1 (the closer to 1 the smoother it becomes)
fit_loess_line <- loess(formula=margin ~ day, degree=1, span = span, data=polls_2008) #degree is the degree of the polynomials. If it is 1, we fit a line. If it is 2, we fit a parabola.

fit_loess_line$fitted #this is the fitted value estimated in the loess

polls_2008 %>% mutate(smooth = fit_loess_line$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

#An iterative algorithm is implemented in which, after fitting a model in one iteration, outliers are detected and down-weighted
#for the next iteration. To use this option, use the argument family= "symmetric".

#-------------------------------fitting a line (local weighted regression - lowess)--------------------------
span <- 21/total_days #This can take a value from 0 to 1 (the closer to 1 the smoother it becomes)
fit_lowess_line <- lowess(polls_2008$margin ~ polls_2008$day, f = span) #the smoother argument here is called f
#we do not have a data argument, so we need to use the $

fit_lowess_line$y #this is the fitted value estimated in the loess

polls_2008 %>% mutate(smooth = fit_lowess$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

#-------------------------------fitting a parabola (local weighted regression - loess)--------------------------
#To fit a parabola, we only need to change the degree of the polynomial (degree argument) to 2. 

fit_loess_parabola <- loess(formula=margin ~ day, degree=2, span = span, data=polls_2008) #degree is the degree of the polynomials. If it is 1, we fit a line. If it is 2, we fit a parabola.

fit_parabola$fitted #this is the fitted value estimated in the loess

polls_2008 %>% mutate(smooth = fit_loess_parabola$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

#We can comapare the loess degree=1 and the loess degree=2
fit_loess_line #loess degree=1
fit_loess_parabola #loess degree=2. Rafa personally prefers degree equals 1 as it is less prone to noise.

polls_2008 %>% mutate(smooth_1 = fit_loess_line$fitted, smooth_2 = fit_loess_parabola$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1)

#-------------------------------5. ggplot geom_smooth--------------------------
#ggplot uses loess in its geom_smooth function:
polls_2008 %>% ggplot(aes(x=day, y=margin)) +
  geom_point() +
  geom_smooth() #However, the default parameters as they are rarely optimal.

#A more optimal graph:
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color="red", span = 0.15,
              method.args = list(degree=1))
