#Property 1
#The expected value of the sum of random variables is the sum of each random variable’s 
#expected value. We can write it like this:
#E[X1+X2+⋯+Xn]=E[X1]+E[X2]+⋯+E[Xn]

#If the  X  are independent draws from the urn, then they all have the same expected value. 
#Let’s call it  μ  and thus:
#E[X1+X2+⋯+Xn]=nμ

#Property 2
#The expected value of a non-random constant times a random variable is the non-random constant
#times the expected value of a random variable. This is easier to explain with symbols:
#E[aX]=a×E[X]
#To see why this is intuitive, consider change of units. If we change the units of a random variable, 
#say from dollars to cents, the expectation should change in the same way. 
#E[(X1+X2+⋯+Xn)/n]=E[X1+X2+⋯+Xn]/n=nμ/n=μ

#Property 3
#The square of the standard error of the sum of independent random variables is the sum of 
#the square of the standard error of each random variable. This one is easier to understand in math form:
#SE[X1+X2+⋯+Xn]=√SE[X1]2+SE[X2]2+⋯+SE[Xn]2
#Variance = square of the standard error

#Property 4
#The standard error of a non-random constant times a random variable is the non-random constant 
#times the random variable’s standard error. As with the expectation:
#SE[aX]=a×SE[X]
#To see why this is intuitive, again think of units.

#The standard error of the average of independent draws from the same
#urn is the standard deviation of the urn divided by the square root of n
#= σ/√n

#Property 5
#If X is a normally distributed random variable, then if a and b are non-random constants, aX + b is
#also a normally distributed random variable. All we are doing is changing the units of the random
#variable by multiplying by a, then shifting the center by b.