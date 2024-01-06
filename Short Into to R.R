#Basic Manipulations

3*(23-7) #R directly computes the result
1:5 # The colon is used to obtain a sequence of numbers

c(2,5,4,3,-2) # The function c creates a vector
c("Robert",1,"Joe",4)

x = c(2,5,4,3,-2) # To store an object you must assign a name to it
x # Visualizing the vector x
x[c(1,3)] # Selects part of the vector
x[-c(1,3)]

length(x)
exp(x) # Note that R works vectorially

x < 4 # Logical operator
x ==4
x[x<4]
sum(x<4)

moy.x = mean(x) # Name convention

y=1:5
z = rep(11, times=4) 
x+y
c(y,z,4,10) # Combining vectors and numbers

Y = rnorm(100,mean=10,sd=sort(5)) # Generates a random vector from a normal distribution
plot(Y,type="p",col='red',pch = 19, cex = 0.1) # Graphical representation of Y with red points
x1 = rbinom(100,8,0.5) # Generates random numbers from a binomial distribution
x2 = rpois(100,2) # Generates random numbers from a Poisson distribution
x3 = Y + rnorm(100,0.5)

model = lm(Y ~ x1 + x2 + x3) # Performs a simple linear regression
summary(model) # Shows the results of this regression

help(plot) # Shows the help of the called function

ls() # Shows the objects created

rm() # Deletes the object

#path = "//"
#weights = read.table(paste)