install.packages("rmarkdown")

#1.a : Calculate the probability that a memory stick will be scrapped.

# Define the parameters of the uniform distribution
min_ms <- 2 # minimum length of memory sticks
max_ms<- 12 # maximum length of memory sticks

# Find the probability that a memory stick is longer than 10 mm (p_ms)
p_ms <- 1 - punif(10, min=min_ms, max=max_ms)
p_ms 


#-----------------------------

#1.b : Simulate 50 memory stick lengths and obtain a histogram of the simulated values. 
#      Calculate the simulated mean and variance.

# Set the seed for reproducibility
set.seed(123)
# Simulate 50 memory stick lengths
ms <- runif(n = 50, min = 2, max = 12)

# Create a histogram of the simulated values
hist(ms, main = "Histogram of Memory Stick Lengths",
     xlab = "Memory Stick Length (mm)", ylab = "Frequency")

# Calculate the simulated mean
mean(ms)

# Calculate the simulated variance
var(ms)



#---------------------------------------------------------------------------------------------------------------------

#2.a : Show that, for this to be a genuine probability density, b = 0.15

# Define a function to find the difference between the left-hand side and the right-hand side of the equation
eqn <- function(b) {
  integrate(function(x) {0.025*x + b}, lower = 2, upper = 6)$value - 1
}
# Solve for b using the uniroot function
uniroot(eqn, c(0, 1))$root

#-----------------------------

#2.b : Find the probability that the measurement of the current exceeds 3.
# pdf function
pdf <- function(x, b) {
  0.025 * x + b
}

# integration limits
l <- 3
u <- 6

# set b = 0.15
b <- 0.15

# Integrate the pdf function over the limits
pme<- integrate(pdf, lower = l, upper = u, b)$value
pme

#-----------------------------

#2.c : Find E(X)
function(x) 
  ifelse(6 > x & x > 2, 0.025*x + b, 0)

F_X <- integrate(function(x) x * f(x), lower = 2, upper = 6)$val
round(F_X,2)



#---------------------------------------------------------------------------------------------------------------------



# Probability that X <= 0
punif(0, min = -pi, max = pi)

# Probability that X <= pi/2
punif(pi/2, min = -pi, max = pi)


