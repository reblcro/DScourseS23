set.seed(100)

X <- matrix(nrow=100000, ncol=10)
for(i in 2:ncol(X)) {
  X[, i] <- rnorm(n = nrow(X), mean = 0, sd = 1)
}
X[,1]<-1

#define eps, beta and Y variables
eps<-rnorm(n=nrow(X),mean=0,sd=.5)
beta<-c(1.5,-1,-.25,.75,3.5,-2,.5,1,1.25,2)
Y<-X%*%beta+eps

#solve for OLS
b_ols<-solve(t(X)%*%X)%*%t(X)%*%Y


#now solve using gradient descent

# Initialize beta coefficients
b_gd <- runif(dim(X)[2])

# Define the learning rate
learning_rate <- 0.0000003

# Define the maximum number of iterations
max_iterations <- 1000

#set the gradient function
gradient <- function(b_gd,Y,X) return(-2 * t(X) %*% (Y - X %*% b_gd))

# Perform gradient descent using a for loop
for (i in 1:max_iterations) {
  b_gd <- b_gd - learning_rate * gradient(b_gd,Y,X)
}

# Print the results
cat("Estimated coefficients:", b_gd)


#Now with nloptr's L-BFGS algorithm
library(nloptr)

objfun <- function(b_lbfgs,Y,X) {
  return (sum((Y-X%*%b_lbfgs)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}
# Gradient of our objective function
gradient <- function(b_lbfgs,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%b_lbfgs)) )
}
# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize!
result <- nloptr(x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)

#Now use Nelder-Mead algorithm from nloptr
objfun <- function(b_lbfgs,Y,X) {
  return (sum((Y-X%*%b_lbfgs)^2))
}
# Gradient of our objective function
gradient <- function(b_lbfgs,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%b_lbfgs)) )
}
# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e5)
# Optimize!
result <- nloptr(x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)

#Answers really don't differ between the two, but I had to increase the max eval for the Nelder-Mead method to reach the correct result

#Now find MLE with nloptr L-BFGS package

## Our objective function
objfun  <- function(theta,Y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

gradient <- function(theta,Y,X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*%beta)/(sig^3)
  return(grad)
}

## read in the data
X <- matrix(nrow=100000, ncol=10)
for(i in 2:ncol(X)) {
  X[, i] <- rnorm(n = nrow(X), mean = 0, sd = 1)
}
X[,1]<-1
eps<-rnorm(n=nrow(X),mean=0,sd=.5)
truebeta<-c(1.5,-1,-.25,.75,3.5,-2,.5,1,1.25,2)
Y<-X%*%truebeta+eps

## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
theta0 <- append(as.vector(summary(lm(Y~X))$coefficients[,1]),runif(1))

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr(x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)


#now the easy way
library(modelsummary)
ols<-lm(Y~X -1)
modelsummary(ols, output="latex")

