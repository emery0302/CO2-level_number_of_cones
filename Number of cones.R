Dat <- read.table(file = 'PineCones.txt')
head(Dat)


# Model (1)
X <- Dat$diam
Y <- Dat$cones

plot(X, Y, xlab = "diam", ylab = 'cones')

mu.fn.1 <- function(par, diam){
  M <-par["b0"] + par["b1"]*log(diam)
  return(M)
}

negloglik.fn1 <- function(par, cones, diam){
  mu1 <- par["b0"] + par["b1"]*log(diam)
  lambda <- exp(mu1)
  nLL1 <- -sum(dpois(x = cones, lambda = lambda, log = T))
  return(nLL1)
}

start.par <- c(b0 = -5, b1 = 2)
mle.pars1 <- optim(par = start.par,fn = negloglik.fn1, cones = Y, diam = X )
mle.pars1


# Model (2)
plot(X^2,Y)

neglink.fn1 <- function(par, cones, diam){
  lambda2 <- par['b']* diam^2
  nLL2 <- -sum(dpois(x = cones, lambda = lambda2, log = T))
  return(nLL2)
}

start.par <- c(b = 1)
mle.pars2 <- optim(par = start.par,fn = neglink.fn1 ,cones = Y, diam = X )
mle.pars2


# Model(1.1)

negloglik.fn2 <- function(par, cones, diam, tmt){
  b1 <- c(par['b1.AMB'], par['b1.CO2']) # vector of slope parameters to be chosen from according to 'tmt'
  names(b1) <- c('AMB','CO2') 
  mu <- par['b0'] + b1[tmt] * log(diam) # linear predictor
  lambda <- exp(mu)                     # link function
  nLL <- - sum( dpois(x = cones, lambda = lambda, log = T) ) # neg. log-Likelihood
  return(nLL)
}

start.par <- c(b0 = 0, b1.AMB = 0, b1.CO2 = 0)
MLE.1.tmt <- optim(par = start.par, fn = negloglik.fn2, 
                   cones = Dat$cones, diam = Dat$diam, tmt = Dat$tmt)
print(MLE.1.tmt)

# Model(2.1)

neglink.fn2 <- function(par, cones, diam, tmt){
  b1 <- c(par['b1.AMB'], par['b1.CO2']) # vector of slope parameters to be chosen from according to 'tmt'
  names(b1) <- c('AMB','CO2') 
  lambda <- b1[tmt] * diam^2            # linear predictor
  nLL <- - sum( dpois(x = cones, lambda = lambda, log = T) )  # neg. log-Likelihood
  return(nLL)
}

start.par <- c(b1.AMB = 0.02, b1.CO2 = 0.02)  
MLE.2.tmt <- optim(par = start.par, fn = neglink.fn2, 
                   cones = Dat$cones, diam = Dat$diam, tmt = Dat$tmt)
print(MLE.2.tmt)

####################################################################
# Model comparison with AIC
AIC.1 <- 2*mle.pars1$value + 2*length(mle.pars1$par)
AIC.1.tmt <- 2*MLE.1.tmt$value + 2*length(MLE.1.tmt$par)
AIC.2 <- 2*mle.pars2$value + 2*length(mle.pars2$par)
AIC.2.tmt <- 2*MLE.2.tmt$value + 2*length(MLE.2.tmt$par)

data.frame(AIC.1, AIC.1.tmt, AIC.2, AIC.2.tmt)
min(AIC.1, AIC.1.tmt, AIC.2, AIC.2.tmt)

# Bootstrapping for Model 1.1

n.boot <- 10000
n.par <- 3 # number of parameters in the model
PAR <- matrix(nrow = n.boot, ncol = n.par)

start.par <- MLE.1.tmt$par

for(i in 1:n.boot){
  res.ind <- sample.int(nrow(Dat), replace = TRUE)
  Dat.res <- Dat[res.ind,]
  
  MLE.res <- optim(par = start.par, fn = negloglik.fn2, 
                   cones = Dat.res$cones, diam = Dat.res$diam, 
                   tmt = Dat.res$tmt)
  
  PAR[i,] <- MLE.res$par
}

# naming the columns PAR according to names in par 
colnames(PAR) <- names(MLE.res$par)
# and convert to data frame
PAR <- as.data.frame(PAR)
summary(PAR)
plot(PAR)

# Calculate limits of confidence intervals as 2.5% and 97.5% quantile
quantile(x = PAR$b0, prob = c(0.025, 0.975))
quantile(x = PAR$b1.AMB, prob = c(0.025, 0.975))
quantile(x = PAR$b1.CO2, prob = c(0.025, 0.975))

# Plot bootstrap samples for 'b1.AMB' and 'b1.CO2' as histograms
# set up two plot panels on top of each other 
par(mfrow = c(2,1))
# get total range of parameter values (as equal x-axis limits in both plots)
x.range <- range( c(PAR$b1.AMB, PAR$b1.CO2) )
hist(PAR$b1.AMB, col = Col['AMB'], xlim = x.range)
hist(PAR$b1.CO2, col = Col['CO2'], xlim = x.range)
####################################################################


