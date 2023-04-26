Dat <- read.table(file = 'PineCones.txt')
head(Dat)

# Question 1
# model (1)
X <- Dat$diam
Y <- Dat$cones

plot(X,Y,xlab ="diam", ylab ='cones')

mu.fn.1 <- function(par, diam){
  M <-par["b0"] + par["b1"]*log(diam)
  return(M)
}

negloglik.fn1 <- function(par,cones,diam){
  mu1 <- par["b0"] + par["b1"]*log(diam)
  lambda <- exp(mu1)
  nLL1 <- -sum(dpois(x = cones, lambda = lambda, log = T))
  return(nLL1)
}
start.par <- c(b0 = -5, b1 = 2)
mle.pars1 <- optim(par = start.par,fn = negloglik.fn1, cones = Y, diam = X )
mle.pars1


# model (2)
plot(X^2,Y)

neglink.fn1 <- function(par,cones,diam){
  lambda2 <- par['b']* diam^2
  nLL2 <-  -sum(dpois(x = cones, lambda = lambda2,log = T))
  return(nLL2)
}

start.par <- c(b = 1)
mle.pars2 <- optim(par = start.par,fn = neglink.fn1 ,cones = Y, diam = X )
mle.pars2

#Question 2
#model(1)
trt <- c('AMB','CO2','AMB')
AMB <- 1
CO2 <- 2
K.trt <- c(AMB, CO2)
names(K.trt) <- c('AMB','CO2')
K.vec <- K.trt[trt]

negloglik.fn2 <- function(par,cones,diam,trt){
    K.vec <- K.trt[trt]
    mu <- par["b0"] + par["b1"]*log(diam) + par["b2"]*log(K.vec)
    lambda <- exp(mu)
    nLL <- -sum(dpois(x = cones, lambda = lambda, log = T))
  return(nLL)
}
start.par <- c(b0=0, b1 = 0, b2= 0, AMB= 0, CO2 =0)
mle.pars.trt <- optim(par = start.par,fn = negloglik.fn2, trt = Dat$tmt, 
                      cones = Dat$cones, diam = Dat$diam )

#model(2)
trt <- c('AMB','CO2','AMB')
AMB <- 1
CO2 <- 2
K.trt <- c(AMB, CO2)
names(K.trt) <- c('AMB','CO2')
K.vec <- K.trt[trt]

neglink.fn2 <- function(par,cones,diam,trt){
  K.vec <- K.trt[trt]
  mu <- par["b0"]*(Dat$diam)^2 + par["b1"]*K.vec
  lambda <- mu
  nLL <- -sum(dpois(x = cones, lambda = lambda, log = T))
  return(nLL)
}

start.par <- c(b0 = 1, b1=1, AMB =1, CO2 =2)
mle.pars2.2 <- optim(par = start.par,fn = neglink.fn2,
                   cones = Dat$cones, diam = Dat$diam,trt = Dat$tmt)
mle.pars2.2

#Question3
#model(1)
par.mle.1 <- mle.pars1$par
AIC.mle.1 <- 2*mle.pars1$value + 2*length(par.mle.1) #3638.134

par.mle.1.1 <- mle.pars.trt$par
AIC.mle.1.1 <- 2*mle.pars.trt$value + 2*length(par.mle.1.1) #3469.639

AIC.mle.1 - AIC.mle.1.1  #168.4822
# AIC.mle.1.1 is smaller, so treatments should be considered in the predicted model.
# AIC.mle.1.1 has the smallest AIC values among the four models, so it's the 
# best prediction models in the excercise.

#model (2)

par.mle.2 <- mle.pars2$par
AIC.mle.2 <- 2*mle.pars2$value + 2*length(par.mle.2) #3805.93

par.mle.2.2 <- mle.pars2.2$par
AIC.mle.2.2 <- 2*mle.pars2.2$value + 2*length(par.mle.2.2) #3778.414

AIC.mle.2 - AIC.mle.2.2 #27.51594
#AIC.mle.2.2 is smaller

#Question 4

#negloglik.fn2 is the best model, according to AIC result, the smalllest AIC.
#model with treatments distingushed

n.boot <- 1000   # number of resamples for the Bootstrapping 
n.par <- 5      # number of parameters ?n the model
n <- nrow(Dat)
# create a matrix to store parameter estimates
PAR <- matrix(0, nrow = n.boot, ncol = n.par)

start.par <- par.mle.1.1

for(i in 1:n.boot){
  res.ind <- sample(1:nrow(Dat), size = nrow(Dat), replace = TRUE)
  cones.res <- Dat$cones[res.ind]
  diam.res <- Dat$diam[res.ind]
  trt.res <- Dat$tmt[res.ind]
  mle <- optim(par = start.par, fn = negloglik.fn2, 
               cones = cones.res, diam= diam.res, trt=trt.res)
  PAR[i,] <- mle$par
}

# naming the columns PAR according to names in par 
colnames(PAR) <- names(mle$par)
PAR <- as.data.frame(PAR)
plot(PAR)
summary(PAR)
# standard errors (SE)
sd(PAR$b0) #0.1990947
sd(PAR$b1) #0.06207156
sd(PAR$b2) #0.06501213
sd(PAR$AMB) #2.294434
sd(PAR$CO2) #2.261097
sapply(PAR,sd)
#     b0         b1         b2        AMB        CO2 
# 0.19909468 0.06207156 0.06501213 2.29443422 2.26109706 

# 95% confidence intervals
sapply(PAR, quantile, prob = c(0.025, 0.975))
#         b0       b1        b2       AMB       CO2
# 2.5%  -6.432131 2.493096 0.4805737 -4.296367 -1.551493
# 97.5% -5.665994 2.735102 0.7375016  4.278190  7.375307

#plot
Cols <- c('AMB' = 'red','CO2'='blue') 
plot(Dat$diam, Dat$cones, col = Cols[Dat$tmt], xlab ="Diameter", ylab="Number of cones",
     main = "Correlation between diameter of pine trees and numbers of cones")
legend("topleft",pch =1, col= c('red','blue','purple'), legend= c('AMB','CO2','No treatment effect'))

das.pred <- 1:45 # vector of DaS values for which predictions are calculated
b0 <- par.mle.1.1['b0']
b1 <- par.mle.1.1['b1']
b2 <- par.mle.1.1['b2']
AMB <- par.mle.1.1['AMB']
CO2 <- par.mle.1.1['CO2']

M.pred.AMB <- b0 + b1*log(das.pred) + b2 * log(AMB)
lines(x = das.pred, y= exp(M.pred.AMB), col = 'red')

M.pred.CO2 <- b0 + b1*log(das.pred) + b2 * log(CO2)
lines(x = das.pred, y= exp(M.pred.CO2), col = 'blue')

M.pred <- par.mle.1['b0'] + par.mle.1['b1']*log(das.pred)
lines(x = das.pred, y= exp(M.pred), col = 'purple')

### END ###
