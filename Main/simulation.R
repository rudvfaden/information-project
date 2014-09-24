## bayes updaing with normal distribution

# setup paramters
sim <- 9
n <- 100
x <- seq(-5, 5, length=n)

# Vectors to store results

std     <- array(0, dim = c(sim, 1))
mu      <- array(0, dim = c(sim, 1))
rho     <- array(0, dim = c(sim, 1))
alpha   <- array(0, dim = c(sim, 1))
nx      <- array(0, dim = c(n, sim))


# prior
mu[1] 	<- 1
std[1] 	<- 5000
rho[1] 	<- 1/std[1]
nx[,1] 	<- dnorm(x,mean =mu[1],sd=std[1]) 

b <- dnorm(x,mean=mu[1],sd=std[1])
# Signal1 
std.err  <- 5
rho.err	 <- 1/std.err
err		 <- dnorm(1,mean=10,sd=1/rho.err)
s 		 <- 1+err  #signal
alpha[1] <- rho[1]/(rho.err)

# Update1

for (i in 2:sim) {
std[i] 		<- 1/(rho[i-1]+rho.err)
rho[i] 		<- 1/std[i]
alpha[i] 	<- rho[i]/(rho.err)
mu[i] 		<- alpha[i]*s+(1-alpha[i])*mu[i-1]
nx[,i] 		<- dnorm(x,mean =mu[i],sd=std[i])
}
# plot
rain <- heat.colors(sim)
plot(x,nx[,1], type="l", lwd=1, ylim=c(0, 1))
for (i in 2:sim) {
  if (i==sim){
    lines(x,nx[,i], type="l", lwd=1,col="black")
  } else if (i<sim) {
    lines(x,nx[,i], type="l", lwd=1,col=rain[i])
  }
}

