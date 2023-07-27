---
title: "Lab_Excercise 4"
author: "Haider Zulfiqar"
date: "November 29, 2022"
output:
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
---

```{r}
library(rjags)
```

# Question 1
## (a) 
```{r}
cat("
model
{
# Likelihood
  for (i in 1:k){
    for (j in 1:n[i]) {
           y[i,j]  ~ dnorm(mu[i], tauinv) 
           }
           mu[i]  ~ dnorm(phi, gaminv)  #manually taken
    }
# Priors
    phi  ~ dnorm(70, 0.001)  #manually taken
    tauinv   ~ dgamma(102, 202)
    gaminv   ~ dgamma(6, 20)
    tau <- 1/tauinv
    gam <- 1/gaminv
}",
  file="diet.jag" )



diet.data =list( k=4, n=c(4, 6, 6, 8),
                  y= structure(
                    .Data=  c(62, 60, 63, 59, NA, NA, NA, NA, #n=4
                              63, 67, 71, 64, 65, 66, NA, NA, #n=6
                              68, 66, 71, 67, 68, 68, NA, NA, #n=6
                              52, 62, 60, 61, 63, 64, 63, 59), #n=8
                    .Dim=c(4,8)))

diet.inits= list( list(tauinv=1, gaminv=1),list( tauinv=10, gaminv=0.1),list( tauinv=100, gaminv=100))

diet.j <- jags.model( file = "diet.jag",data = diet.data, n.chains = 3, inits =  diet.inits, n.adapt = 2000)

diet.par <- c("mu", "tau")
p.res <- coda.samples( diet.j, var = diet.par, n.iter = 10000, thin = 10 )

summary(p.res)
plot(p.res)

plot(p.res[[1]][,1:2])
```
##(b) 
```{r}
cat("
model
{
# Likelihood
  for (i in 1:k){
    for (j in 1:n[i]) {
           y[i,j]  ~ dnorm(mu[i], tauinv) 
           }
           mu[i]  ~ dnorm(phi, gaminv[i])  #manually taken
           gaminv[i]   ~ dgamma(6, 20)
           gam[i] <- 1/gaminv[i]
    }
# Priors
    phi  ~ dnorm(70, 0.001)  #manually taken
    tauinv   ~ dgamma(102, 202)
    tau <- 1/tauinv
    
}",
  file="diet.jag" )



diet.data =list( k=4, n=c(4, 6, 6, 8),
                 y= structure(
                   .Data=  c(62, 60, 63, 59, NA, NA, NA, NA, #n=4
                             63, 67, 71, 64, 65, 66, NA, NA, #n=6
                             68, 66, 71, 67, 68, 68, NA, NA, #n=6
                             52, 62, 60, 61, 63, 64, 63, 59), #n=8
                   .Dim=c(4,8)))

diet.inits= list( list(tauinv=1, gaminv=rep(1,4)),list( tauinv=10, gaminv=rep(0.1,4)),list( tauinv=100, gaminv=rep(100,4)))

diet.j <- jags.model( file = "diet.jag",data = diet.data, n.chains = 3, inits =  diet.inits, n.adapt = 2000)

diet.par <- c("mu", "tau")
p.res <- coda.samples( diet.j, var = diet.par, n.iter = 10000, thin = 10 )

summary(p.res)
plot(p.res)

plot(p.res[[1]][,1:2])
```

# Question 2
```{r}
cat( "
model
{
# Likelihood
  for(i in 1:6)
  {
    for(j in 1:44)
    {
      y[i,j]~dnorm(mu[i,j], tauinv)
      mu[i,j]<-beta[i]+beta1[i]*time_cen[j]
    }
    
    beta[i]~dnorm(beta0,sigma2inv)   #manually taken
    beta1[i]~dnorm(beta1_p, tau1inv)  #manually taken
  }
  #Priors
  beta0~dnorm(0,1/100)   #manually taken
  beta1_p~dnorm(0,1/100) #manually taken
  
  sigma2inv~dgamma(.01,.01)
  sigma2<-1/sigma2inv
  
  tau1inv~ dgamma(0.01,0.01)
  tau1<- 1/tau1inv
  
  tauinv~dgamma(.01,.01)
  tau<-1/tauinv
}",
  file="ind.jag" )

ind.data= list(time_cen=seq(1:44),
  y = structure(.Data = c(1.1933,1.2173,1.2544,1.2847,1.3198,1.3351,1.3601,1.4116,1.4517,1.4781,
                                       1.5126,1.5306,1.5427,1.5639,1.5798,1.586,1.6107,1.6435,1.6713,1.7154,1.7558,1.7931,1.8195,1.8477,
                                       1.8827,1.9398,1.976,2.0128,2.0441,2.0698,2.0979,2.1284,2.1632,2.2119,2.2651,2.325,2.3587,2.4056,
                                       2.4522,2.5051,2.5607,2.5729,2.5866,2.6127,87.973,90.021,90.12,91.18,89.832,90.668,89.85,91.276,
                                       89.964,91.566,91.405,91.691,89.341,90.31,89.324,89.436,88.815,89.92,90.48,92.367,91.637,94.256,
                                       95.021,96.547,95.45,97.657,97.985,99.383,98.213,99.85,99.877,101.169,99.922,102.186,102.657,104.522
                                       ,103.445,106.05,105.963,107.644,106.425,108.741,108.897,110.26,9.392,13.637,14.392,13.301,
                                       10.411,13.057,13.621,13.354,11.056,14.559,14.067,12.286,9.658,13.659,12.351,12.307,10.508,16.287,
                                       16.697,15.523,13.54,19.19,18.827,17.215,13.848,20.319,20.403,19.179,16.79,24.4,24.215,22.042,18.182,
                                       22.67,21.204,19.746,16.656,24.552,23.775,22.449,18.145,26.344,25.278,22.511,42.96,47.516,43.888,
                                       40.298,40.016,39.073,40.484,38.703,41.919,45.398,46.635,39.97,41.554,48.384,46.041,46.411,47.558,
                                       59.342,57.487,57.165,62.2,74.678,69.074,67.03,70.453,85.102,85.142,71.203,73.38,88.139,92.899,
                                       81.838,75.122,90.515,89.907,77.163,84.508,99.279,94.556,87.039,87.696,102.934,102.095,84.497,
                                       9.203,10.174,11.078,12.334,10.228,10.394,11.246,13.071,10.954,11.41,11.892,13.204,10.439,11.148,
                                       11.556,13.37,11.114,12.043,13.165,15.534,13.002,14.602,15.684,19.088,15.436,16.117,16.782,20.491,
                                       16.383,18.18,19.782,23.605,18.483,19.659,21.042,24.203,20.242,22.143,22.87,27.486,22.399,22.412,
                                       22.223,25.648,30.363,25.812,26.026,38.081,21.909,26.615,27.039,40.17,23.641,30.477,30.065,43.805,
                                       24.256,30.976,30.579,45.471,25.891,33.05,33.385,50.311,29.177,36.855,35.418,53.126,30.325,37.977,
                                       37.028,54.369,31.594,37.617,36.297,52.457,30.718,41.09,40.706,60.491,34.723,42.871,42.072,64.101,
                                       36.516,46.782,47.006,69.966), .Dim = c(6,44)))
               
ind.ini= list( list(beta0=1, beta1_p= 1),list(beta0=5, beta1_p= 5),list(beta0=10, beta1_p= 10) )

ind.j <- jags.model( file = "ind.jag", data = ind.data, n.chains = 3, inits=ind.ini, n.adapt = 2000)
ind.par <- c("beta", "beta1")

ind.res <- coda.samples(ind.j, var =ind.par, n.iter = 10000, thin = 10 )
summary(ind.res)
plot(ind.res)

#acf(ind.res[[1]][,1])
#acf(as.matrix(ind.res))

```

# Question 3
## (a)
```{r}
cat( "
model{
#Likelihood
 for (i in 1:N){
   Y[i] ~ dbern(theta[i])
   probit(theta[i]) <- beta1 + beta2 * x[i]
 }
#Priors
  beta1~dnorm(0,1/100)
  beta2 ~dnorm(0,1/100)
}",
     file="ring.jag" )

ring.dat= list(x=c(53,57,58,63,66,67,67,67,68,69, 70,70,70,70,72,73,75,75,76,76, 78,79,81), Y=c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,0,0),N=23)
ring.ini= list( list(beta1=1, beta2= 1),list(beta1=10, beta2= 5),list(beta1=20, beta2= 40))
ring.j <- jags.model( file = "ring.jag",data=ring.dat, n.chains = 3, inits=ring.ini, n.adapt = 2000)
ring.par <- c("beta1", "beta2")

ring.res <- coda.samples(ring.j, var= ring.par, n.iter = 10000, thin = 10 )
summary(ring.res)
plot(ring.res)
```  
##(b)
```{r}
cat( "
model{
#Likelihood
 for (i in 1:N){
   Y[i] ~ dbern(theta[i])
   probit(theta[i]) <- beta1 + beta2 * (x[i] - mean(x[]))
 }
#Priors
  beta1~dnorm(0,1/100)
  beta2 ~dnorm(0,1/100)
  
  LD50 <- mean(x[]) - beta1/beta2
}",
     file="ring.jag" )

ring.dat= list(x=c(53,57,58,63,66,67,67,67,68,69, 70,70,70,70,72,73,75,75,76,76, 78,79,81), Y=c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,0,0),N=23)
ring.ini= list( list(beta1=1, beta2= 1),list(beta1=10, beta2= 5),list(beta1=20, beta2= 40))
ring.j <- jags.model( file = "ring.jag",data=ring.dat, n.chains = 3, inits=ring.ini, n.adapt = 2000)
ring.par <- c("beta1", "beta2")

ring.res <- coda.samples(ring.j, var= ring.par, n.iter = 10000, thin = 10 )
summary(ring.res)
plot(ring.res)
```
##(c)
```{r}
cat("
model{
#Likelihood
 for (i in 1:N){
   Y[i] ~ dbern(theta[i])
   probit(theta[i]) <- beta1 + beta2 * (x[i] - mean(x[]))
 }
#Priors
  beta1~dnorm(0,1/100)
  beta2~dnorm(0,1/100)

  LD50 <- mean(x[]) - beta1/beta2
  
  #Preduction
  for(i in 1:5){
  probit(theta.new[i])= beta1+beta2*(28+2*i-mean(x[]))
  }
  
}",
     file="ring.jag" )

ring.dat= list(x=c(53,57,58,63,66,67,67,67,68,69, 70,70,70,70,72,73,75,75,76,76, 78,79,81), Y=c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,0,0),N=23)
ring.ini= list( list(beta1=1, beta2= 1),list(beta1=10, beta2= 5),list(beta1=20, beta2= 40))
ring.j <- jags.model( file = "ring.jag",data=ring.dat, n.chains = 3, inits=ring.ini, n.adapt = 2000)
ring.par <- c("theta.new")

ring.res <- coda.samples(ring.j, var= ring.par, n.iter = 10000, thin = 10 )
summary(ring.res)
plot(ring.res)
```
