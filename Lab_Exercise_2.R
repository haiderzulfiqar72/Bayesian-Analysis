---
title: "Lab_Exercise 2"
author: "Haider Zulfiqar"
date: "November 9, 2022"
output:
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
---
  
# Question 1
## (a) 
# Algorithm
1. Generate/draw u(i) ~uniform(0,1)
2. sim.x(i) ~ -log(1/u(i) - 1)
3. Repeat i= 1 to N

## (b)
```{r}
unif<- runif(100000,0,1)
my.func<- function(u){-log((1/u) - 1)}
sim<- my.func(unif)
hist(sim,prob='T')
```

## (c)
```{r}
mean((sim>2)&(sim<3))
```

# Question 2
## (a)
# Algorithm
1. Generate x ~ exp(1) and u ~ uniform(0,1)
2. Accept x if u<= f(x)/M*g(x) = (e^-1 + (e-1)*e^-x(e-1)) / (e^-1)+(e-1)

## (b)
```{r}
acc<- function(x){exp(-x-1)+(exp(1)-1)*exp(-exp(1)*(x))}

x<- 0 #for bound
f<- exp(-x-1)+(exp(1)-1)*exp(-exp(1)*(x))
g<- exp(-x)

m<- f/g
c.b<- 1/m

accept_reject<- function(n){
  samp<- rep(NA, length=n)
  
  for (i in 1:n){
    bool=T
    
    while(bool==T){
      x_star<- rexp(1,1)
      u<- runif(1, 0, 1)
      
      u.sim<- acc(x_star)/(m*exp(-x_star))
      
      if(u<=u.sim){
        bool= F
        samp[i]=x_star
      }
    }
  }
  samp
}
c<- accept_reject(10000)
hist(c, nclass=100, prob=T)
```

# Question 3
## (a)
```{r}
sim.1<- rcauchy(10000)
x<- 2
sim.t1<- pi * sim.1 * exp((-(x-sim.1)^2)/2)
mean(sim.t1)

th<- seq(-15,15, length=1000)
plot(th, th/(1+th^2)*exp(-(2-th)^2/2),type='l' )
```  
## (b)
```{r}
sim.t2<- pi * exp((-(x-sim.1)^2)/2)
mean(sim.t2)
mean(sim.t1)/mean(sim.t2)
```  
