---
title: "Lab_Excercise 1"
author: "Haider Zulfiqar"
date: "November 1, 2022"
output:
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
---
  
# Question 1
## (I) 
```{r}
x <- rbeta(10000,6,5) 
hist(x, prob=T)         
#Empiricals for a Beta distribution

p<- seq(0,1, length=100)
lines(p,dbeta(p,6,5),type="l")
```
## (II) 
```{r}
odds<- x/(1-x)
hist(odds, prob=T, nclass=30)
```
## (III)
```{r}
mean(odds)
sd(odds)
```  
## (IV)
```{r}
mean(odds>=1)
mean(x>=0.5)
```  
## (V)
```{r}
quantile(odds,c(.05,0.95))
```  

# Question 2
## (I)
```{r}
z<- pnorm(3, mean = 0, sd = 1) - pnorm(1, mean = 0, sd = 1)
z
```
## (II)
```{r}
z.sim<- rnorm(100000,0,1)
mean(z.sim>1 & z.sim<3)
var(z.sim>1 & z.sim<3)
```
## (III)
```{r}
z.sim1 <- runif(10000,1,3)
mean(2*dnorm(z.sim1))
var(2*dnorm(z.sim1))
```

# Question 3
## I)
```{r}
u<- runif(100000000)
sim.u<- -2*log(u)
hist(sim.u,prob=T)

x<- seq(0,10,length=100)
lines(x,dexp(x,0.5),type='l')
```  
## (II)
```{r}
sim<- rexp(10000000, 0.5)
mean(2*sim^2*sin(pi*sim))
```  
## (III)
```{r}
mean(2*sim.u^2*sin(pi*sim.u))
```  

# Question 4
## (I) 
```{r}
dens= function(x){(2+(sin(x))^2)*exp(-(3+(cos(x))^3)*(x))}

sim.e<- rexp(10000000,2)
nc<- 1/mean(dens(sim.e)/dexp(sim.e,2))
nc
var(dens(sim.e)/dexp(sim.e,2))

```
## (II)
```{r}
sim.e1<- rexp(10000000,0.1)
nc.1<- 1/mean(dens(sim.e1)/dexp(sim.e1,0.1))
nc.1
var(dens(sim.e1)/dexp(sim.e1,0.1))
``` 

# Question 5
## (I)
```{r}
acc<- function(x){ 2/5 * (2+cos(x))*exp(-x)}

x<- 0 #since cos has highest value of 1 at x=0   x===> change accordingly
f<- 2/5 * (2+cos(x))*exp(-x)
g<- exp(-x)

m<- f/g
m
c.b<- 1/m
c.b
```
## (II)
```{r}
accept_reject<- function(n){
  samp<- rep(NA, length=n)
  
  for (i in 1:n){
    bool=T
    
    while(bool==T){
      x_star<- rexp(1,1)   #instrumental distribution ==>change accordingly
      u<- runif(1, 0, 1)
      
      u.sim<- acc(x_star)/(m*exp(-x_star))  # -x_star ==>change accordingly
      
      if(u<=u.sim){
        bool= F
        samp[i]=x_star
      }
    }
  }
  samp
}
c<- accept_reject(10000)
hist(c, nclass=80, prob=T)
```
