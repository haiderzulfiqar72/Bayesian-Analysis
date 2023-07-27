---
title: "Lab_Exercise 3"
author: "Haider Zulfiqar"
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
---
  
#Question 1
library(ggplot2)

#(a)

N=10000
init= 0.5
samp = rep(NA,N)      # initialize Storage
samp[1]= init              # store starting position
accept=0             # For the number of acceptance

for (i in 2:N){   
  
  proposal <- rgamma(1, 4, 7)       #Proposal Distribution 
  alpha = (dgamma(proposal, 4.3,6.2)*dgamma(samp[i-1], 4, 7))/(dgamma(samp[i-1], 4.3, 6.2)*dgamma(proposal,4,7))  
  
  if (runif(1) <= alpha){
    samp[i]= proposal       # Accept 
    accept = accept+1         # Update accepted count
  } #end of if
  else 
    samp[i]= samp[i-1]     # Reject
  
} # end of for

length(unique(samp))/N     # acceptance rate

x<- seq(0,2,0.01)
viz_plot<- function(x,y,samp){samples<- list(x=x,y= y) |> data.frame()

ggplot(samples, aes(x=x,y=y))+
  geom_line(color='blue', size=2)+
  geom_histogram(data=list(s=samp) |> data.frame(), aes(x=s,y=..density..), fill='orange', alpha=.4, bins=100)
}

viz_plot(x,dgamma(x,4.3,6.2),samp)

#(b)
N=100000
init= 0.3
samp = rep(NA,N)      # initialize Storage
samp[1]= init              # store starting position
accept=0             # For the number of acceptance

for (i in 2:N){   
  
  proposal <- rnorm(1, 0.7, 0.11)       #Proposal Distribution 
  alpha = (dgamma(proposal, 4.3,6.2)*dnorm(samp[i-1], 0.7, 0.11))/(dgamma(samp[i-1], 4.3, 6.2)*dnorm(proposal,0.7,0.11))  
  
  if (runif(1) <= alpha){
    samp[i]= proposal       # Accept 
    accept = accept+1         # Update accepted count
  } #end of if
  else 
    samp[i]= samp[i-1]     # Reject
  
} # end of for

length(unique(samp))/N     # acceptance rate

x<- seq(0,2,0.01)
viz_plot<- function(x,y,samp){samples<- list(x=x,y= y) |> data.frame()

ggplot(samples, aes(x=x,y=y))+
  geom_line(color='blue', size=2)+
  geom_histogram(data=list(s=samp) |> data.frame(), aes(x=s,y=..density..), fill='orange', alpha=.4, bins=100)
}

viz_plot(x,dgamma(x,4.3,6.2),samp)

#Increasing the variability of the proposal distribution to 0.7
N=100000
init= 0.3
samp = rep(NA,N)      # initialize Storage
samp[1]= init              # store starting position
accept=0             # For the number of acceptance

for (i in 2:N){   
  
  proposal <- rnorm(1, 0.7, 0.7)       #Proposal Distribution 
  alpha = (dgamma(proposal, 4.3,6.2)*dnorm(samp[i-1], 0.7, 0.7))/(dgamma(samp[i-1], 4.3, 6.2)*dnorm(proposal,0.7,0.7))  
  
  if (runif(1) <= alpha){
    samp[i]= proposal       # Accept 
    accept = accept+1         # Update accepted count
  } #end of if
  else 
    samp[i]= samp[i-1]     # Reject
  
} # end of for

length(unique(samp))/N     # acceptance rate

x<- seq(0,2,0.05)
viz_plot<- function(x,y,samp){samples<- list(x=x,y= y) |> data.frame()

ggplot(samples, aes(x=x,y=y))+
  geom_line(color='blue', size=2)+
  geom_histogram(data=list(s=samp) |> data.frame(), aes(x=s,y=..density..), fill='orange', alpha=.4, bins=100)
}

viz_plot(x,dgamma(x,4.3,6.2),samp)


#Question2

#a)
f<- function(x){0.5*dbeta(x,2,20)+0.5*dbeta(x,20,2)}
x<- seq(0,1,length=100)
plot(x, f(x), type='l')

#b)
N= 5000
RW_s <- rep(.5,N)
for (i in 2:N){   
  proposal <- rnorm(1, RW_s[i-1], 0.1)       #Proposal Distribution 
  accept = min(1, f(proposal)/f(RW_s[i-1]))  
  
  if (runif(1) <= accept){
    RW_s[i]= proposal       # Accept 
  } #end of if
  else 
    RW_s[i]= RW_s[i-1]     # Reject
  
} # end of for

plot(RW_s, type= 'l')
hist(RW_s)

#c)
MH_s<- rep(1,N)

for (i in 2:N){   
  proposal <- runif(1)       #Proposal Distribution 
  accept = min(1, f(proposal)/f(MH_s[i-1]))  
  
  if (runif(1) <= accept){
    MH_s[i]= proposal       # Accept 
  } #end of if
  else 
    MH_s[i]= MH_s[i-1]     # Reject
  
} # end of for

plot(MH_s, type= 'l')
hist(MH_s)

#Question 4 (b)

gibbs <- function(niter) {
  
  y_test= rnorm(1000, 10, 4)
  n= length(y_test)
  
  mu<-0  # initial value
  tau<-1  # initial value
  
  mu_sam <- NULL    #  to store samples for mu
  tau_sam <- NULL    #  to store samples for tau
  
  for(i in 1:niter) {
    mu <- rnorm(1, mean(y_test), sqrt(tau/n))      # full conditional dist mu | tau,y
    tau <- (sum((y_test-mu)^2))/rchisq(1,n)    # full conditional dist tau| mu,y
    
    mu_sam <- append(mu_sam,mu)     # collect samples for mu
    tau_sam <- append(tau_sam,tau)     # collect samples for tau
    
  }
  return(list(mus=mu_sam, tau=tau_sam))
}

gibbs.samp= gibbs(100000)
mu.gibbs= gibbs.samp$mus
tau.gibbs= gibbs.samp$tau
hist(mu.gibbs)
hist(tau.gibbs)
