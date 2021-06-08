# Loading libraries
library(tidyverse)
library(HMDHFDplus)

# Setting user and password for HMD

user.n <- readline(prompt="Enter HMD username: ")
user.pass <- readline(prompt="Enter HMD password: ")


# Take population info from Finland:
finland <- readHMDweb(CNTRY= "FIN",item="Population",
                      username= user.n,
                      password= user.pass,
                      fixup=TRUE)

glimpse(finland)


# Wrangling time
pop <- finland %>% 
        group_by(Year) %>% 
        summarise(Pop = sum(Total1, na.rm = TRUE))
  
# Quick look
plot(pop$Year, pop$Pop, t = "n", col=1, lwd=2,
     ylab="population size", xlab="Year",
     main = "Finland's Population")
lines(pop$Year, pop$Pop,col=1,lwd=2)


# Ex 1.1: Estimate population growth rate using 2 different time intervals: last 5 and 10 year intervals
last.y <- max(pop$Year)

# Point Populations for calculating r
N1 <- pop$Pop[pop$Year == last.y]
N0.fiver <- pop$Pop[pop$Year == last.y - 5]
N0.tenr <- pop$Pop[pop$Year == last.y - 10]

# Solving r for constant exponential model
r.fiver <- (1/5)*log(N1/N0.fiver)
r.tenr <- (1/10)*log(N1/N0.tenr)


# Ex 1.2: Project population 25-years ahead with the 2 found growth rates

# Function to return Population at moment t, given rate r
PopProj <- function(N0,r,t){
  NT <- N0*exp(r*t)
  return(NT)
}


N0 <- pop$Pop[pop$Year == last.y]
t <- seq(0,25,1)

N25.fiver <- PopProj(N0=N0, r = r.fiver, t=t)
N25.tenr <- PopProj(N0=N0, r = r.tenr, t=t)


## plotting the population
plot(pop$Year[pop$Year >=2000], pop$Pop[pop$Year >=2000], t="n",xlab="Year", ylab="population size", 
     ylim=range(min(pop$Pop[pop$Year >=2000]), max(N25.fiver,N25.tenr)),
     xlim=range(2000,2019+25))
grid()
lines(pop$Year[pop$Year >=2000], pop$Pop[pop$Year >=2000],col=1,lwd=2)
lines(seq(2019,2019+25,1),N25.fiver,col=2, lwd=2)
lines(seq(2019,2019+25,1),N25.tenr,col=3,lwd=2,lty=2)
legend("bottomright",c("Current Pop", "Projection 5-y rate", "Projection 10-y rate"),col=1:3,lwd=2,lty=c(1,2,3))


# Ex 1.3: Make an assumption about one of the demographic components, and compare this projection with the base line

# We give arbitrary values to the demographic components (high fertility), making the growth rate 20% higher than calculated for 5-year period
N0 <- pop$Pop[pop$Year == last.y]
b_a <- 0.0035
d_a <- 0.0016
m_a <- 0.001
t <- seq(0,25,1)
PopProj <- function(N0,b,d,m,t){
  r <- b - d + m
  NT <- N0*exp(r*t)
  return(NT)
}

N25_sim <- PopProj(N0, b=b_a, d=d_a, m=m_a, t=t)


## plotting
plot(pop$Year[pop$Year >=2000], pop$Pop[pop$Year >=2000], t="n",xlab="Year", ylab="population size", 
     ylim=range(min(pop$Pop[pop$Year >=2000]), max(N25.fiver,N25.tenr)),
     xlim=range(2000,2019+25))
grid()
lines(pop$Year[pop$Year >=2000], pop$Pop[pop$Year >=2000],col=1,lwd=2)
lines(seq(2019,2019+25,1),N25.fiver,col=2, lwd=2)
lines(seq(2019,2019+25,1),N25.tenr,col=3,lwd=2,lty=2)
lines(seq(2019,2019+25,1),N25_sim,col=4,lwd=2,lty=2)
legend("bottomright",c("Current Pop", "Projection 5-y rate", "Projection 10-y rate", "High Fertility Finland"),
       col=1:4,lwd=2,lty=c(1,2,3,4))

# A higher fertility, making the growth rate 20% higher is not enough to account for the loss of growth in the last 5 year period.




# Ex 1.4: Assume that your observed data end 10 years before the last available data point. Project the population 10-year ahead
# with the 2 growth rates estimated in point 1. Which of the 2 projections is more accurate?

N0 <- pop$Pop[pop$Year == last.y - 10]
t <- seq(0,10,1)

N10.fiver <- PopProj(N0=N0, r = r.fiver, t=t)
N10.tenr <- PopProj(N0=N0, r = r.tenr, t=t)


## plotting the population
plot(pop$Year[pop$Year >=2000], pop$Pop[pop$Year >=2000],xlab="Year", ylab="population size", 
     ylim=range(min(pop$Pop[pop$Year >=2000]), max(N10.fiver, N10.tenr)),
     type = "l", col=1, lwd=2, lty = 1)
grid()
lines(seq(2009,2019,1),N10.fiver,col=2, lwd=2)
lines(seq(2009,2019,1),N10.tenr,col=3,lwd=2,lty=2)
legend("bottomright", c("Current Pop", "Projection 5-y rate", "Projection 10-y rate"),col=1:3,lwd=2,lty=c(1,2,3))




