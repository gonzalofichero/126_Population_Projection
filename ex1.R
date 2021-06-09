# Loading libraries
library(tidyverse)
library(HMDHFDplus)
library(data.table)

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






#######################
# Exercise 2


# Ex 2.1


# Let's start with:
# We take the mid-year population for the 5-age group population of Finland
ex2.date <- max(finland$Year) - 5

finland.pop <- readHMDweb( CNTRY = "FIN", item="Population5",
                       username = user.n, password = user.pass)

finland.pop.2014 <- finland.pop %>% 
                    mutate(Nfx = (Female1+Female2)/2,
                           Nmx = (Male1+Male2)/2) %>% 
                    filter(Year == ex2.date) %>% 
                    select(Year, Age,Nfx,Nmx)


# We need to group the 0 and 1 age group to create 0-4 age group
finland.pop.2014$Age_groups <- finland.pop.2014$Age
finland.pop.2014$Age_groups[finland.pop.2014$Age==0 | finland.pop.2014$Age==1] <- 0

finland.pop.2014.v2 <- finland.pop.2014  %>% 
                    group_by(Age_groups) %>% 
                      summarise(Nfx.sum = sum(Nfx, na.rm = T),
                                Nmx.sum = sum(Nmx, na.rm = T))


# Male Life table to get Lx
Male_LT <- readHMDweb( CNTRY = "FIN", item="mltper_5x1",
                       username = user.n, password = user.pass)

Male_LT <- Male_LT %>% 
            select(Year, Age, Lx) %>% 
            filter(Year == ex2.date)


# We need to group the 0 and 1 age group to create 0-4 age group
Male_LT$Age_groups <- Male_LT$Age
Male_LT$Age_groups[Male_LT$Age==0 | Male_LT$Age==1] <- 0

Male_LT <- Male_LT  %>% group_by(Age_groups) %>% 
                        summarise(Lx = sum(Lx, na.rm = T))


# Female Life table to get Lx
Female_LT <- readHMDweb( CNTRY = "FIN", item="fltper_5x1",
                       username = user.n, password = user.pass)

Female_LT <- Female_LT %>% 
  select(Year, Age, Lx) %>% 
  filter(Year == ex2.date)


# We need to group the 0 and 1 age group to create 0-4 age group
Female_LT$Age_groups <- Female_LT$Age
Female_LT$Age_groups[Female_LT$Age==0 | Female_LT$Age==1] <- 0

Female_LT <- Female_LT  %>% group_by(Age_groups) %>% 
  summarise(Lx = sum(Lx, na.rm = T))



# Downloading Age-specific fertility rates for Finland

fertil <- readHFDweb(CNTRY= "FIN",item="asfrRR",
                     username= user.n,
                     password= user.pass,
                     fixup=TRUE)
glimpse(fertil)

Births <- fertil %>%  filter(Year == ex2.date)

# Age groups
Births$Age2 <- 12:55
Births$Age_groups[Births$Age2>=12 & Births$Age2<=14] <- 1
Births$Age_groups[Births$Age2>=15 & Births$Age2<=19] <- 2
Births$Age_groups[Births$Age2>=20 & Births$Age2<=24] <- 3
Births$Age_groups[Births$Age2>=25 & Births$Age2<=29] <- 4
Births$Age_groups[Births$Age2>=30 & Births$Age2<=34] <- 5
Births$Age_groups[Births$Age2>=35 & Births$Age2<=39] <- 6
Births$Age_groups[Births$Age2>=40 & Births$Age2<=44] <- 7
Births$Age_groups[Births$Age2>=45 & Births$Age2<=49] <- 8
Births$Age_groups[Births$Age2>=50 & Births$Age2<=55] <- 9


ASFR <- Births  %>% group_by(Age_groups) %>% 
                    summarise(ASFR.sum = sum(ASFR, na.rm = T))

ASFR$ASFR.sum <- ASFR$ASFR.sum/5

Fx <- c(0,0, ASFR$ASFR.sum, 0,0,0,0,0,0,0,0,0,0,0,0)



# Now getting everything together:

finland.2014 <- data.table( Age = Male_LT$Age_groups,
                            NFx = finland.pop.2014.v2$Nfx.sum,
                            NMx = finland.pop.2014.v2$Nmx.sum,
                            LMx = Male_LT$Lx,
                            LFx = Female_LT$Lx,
                            Fx = Fx)

# Creating the Tx's for each sex
finland.2014$TMx <- rev(cumsum(rev(finland.2014$LMx)))
finland.2014$TFx <- rev(cumsum(rev(finland.2014$LFx)))

#####################
# Females...
# Time to calculate sFx
finland.2014$sFx_5 <- NA
finland.2014$NFxt_5 <- NA
for(i in 1:22){
  finland.2014$sFx_5[i] <- finland.2014$LFx[i+1]/finland.2014$LFx[i]
}
for(i in 2:23){
  finland.2014$NFxt_5[i] <- finland.2014$NFx[i-1] * finland.2014$sFx_5[i-1]
}


# The population in the last age group is not correct
# Adjusting the last age group
finland.2014$sFx_5[finland.2014$Age==105] <- finland.2014$TFx[finland.2014$Age==110]/finland.2014$TFx[finland.2014$Age==105]
finland.2014$NFxt_5[finland.2014$Age==110] <- (finland.2014$NFx[finland.2014$Age==110] + finland.2014$NFx[finland.2014$Age==105]) * finland.2014$sFx_5[finland.2014$Age==105]

finland.2014$bFx <- NA
# Gonna use standard birth ratio... don't wanna look for the real one
factor.srb <- 1/(1 + 1.05)
Fradix <- finland.2014$LFx[finland.2014$Age==0]/(2*100000)


# Estimate the first age group
for(i in 1:23){
  finland.2014$bFx[i] <- factor.srb * (Fradix * (finland.2014$Fx[i]+finland.2014$sFx[i]*finland.2014$Fx[i+1]))
}
finland.2014$NFxt_5[finland.2014$Age==0] <- sum(finland.2014$NFx * finland.2014$bFx, na.rm=T)


#####################
# Males
# Time to calculate sMx and do all same stuff as for Females
finland.2014$sMx_5 <- NA
finland.2014$NMxt_5 <- NA
for(i in 1:22){
  finland.2014$sMx_5[i] <- finland.2014$LMx[i+1]/finland.2014$LMx[i]
}
for(i in 2:23){
  finland.2014$NMxt_5[i] <- finland.2014$NMx[i-1] * finland.2014$sMx_5[i-1]
}

# The population in the last age group is not correct
# Adjusting the last age group
finland.2014$sMx_5[finland.2014$Age==105] <- finland.2014$TMx[finland.2014$Age==110]/ finland.2014$TMx[finland.2014$Age==105]
finland.2014$NMxt_5[finland.2014$Age==110] <- (finland.2014$NMx[finland.2014$Age==110] + finland.2014$NMx[finland.2014$Age==105]) * finland.2014$sMx_5[finland.2014$Age==105]

finland.2014$bMx <- NA
factor.srb.M <- 1.05/(1 + 1.05)
Fradix <- finland.2014$LMx[finland.2014$Age==0]/(2*100000)

# Estimate the first age group
for(i in 1:23){
  finland.2014$bMx[i] <- factor.srb.M * (Fradix * (finland.2014$Fx[i]+finland.2014$sMx[i]*finland.2014$Fx[i+1]))
}
finland.2014$NMxt_5[finland.2014$Age==0] <- sum(finland.2014$NMx * finland.2014$bMx, na.rm=T)


# Everything together, again:
finland.2014.long <- finland.2014 %>%
                      select(Age,NFx,NFxt_5) %>%
                      rename("2014"=NFx, "2019"=NFxt_5) %>%
                      pivot_longer(-Age,names_to = "Year",values_to = "Population")



###########################################################
# Plot Projection vs Real 2019 Population (by sex)
finland.pop.2019 <- finland.pop %>% 
                    mutate(Nfx = (Female1+Female2)/2,
                           Nmx = (Male1+Male2)/2) %>% 
                    filter(Year == 2019) %>% 
                    select(Year, Age, Nfx, Nmx)

# We need to group the 0 and 1 age group to create 0-4 age group
finland.pop.2019$Age_groups <- finland.pop.2019$Age
finland.pop.2019$Age_groups[finland.pop.2019$Age==0 | finland.pop.2019$Age==1] <- 0

finland.pop.2019 <- finland.pop.2019  %>% 
                        group_by(Age_groups) %>% 
                        summarise(Nfx = sum(Nfx, na.rm = T),
                                  Nmx = sum(Nmx, na.rm = T)) %>% 
                        rename(Age = Age_groups) %>% 
                        mutate(Origin = "Real 2019 population")


finland.compare <- finland.2014 %>% 
                    select(Age, NFxt_5, NMxt_5) %>% 
                    rename(Nfx = NFxt_5,
                           Nmx = NMxt_5) %>% 
                    mutate(Origin = "Projected 2019 population") %>% 
                    rbind(finland.pop.2019)

# Females
finland.compare %>% 
  ggplot(aes(x = Age, y= Nfx, fill=Origin)) +
    geom_area(position = "identity", alpha=0.3) +
    coord_flip() +
    theme_bw() +
    ggtitle("Finland female population: Real vs Projection") +
    scale_fill_manual(values=c("#E69F00", "#56B4E9"))

# Males + Females in 1 plot
finland.compare %>% 
  ggplot(aes(x = Age, y= Nfx, fill=Origin)) +
  geom_area(position = "identity", alpha=0.3) +
  geom_area(aes(x = Age, y= -Nmx, fill=Origin), position = "identity", alpha=0.3) +
  coord_flip() +
  theme_bw() +
  ggtitle("Finland male population: Real vs Projection") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))



