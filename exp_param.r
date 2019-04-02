library(tidyr)
library(dplyr)
library(ggplot2)




data <- read.csv("ttz.csv", head=TRUE, dec = ",", sep=";")
x <- data$x
mpx1 <- data$mpx
par1 <- 0.01
kpx1 <- data$kpx
mse <- function(par1, v1, x){
  sum((v1 - exp(-x*par1))^2)
}
# mse(par1,x,mpx1)

min_m <- optimize(mse, c(0,1), tol = 0.00001, maximum = FALSE, x = x, v1 = mpx1)$minimum
min_k <- optimize(mse, c(0,1), tol = 0.00001, maximum = FALSE, x = x, v1 = kpx1)$minimum

mpx_est <- exp( -min_m *x)
kpx_est <- exp( -min_k *x)
est <- data.frame(mpx_est, kpx_est)

data <- cbind(data, est)

ggplot(data, aes(x)) +
  geom_line(aes(y = mpx, colour = "blue")) +
  geom_line(aes(y = mpx_est, colour = "dark green"))+
  xlab("time in year") +
  ylab("survival probablity")+
  scale_color_discrete(name = "Man", labels = c("real", "approx"))

ggplot(data, aes(x)) +
  geom_line(aes(y = kpx, colour = "blue")) +
  geom_line(aes(y = kpx_est, colour = "dark green"))+
  xlab("time in year") +
  ylab("survival probablity")+
  scale_color_discrete(name = "Woman", labels = c("real", "approx"))

u12 <- min_m
u13 <- min_k
u24 <- min_k
u34 <- min_m


p12 <- function(x,t){
  (1-exp(-u12*t))*exp(-u24*t)
}

p13 <- function(x,t){
  (1-exp(-u13*t))*exp(-u34*t)
}

p11 <- function(x,t){
  1 - p12(x,t) - p13(x,t)
}

p22 <- function(x,t){
  exp(-u24*t)
}
p24 <- function(x,t){
  1 - p22(x,t)
}
p33 <- function(x,t){
  exp(-u34*t)
}
p34 <- function(x,t){
  1 - p33(x,t)
}

y <- 30
t <- 1:60
plot_p12 <- ggplot(data.frame(t, p = p12(y,t)), aes(t,y)) +
  geom_line(aes(t,p))+
  xlab("time in year") +
  ylab("probablity")+
  ggtitle("P12")+
  theme(plot.title = element_text(hjust = 0.5))
 

plot_p13 <- ggplot(data.frame(t, p = p13(y,t)), aes(t,y)) +
  geom_line(aes(t,p))+
  xlab("time in year") +
  ylab("probablity")+
  ggtitle("P13")+
  theme(plot.title = element_text(hjust = 0.5))

plot_p11 <- ggplot(data.frame(t, p = p11(y,t)), aes(t,y)) +
  geom_line(aes(t,p))+
  xlab("time in year") +
  ylab("probablity")+
  ggtitle("P11")+
  theme(plot.title = element_text(hjust = 0.5))

plot_p22 <- ggplot(data.frame(t, p = p22(y,t)), aes(t,y)) +
  geom_line(aes(t,p))+
  xlab("time in year") +
  ylab("probablity")+
  ggtitle("P22")+
  theme(plot.title = element_text(hjust = 0.5))

plot_p24 <- ggplot(data.frame(t, p = p24(y,t)), aes(t,y)) +
  geom_line(aes(t,p))+
  xlab("time in year") +
  ylab("probablity")+
  ggtitle("P24")+
  theme(plot.title = element_text(hjust = 0.5))

plot_p33 <- ggplot(data.frame(t, p = p33(y,t)), aes(t,y)) +
  geom_line(aes(t,p))+
  xlab("time in year") +
  ylab("probablity")+
  ggtitle("P33")+
  theme(plot.title = element_text(hjust = 0.5))

plot_p34 <- ggplot(data.frame(t, p = p34(y,t)), aes(t,y)) +
  geom_line(aes(t,p))+
  xlab("time in year") +
  ylab("probablity")+
  ggtitle("P34")+
  theme(plot.title = element_text(hjust = 0.5))


par(mfrow = c(3,2))
plot_p12; plot_p13; plot_p11; plot_p22; plot_p24; plot_p33; plot_p34


grid.arrange(plot_p12, plot_p13, plot_p24, plot_p34, ncol=2)
grid.arrange(plot_p11, plot_p22, plot_p33)


AV_premiums <- function(x, i, p) #actuarial Value /// x - age, p - premium
{
  v = 1/(1+i)
  y <- x:100
  return(sum(v^(y-30) * p * p11(x,y-x)))
}

AV_premiums(x = 30, i = 0.02, p = 10)


AV_lump_sum<-function(x, i, c) #kiedy p³acone? przyjmuje, ¿e na koniec roku
{
  v = 1/(1+i)
  y <- 0:(100-(x+1))
  return(sum( v^(y+1)*(p24(x,1)*p34(x,1)*c )))
}

AV_lump_sum(x = 30, i =.02, c = 1000)

