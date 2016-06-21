x <- matrix(c(196.6,197.7,199.8,196,196,199.4,198.5,196,198.4,197.2,196.9,197.6,197.5,195.6,197.4,196.6,196.2,198.1,198.4,199.6,200.6,198.6,200.4,200.9,197.5,198.7,199.6,198.1,198,199,197.6,197,198.5,198.4,197.8,199.8),byrow = T,ncol = 3)
r <- c(t(x))
f1 <- c("400","500","650")
f2 <- c("2","4","8")
f3 <-  c("3","4")
k1 <- length(f1)
k2 <- length(f2)
k3 <- length(f3)
n <- 4
tm1 <- gl(k1,1,n*k1*k2,factor(f1)) 
tm2 <- gl(k2,n/2*k1,n*k1*k2,factor(f2))
tm3 <- gl(k3,k1*k2*2,n*k1*k2,factor(f3))
av <- aov(r~tm1*tm2*tm3)
summary(av)

qqnorm(av$residuals)
qqline(av$residuals)

plot(av$residuals,av$fitted.values)
plot(av$residuals)

interaction.plot(tm1,tm2,r,type = "b",col = c(2,4,6),main = "Interaction Graph")
interaction.plot(tm3,tm2,r,type = "b",col = c(2,4,6),main = "Interaction Graph")
interaction.plot(tm3,tm1,r,type = "b",col = c(2,4,6),main = "Interaction Graph")
