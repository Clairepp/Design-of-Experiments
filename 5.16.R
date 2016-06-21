x <- matrix(c(9.6,11.28,9,9.69,10.1,9.57,8.43,11.01,9.03,9.98,10.44,9.8),byrow = T,ncol = 3)
r <- c(t(x))
f1 <- c("250","260","270")
f2 <- c("120","130","140","150")
k1 <- length(f1)
k2 <- length(f2)
n <- 1
tm1 <- gl(k1,1,n*k1*k2,factor(f1)) 
tm2 <- gl(k2,n*k1,n*k1*k2,factor(f2))
av <- aov(r~tm1+tm2)
summary(av)

qqnorm(av$residuals)
qqline(av$residuals)

plot(av$residuals,av$fitted.values)
plot(av$residuals)

interaction.plot(tm1,tm2,r,type = "b",col = c(2,4,6),main = "Interaction Graph")

