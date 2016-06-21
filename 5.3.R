x <- matrix(c(90.4,90.7,90.2,90.2,90.6,90.4,90.1,90.5,89.9,90.3,90.6,90.1,90.5,90.8,90.4,90.7,90.9,90.1),byrow = T,ncol = 3)
r <- c(t(x))
f1 <- c("200","215","230")
f2 <- c("150","160","170")
k1 <- length(f1)
k2 <- length(f2)
n <- 2
tm1 <- gl(k1,1,n*k1*k2,factor(f1)) 
tm2 <- gl(k2,n*k1,n*k1*k2,factor(f2))
av <- aov(r~tm1*tm2)
summary(av)
qqnorm(av$residuals)
qqline(av$residuals)

plot(av$residuals,av$fitted.values)
plot(av$residuals)

interaction.plot(tm1,tm2,r,type = "b",col = c(2,4,6),main = "Interaction Graph")

TukeyHSD(av)

# 5.11
t1 <- c(rep(c(200,215,230),6))
t2 <- c(rep(150,6),rep(160,6),rep(170,6))
t3 <- t1*t1
t4 <- t2*t2
rt <- lm(r~t1+t2+t3+t4+t1:t2)
summary(rt)

model = function(t1,t2){
  48.5462963 + 0.8675926*t1-0.6404167*t2-0.0018148*t1^2+0.0024167*t2^2-0.0005833*t1*t2
}
t1 <- 200:230
t2 <- 150:170
w <- outer(t1,t2,model)
contour(t1,t2,w)
persp(t1,t2,w,theta = 5, phi = 29, ticktype = "simple")
