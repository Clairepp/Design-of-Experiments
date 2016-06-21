x <- c(3,5,3,7,6,5,3,2,1,6,1,3,4,7,5,6,3,2,1,7,4,1,3,5,7,1,2,4,2,7,3,5,7,5,10,3,4,7,2,7)
y <- c(rep("1",10),rep("2",10),rep("3",10),rep("4",10))
dat <- data.frame(x,y)
result <- aov(x~y,data = dat)
summary(result)


xsum1 <- sum(x1)
xsum2 <- sum(x2)
xsum3 <- sum(x3)
xsum4 <- sum(x4)
xsum <- sum(x1,x2,x3,x4)

sst <- sum(x1^2+x2^2+x3^2+x4^2)-xsum^2/40
sstreatment <- sum(xsum1^2+xsum2^2+xsum3^2+xsum4^2)/10-xsum^2/40
sse <- sst -sstreatment
mst <- sstreatment/3
mse <- sse/36
f <- mst/mse

qf(0.05,3,36)
1-pf(f,3,36)


xmean1 <- mean(x1)
xmean2 <- mean(x2)
xmean3 <- mean(x3)
xmean4 <- mean(x4)

x11 <- x1 - xmean1
x22 <- x2 - xmean2
x33 <- x3 - xmean3
x44 <- x4 - xmean4
x <- c(x11,x22,x33,x44)
qqnorm(x)
qqline(x)

qqnorm(result$residuals)
qqline(result$residuals)
plot(result$residuals~result$fitted.values)

x1 <- sqrt(x)
y <- c(rep("1",10),rep("2",10),rep("3",10),rep("4",10))
dat <- data.frame(x1,y)
result1 <- aov(x1~y,data = dat)
summary(result1)