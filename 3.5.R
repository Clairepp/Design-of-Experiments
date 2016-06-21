library(stats)
library(metRology)
x <- c(3129,3000,2865,2890,3200,3300,2975,3150,2800,2900,2985,3050,2600,2700,2600,2765)
y <- c(rep("1",4),rep("2",4),rep("3",4),rep("4",4))
dat <- data.frame(x,y)
result <- aov(x~y,data = dat)
summary(result)
str(summary(result))


x1 <- c(3129,3000,2865,2890)
x2 <- c(3200,3300,2975,3150)
x3 <- c(2800,2900,2985,3050)
x4 <- c(2600,2700,2600,2765)
xsum1 <- sum(x1)
xsum2 <- sum(x2)
xsum3 <- sum(x3)
xsum4 <- sum(x4)
xsum <- sum(x1,x2,x3,x4)

sst <- sum(x1^2+x2^2+x3^2+x4^2)-xsum^2/16
sstreatment <- sum(xsum1^2+xsum2^2+xsum3^2+xsum4^2)/4-xsum^2/16
sse <- sst -sstreatment
mst <- sstreatment/3
mse <- sse/12
f <- mst/mse

qf(0.05,3,12)
1-pf(f,3,12)

par(mfrow=c(1,1))
sd <- sqrt(12826/4)
mean <- mean(x)
plot(function(x) dt.scaled(x, df = 12, mean = mean, sd = sd),2600,3400)


abs(qt(0.025,12))*sqrt(2*12826/4)
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

plot(x)
3.77*sqrt(mse/4)


qqnorm(result$residuals)
qqline(result$residuals)
plot(result$residuals~result$fitted.values)
par(mfrow=c(2,2))
plot(result)


TukeyHSD(result,conf.level = 0.95)

