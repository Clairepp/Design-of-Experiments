x <- c(4.93,4.86,4.75,4.95,4.79,4.88,4.85,4.91,4.79,4.85,4.75,4.85,4.83,4.88,4.90,4.75,4.82,4.90,4.89,4.77,4.94,4.86,4.79,4.76)
tre <- c(rep("1",6),rep("2",6),rep("3",6),rep("4",6))
k <- 4
n <- 6
blk = gl(n, 1, k*n)
av <- aov(x ~ tre+blk)
summary(av)

qqnorm(av$residuals)
qqline(av$residuals)
plot(av$residuals~av$fitted.values,main = "Residuals vs. Predicted")

xsd <- c(0.05,0.04,0.05,0.06,0.03,0.05,0.04,0.02,0.03,0.05,0.03,0.02,0.09,0.13,0.11,0.15,0.08,0.12,0.03,0.04,0.05,0.05,0.03,0.02)
xsd <- log(xsd)
tre <- c(rep("1",6),rep("2",6),rep("3",6),rep("4",6))
k <- 4
n <- 6
blk = gl(n, 1, k*n)
av <- aov(xsd ~ tre+blk)
summary(av)
qqnorm(av$residuals)
qqline(av$residuals)
plot(av$residuals~av$fitted.values,main = "Residuals vs. Predicted")
plot(av$residuals~y,main = "Residuals vs. Treatment Factors")

TukeyHSD(av,conf.level = 0.95)
