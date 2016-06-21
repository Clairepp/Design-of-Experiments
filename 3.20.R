library(stats)
x <- c(9,12,10,8,15,20,21,23,17,30,6,5,8,16,7)
y <- c(rep("1",5),rep("2",5),rep("3",5))
dat <- data.frame(x,y)
result <- aov(x~y,data = dat)
summary(result)
TukeyHSD(result,conf.level = 0.99)
str(TukeyHSD(result,conf.level = 0.99))
qtukey(0.99,3,12)

str(summary(result))
sd <- sqrt(16.9/5)
plot(function(x) dt.scaled(x, df = 12, mean = 13.8, sd = sd),4,28)


(9+12+10+8+15+6+5+8+16+7-2*(20+21+23+17+30))/sqrt(5*16.9*6)
qt(0.975,12)

qqnorm(result$residuals)
qqline(result$residuals)
plot(result$residuals~result$fitted.values)
plot(result$residuals~y)

mean(x)

