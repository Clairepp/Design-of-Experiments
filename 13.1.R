library(lme4)
y <- c(14,14.1,14.2,14,14.1,13.9,13.8,13.9,14,14,14.1,14.2,14.1,14,13.9,13.6,13.8,14,13.9,13.7,13.8,13.6,13.9,13.8,14)
looms <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5))
looms <- as.factor(looms)
y.lmer1<-lmer(y~1+(1|looms)) 
summary(y.lmer1)
y.aov <- aov(y ~ Error(looms))
summary(y.aov)

qqnorm(residuals(y.lmer1))

pchisq(0.0854/0.0148,4,20)

qchisq(0.025,4,20)
1/qchisq(0.025,4,20)

(0.0854/(0.0148*qchisq(0.025,4,20))-1)/5
((0.0854*qchisq(0.025,4,20))/0.0148-1)/5

-0.06553329/(1-0.06553329)
9.704613/(1+9.704613)
