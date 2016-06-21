library(lme4)
y <- c(109,110,108,110,110,115,109,108,110,110,111,114,112,111,109,112,116,112,114,120,114,115,119,117)
operator <- c(rep(1,8),rep(2,8),rep(3,8))
operator <- as.factor(operator)
machine <- c(rep(c(1,2,3,4),6))
machine <- as.factor(machine)
y.lmer<-lmer(y~1+(1|operator)+(1|machine)) 
summary(y.lmer)
y.aov <- aov(y ~ Error(operator*machine))
summary(y.aov)
1-pchisq(357.13,1,2)
(13889- 38.89)/6
(405.6- 38.89)/9