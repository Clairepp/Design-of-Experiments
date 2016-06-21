y <- c(79,94,46,92,85,76,88,53,46,36,40,62,62,74,57,99,79,68,75,56,57,53,56,47)
machine <- c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(1,3),rep(2,3),rep(3,3),rep(4,3))
machine <- factor(machine)
operator <- c(rep(c(1,2,3),8))
operator <- factor(operator)

anova(lm<-lm(y~machine/operator+machine)) 

