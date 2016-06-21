x <- c(22,31,25,32,43,29,35,34,50,55,47,46,44,45,38,40,37,36,60,50,54,39,41,47)
a <- c(rep("-",3),rep("+",3),rep("-",3),rep("+",3),rep("-",3),rep("+",3),rep("-",3),rep("+",3))
b <- c(rep("-",6),rep("+",6),rep("-",6),rep("+",6))
c <- c(rep("-",12),rep("+",12))
av <- aov(x~a*b*c)
summary(av)
rt <- lm(x~a*b*c)
summary(rt)

qqnorm(av$residuals)
qqline(av$residuals)

plot(av$residuals,av$fitted.values)
plot(av$residuals)

interaction.plot(a,b,x,type = "b",col = c(2,4,6),main = "Interaction Graph")
interaction.plot(b,c,x,type = "b",col = c(2,4,6),main = "Interaction Graph")
interaction.plot(c,a,x,type = "b",col = c(2,4,6),main = "Interaction Graph")

# 6.2
model = function(a,b,c){
  26.000+8.667*a+13.667*b+16.333*c-13.333*a*c
}
a <- 0:1
b <- 0:1
c <- 0:1

contour(a,b,model)
