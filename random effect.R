library(Matrix)
library(lattice)



#12.9

Measure<-read.table(file="Measure.txt",header=T)

measure<-Measure[,3]

part<-Measure[,1]

Operator<-Measure[,2]


Operator<-factor(Operator)
part<-factor(part)


Y.lm1<-lm(measure~part)
anova(Y.lm1)


library(lme4)


Y.lmer1<-lmer(measure~1+(1|part)) ###part is a random factor
summary(Y.lmer1)


Y.lmer2<-lmer(measure~1+(1|part),REML=F) ##Use maximum likelihood instead for REML
summary(Y.lmer2)



library(nlme)

Y.nlme<-lme(measure~1,random=~1|part)

summary(Y.nlme)




##################################

anova(lm(measure~factor(Operator)*factor(part)))



fm1<-lmer(measure~(1|Operator)*(1|part))

summary(fm1)


fm1<-lmer(measure~(1|Operator)+(1|part)+(1|part:Operator))

summary(fm1)

anova(fm1)


fm1<-lmer(measure~part+(1|Operator)+(1|part:Operator)) ###Use lmer to fit the model with unrestricted model assumptions by default


summary(fm1)


anova(fm1)




############Nested  Design

Nest.lmer<-lmer(measure~1+(1|part/Operator))  ###Operator nested in part $$ both part and Operator are random factors

summary(Nest.lmer)



Nest.lmer<-lmer(measure~1+(1|part)+(1|part:Operator))  ###Operator nested in part $$ both part and Operator are random factors

summary(Nest.lmer)



Nest.lm<-lm(measure~ part/Operator)

anova(Nest.lm)



Nest.lmer<-lmer(measure~1+part+(1|part:Operator))  ###Operator nested in part $$  part fixed and Operator are random factors

summary(Nest.lmer)

anova(Nest.lmer)



###############################
Score<-read.table(file="Score.txt",header=T)

score<-Score[,4]
Time<-factor(Score[,1])
Operator<-factor(Score[,2])
Temp<-factor(Score[,3])

score.lmer<-lmer(score~Temp+(1|Time/Operator)+(1|Temp:Time)+(1|Temp:Time:Operator))

summary(score.lmer)

anova(score.lmer)


score.lm<-lm(score~Temp*Time +Temp/Time*Operator)

anova(score.lm)


score.lm<-lm(score~Temp+ Temp/Time +Temp*Time/Operator)

anova(score.lm)


aov(score.lm)


score.lm<-lm(score~Temp+ Temp/Time +Temp:Time/Operator)

anova(score.lm)



score.lm<-lm(score~Temp+ Time +Temp:Time/Operator)

anova(score.lm)
