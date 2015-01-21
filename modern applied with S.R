#################################################################
# ��������ת����������
#################################################################
factorTest <- factor(c('just so so ','good', 'better', 'better', 'good', 'best', 'good', 'better'))
factorTest
factorLable <- ordered(factorTest, labels = c(">90", '80-90', '70-80', '<70'))
factorLable
?ordered

search()
objects()
rm(list = ls())
find("objects")


x <- rt(250, df = 9)
par(pty = "s")
qqnorm(x)
qqline(x)


# ����������ں��ֲַ�����ʹ��MASS���е�fitdistr()
library(MASS)
x <- rgamma(100, shape = 5, rate = 0.1)
fitdistr(x, 'gamma')

x2 <- rt(250, df = 9)
fitdistr(x2, "t", df = 9)

fitdistr(x2, 't')



data(shoes)
head(shoes)
shoes





#################################################
#Э�������ʵ��
#################################################
library(MASS)
whiteside
str(whiteside)

?xyplot
xyplot(Gas ~ Temp | Insul, whiteside, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, ...)
}, xlab = "Average external temperature(deg.C)", 
ylab = "Gas consumption(1000 cubic feet)",
aspect = "xy",
strip = function(...) strip.default(..., style = 1)
)

?lm
##############################
#����Insul����"Before"��ģ��
##############################
gasB <- lm(Gas ~ Temp, data = whiteside, subset = Insul == 'Before')
##############################
#update���ڸ���ģ��
##############################
gasA <- update(gasB, subset = Insul == 'After')

############################################################
#fitted model objects
#��ģ�͵ĸ���һ������������
# print�� �򵥵�display
# summary�� ����output
# coef��coefficient������ȡ��regression coefficeints
# resid��residuals�����в�
# fitted��fitted.values�������ֵ
# deviance: redisual sum of squares
# anova���������
# predict���������ݵ�means��standard errors��Ԥ��
# plot�� ���ͼ
############################################################

summary(gasB)

summary(gasA)

########################################
# �����������
########################################
gasB
summary(gasB)
deviance(gasB)
gasB$df.resid
varB <- deviance(gasB)/gasB$df.resid  #ֱ�Ӽ���
varB

varB1 <- summary(gasB)$sigma^2  #�ɹ�ѡ��ķ�������ͬ��ֱ�Ӽ���
varB1


#####################################################################################################
#������ģ�����嵽һ������ȥ
# a/x����������ǣ�a��һ��factor���ڲ�ͬa��ˮƽ��x��Ӧ��ģ��
# ������ Insul/Temp - 1��˼���ڲ�ͬInsulˮƽ�ϣ���Ӧ��Tempģ�ͣ�- 1��ʾʡ�Ե��ؾ�
#####################################################################################################
gasBA <- lm(Gas ~ Insul/Temp - 1, data = whiteside)
summary(gasBA)

##############################
# �������
# 'identity' function I(...)
##############################
gasQ <- lm(Gas ~ Insul/(Temp + I(Temp^2)) - 1, data = whiteside)
summary(gasQ)
summary(gasQ)$coef

##########################################################
# parallel regression
# R: options(contrasts = c("contr.helmert", "contr.poly"))
##########################################################
gasPR <- lm(Gas ~ Insul + Temp, data = whiteside)
gasPR
anova(gasPR, gasBA) #�������


#####################################
# ʹ�ò�ͬ�Ĳ������ֿ�slopes�����ģ��
#####################################
options(contrasts = c("contr.treatment", "contr.poly"))
gasBA1 <- lm(Gas ~ Insul*Temp, data = whiteside)
summary(gasBA1)$coef






#################################################### 
# �ع����ʵ�� 
####################################################
?options
library(MASS)
head(hills)
str(hills)
(hills.lm <- lm(time ~ dist + climb, data = hills))

?frame
frame()  # create/start a new plot frame
par(fig = c(0, 0.6, 0, 1))
??fig
plot(fitted(hills.lm), studres(hills.lm))
abline(h = 0, lty = 2)
identify(fitted(hills.lm), studres(hills.lm), row.names(hills))
par(fig = c(0.6, 1, 0, 1), pty = "s", new = TRUE)
qqnorm(studres(hills.lm))
qqline(studres(hills.lm))
hills.hat <- lm.influence(hills.lm)$hat
cbind(hills, lev = hills.hat)[hills.hat > 3/35, ]

#�鿴Knock Hill��Ԥ��ֵ
cbind(hills, pred = predict(hills.lm))["Knock Hill", ]

(hills1.lm <- update(hills.lm, subset = -18))
#######################################################
# ���Կ����߳�Knock Hill��ģ�Ͳ���û�нϴ�Ӱ��
# ���Ǽ��һ���������������
#######################################################
update(hills.lm, subset = - c(7, 18))
summary(hills1.lm)

summary(update(hills1.lm, weights = 1/dist^2))

lm(time ~ -1 + dist + climb, hills[-18, ], weights = 1/dist^2)

hillsNew <- hills
head(hillsNew)
hillsNew$ispeed <- hillsNew$time/hillsNew$dist
hillsNew$grad <- hillsNew$climb/hillsNew$dist
(hills2.lm <- lm(ispeed ~ grad, data = hillsNew[-18, ]))
frame()
par(fig = c(0, 0.6, 0, 1))
plot(hillsNew$grad[-18], studres(hills2.lm), xlab = 'grad')
abline(h = 0, lty = 2)
identify(hillsNew$grad[-18], studres(hills2.lm), row.names(hillsNew)[-18])
par(fig = c(0.6, 1, 0, 1), pty = 's', new = TRUE)
qqnorm(studres(hills2.lm))
qqline(studres(hills2.lm))
hills2.hat <- lm.influence(hills2.lm)$hat
cbind(hillsNew[-18, ], lev = hills2.hat)[hills2.hat > 1.8*2/34, ]




N <- factor(Nlevs <- c(0, 1, 2, 4))
contrasts(N)
contrasts(ordered(N))


###########################
# safe prediction
# predict method
###########################
library(MASS)
head
head(wtloss)
quad1 <- lm(Weight ~ Days + I(Days^2), data = wtloss)
quad2 <- lm(Weight ~ poly(Days, 2), data = wtloss)
quad1
quad2

new.x <- data.frame(Days = seq(250, 300, 10), row.names = seq(250, 300, 10))
predict(quad1, newdata = new.x)
predict(quad2, newdata = new.x)

library(MASS)
head(anorexia)
str(anorexia)

options(contrasts = c("contr.treatment", "contr.poly"))
ldose <- rep(0:5, 2)
ldose
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
numdead
sex <- factor(rep(c("M", "F"), c(6, 6)))
sex
SF <- cbind(numdead, numalive = 20 - numdead)
SF
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
summary(budworm.lg, cor = F)

plot(c(1, 32), c(0, 1), type = 'n', xlab = 'dose', ylab = 'prob', log = 'x')
text(2^ldose, numdead/20, labels = as.character(sex))
ld <- seq(0, 5, 0.1)
lines(2^ld, predict(budworm.lg, data.frame(ldose = ld, sex = factor(rep("M", length(ld)), levels = levels(sex))), type = "response", col = 3))
lines(2^ld, predict(budworm.lg, data.frame(ldose = ld, sex = factor(rep("F", length(ld)), levels = levels(sex))), type = "response"), lty = 2, col = 2)

budworm.lgA <- update(budworm.lg, . ~ sex * I(ldose - 3))
summary(budworm.lgA, cor = F)$coefficients

anova(update(budworm.lg, . ~ . + sex * I(ldose^2)), test = "Chisq")


########################################################
# a binary data example: Low birth weight in infants
########################################################
library(MASS)
head(birthwt)
tail(birthwt)
str(birthwt)

options(contrasts = c("contr.treatment", "contr.poly"))
attach(birthwt)
race <- factor(race, labels = c("white", "black", "other"))
race
table(ptl)
ptd <- factor(ptl > 0)
ptd
table(ftv)
ftv <- factor(ftv)
ftv
levels(ftv)[-(1:2)] <- "2+"
ftv
table(ftv)
bwt <- data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0), ptd , ht = (ht > 0), ui = (ui >  0), ftv)
head(bwt)
detach()
rm(race, ptd, ftv)
birthwt.glm <- glm(low ~ ., family = binomial, data = bwt)
summary(birthwt.glm, cor = F)

birthwt.step <- stepAIC(birthwt.glm, trace = F)
summary(birthwt.step)
birthwt.step$anova 

birthwt.step2 <- stepAIC(birthwt.glm, ~ .^2 + I(scale(age)^2) + I(scale(lwt)^2), trace = F)
birthwt.step2$anova

summary(birthwt.step2, cor = F)$coef
table(bwt$low, predict(birthwt.step2) > 0)

housing
wtloss
??MASS







#######################################################
# Surival Analysis
#######################################################
install.packages("survival")
library(survival)
library(MASS)
#################################################
# leuk���ݼ���δ����leukaemia�Ĵ��ʱ��
# ����accelerated life test ʵ��Ľ������
#################################################
head(leuk)
head(motors)

#####################################################################################
#��������Ļ�������
#����ʱ��һ��ͨ������ռ�

# ��β��������������
# �Ƿ�����;���˵�����

#����ʱ�� �����˴ӽ���۲���õ��涨�Ľ�ֳ��֣������ʱ��
#��ȫ���ݣ�complete data�����������������������ʱ�䣬�����ߵĴ��ʱ��
#ɾʧ���ݣ�censored data��������ʧ�á��ı����Ʒ������о����������������ʹ�ò��ֲ��˲���
#������������������������������õ��ף���Ϊɾʧ����

# ��������
# �����ܶȺ���

# ������
# ���պ���


################################
# ��������о�����Ҫ����
################################
#����������̣��о�����ʱ��ķֲ��ص㣬���������ʼ�ƽ������ʱ�䣬�����������ߣ����Ƴ���ʱ��
����������������# �������ʼ���λ����ʱ�䣬Ҳ�ɷ����������ص㡡
#�Ƚ�������̣���ͨ�������ʼ����׼��Ը������������ʽ��бȽϣ���̽�ָ�������������Ƿ��в��
#Ӱ������ʱ������ط�����ͨ���������ģ����̽��Ӱ������ʱ������أ�ͨ��������ʱ��ͽ����Ϊ���������
#������Ӱ�����ǵ�������Ϊ�Ա����������䡢�Ա𡢲������͡����Ʒ�ʽ�ȣ�ͨ�����ģ�ͣ�ɸѡ��Ӱ������ʱ
#  ��ı������غ��к�����


######################################################
# ��������Ļ�������
######################################################
#���ǲ�������������������ʲô���ķֲ���ʽ��ֻ���������ṩ��˳��ͳ�����������ʽ��й��ƣ�
# ���õķ����г˻����޷�����������

#  �����������ٶ�����ʱ������ض��Ĳ����ֲ���������֪�ֲ����ص��Ӱ�������ʱ�����
# ���������÷�����ָ���ֲ�����Weibull�ֲ�����������̬�ع��������

# ������������в����ͷǲ������ص㣬��Ҫ���ڷ���Ӱ������ʱ��������ʵ����أ��������ط���������
# ���͵ķ���������Coxģ�ͷ�������

########################################
# �����ʵĹ�������������
########################################
#���������������Ĵ�С�ɷֱ�ѡ��
# �˻����޷���Kaplan-Meier����С����ʱ
# ����������life table����������ʱ



###########################################################################


###########################################################################
# Surv(times, status)��������uncensored data��ͨ��������model����� 
###########################################################################
?Surv
str(leuk)
?survfit
####################################
# survfit����һ��survival curves
####################################
plot(survfit(Surv(time) ~ ag, data = leuk), lty = 2:3, col = 2:3)
legend(110, 0.8, c("ag absent", "ag present"), lty = 2:3, col = 2:3)



###########################################################################
# survfitҲ�����ڴ���censored data(ɾʧ����) 
###########################################################################
head(gehan)
str(gehan)
attach(gehan)
Surv(time, cens)
?Surv
time
cens
plot(log(time) ~ pair)
gehan.surv <- survfit(Surv(time, cens) ~ treat, data = gehan, conf.type = "log-log")
summary(gehan.surv)
plot(gehan.surv, conf.int = T, lty = 3:2, log = T, xlab = "time of remission(weeks)", ylab = "survival")
lines(genhan.surv, lty = 3:2, lwd = 2, cex = 2)
legend(25, 0.1, c("control", "6-MP"), lty = 2:3, lwd = 2)

#########################################################
# Testing survivor curves
# �ɼ��groups��Ĳ�����
#########################################################
survdiff(Surv(time, cens) ~ treat, data = gehan)
survdiff(Surv(time) ~ ag, data = leuk)

#################################################
# Parametric Models
#������exploratory Models
#################################################
plot(gehan.surv, lty = 3:4, col = 2:3, fun = "cloglog", xlab = "time of remission(weeks)", ylab = "log H(t)")
legend(2, 0.5, c("control", "6-MP"), lty = 4:3, col = 3:2)

##############################################
# Weibull and log-logistic regression analyses
##############################################
options(contrasts = c("contr.treatment", "contr.poly"))
survreg(Surv(time) ~ ag*log(wbc), data = leuk, dist = "exponential")
summary(survreg(Surv(time) ~ ag + log(wbc), data = leuk, dist = "exponential"))
summary(survreg(Surv(time) ~ ag + log(wbc), data = leuk))
summary(survreg(Surv(time) ~ ag + log(wbc), data = leuk, dist = "loglogistic"))


#################################################################
# �ɼ����groups�е�difference ��ʹ��Weibull model�е�Wald test
# ��ʹ��anova���������� likelihood ration
#################################################################
anova(survreg(Surv(time) ~ log(wbc), data = leuk), survreg(Surv(time) ~ ag + log(wbc), data = leuk))

summary(survreg(time) ~ strata(ag) + log(wbc), data = leuk)


survreg(Surv(time, cens) ~ factor(pair) + treat, data = gehan, dist = "exponential")
summary(survreg(Surv(time, cens) ~ treat, data = gehan, dist = "exponential"))
summary(survreg(Surv(time, cens) ~ treat, data = gehan))

plot(survfit(Surv(time, cens) ~ factor(temp), data = motors), conf.int = F)
motor.wei <- survreg(Surv(time, cens) ~ temp, data = motors)
summary(motor.wei)

predict(motor.wei, data.frame(temp = 130), type = "quantile", p = c(0.5, 0.1))
t1 <- predict(motor.wei, data.frame(temp = 130), type = "uquantile", p = 0.5, se = T)
exp(c(LL = t1$fit - 2*t1$se, UL = t1$fit + 2*t1$se))
t1 <- predict(motor.wei, data.frame(temp = 130), type = "uquantile", p = 0.1, se = T)
exp(c(LL = t1$fit - 2*t1$se, UL = t1$fit + 2*t1$se))






#####################################################
# time series
#####################################################
library(MASS)
library(TSA)
install.packages("stratification")
library(stratification)
########################################
# lh��deaths���ݼ�
########################################
lh
deaths


tspar(deaths)  
str(deaths)
start(deaths)
end(deaths)
frequency(deaths)
cycle(deaths)

par(mfrow = c(2, 2))
ts.plot(lh)
ts.plot(deaths, mdeaths, fdeaths, lty = c(1, 3, 4), xlab = 'year', ylab = 'deaths')

par(mfrow = c(2, 2))
spectrum(lh)
spectrum(deaths)

####################################################
# Spatial Statistics
####################################################
library(spatial)
??spatial
topo





































