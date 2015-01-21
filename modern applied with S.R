#################################################################
# 将因素型转换重新命名
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


# 拟合数据属于何种分布，可使用MASS包中的fitdistr()
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
#协方差分析实例
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
#构建Insul等于"Before"的模型
##############################
gasB <- lm(Gas ~ Temp, data = whiteside, subset = Insul == 'Before')
##############################
#update用于更改模型
##############################
gasA <- update(gasB, subset = Insul == 'After')

############################################################
#fitted model objects
#对模型的更进一步操作包括：
# print： 简单的display
# summary： 分析output
# coef（coefficient）：提取出regression coefficeints
# resid（residuals）：残差
# fitted（fitted.values）：拟合值
# deviance: redisual sum of squares
# anova：方差分析
# predict：对新数据的means或standard errors的预测
# plot： 诊断图
############################################################

summary(gasB)

summary(gasA)

########################################
# 样本方差分析
########################################
gasB
summary(gasB)
deviance(gasB)
gasB$df.resid
varB <- deviance(gasB)/gasB$df.resid  #直接计算
varB

varB1 <- summary(gasB)$sigma^2  #可供选择的方案，等同于直接计算
varB1


#####################################################################################################
#将两个模型整体到一个里面去
# a/x术语的意义是：a是一个factor，在不同a的水平上x对应的模型
# 本例中 Insul/Temp - 1意思是在不同Insul水平上，对应的Temp模型，- 1表示省略掉截距
#####################################################################################################
gasBA <- lm(Gas ~ Insul/Temp - 1, data = whiteside)
summary(gasBA)

##############################
# 拟合曲线
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
anova(gasPR, gasBA) #方差分析


#####################################
# 使用不同的参数，分开slopes来拟合模型
#####################################
options(contrasts = c("contr.treatment", "contr.poly"))
gasBA1 <- lm(Gas ~ Insul*Temp, data = whiteside)
summary(gasBA1)$coef






#################################################### 
# 回归诊断实例 
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

#查看Knock Hill的预测值
cbind(hills, pred = predict(hills.lm))["Knock Hill", ]

(hills1.lm <- update(hills.lm, subset = -18))
#######################################################
# 可以看到踢除Knock Hill对模型参数没有较大影响
# 考虑检测一下其他两个离异点
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
# leuk数据集，未审查过leukaemia的存活时间
# 包含accelerated life test 实验的结果数据
#################################################
head(leuk)
head(motors)

#####################################################################################
#生存分析的基本概念
#生存时间一般通过随访收集

# 截尾数据与完整数据
# 是否考虑中途撤退的数据

#生存时间 ：病人从进入观察随访到规定的结局出现，其间所时间
#完全数据（complete data）：从起点至死亡所经历的时间，即死者的存活时间
#删失数据（censored data）：由于失访、改变治疗方案、研究工作结束等情况，使得部分病人不能
#　　　　　　　　　　　　　　随访到底，称为删失数据

# 死亡概率
# 死亡密度函数

# 生存率
# 风险函数


################################
# 生存分析研究的主要内容
################################
#描述生存过程：研究生存时间的分布特点，估计生存率及平均生存时间，绘制生存曲线，估计出各时点
　　　　　　　　# 的生存率及中位生存时间，也可分析其生存特点　
#比较生存过程：　通过生存率及其标准误对各样本的生存率进行比较，以探讨各总体生存过程是否有差别
#影响生存时间的因素分析：通过生存分析模型来探讨影响生存时间的因素，通常以生存时间和结局作为来因变量，
#　而将影响它们的因素作为自变量，如年龄、性别、病理分型、治疗方式等，通过拟和模型，筛选出影响生存时
#  间的保护因素和有害因素


######################################################
# 生存分析的基本方法
######################################################
#　非参数法：　无论资料是什么样的分布形式，只根据样本提供的顺序统计量对生存率进行估计，
# 常用的方法有乘积极限法和寿命表法

#  参数法：　假定生存时间服从特定的参数分布，根据已知分布的特点对影响生存的时间进行
# 分析，常用方法有指数分布法、Weibull分布法、对数正态回归分析法等

# 半参数法：兼有参数和非参数的特点，主要用于分析影响生存时间和生存率的因素，属多因素分析方法，
# 典型的分析方法是Cox模型分析方法

########################################
# 生存率的估计与生存曲线
########################################
#　根据样本含量的大小可分别选择：
# 乘积极限法（Kaplan-Meier）：小样本时
# 寿命表法（life table）：大样本时



###########################################################################


###########################################################################
# Surv(times, status)用于描述uncensored data，通常出现在model的左边 
###########################################################################
?Surv
str(leuk)
?survfit
####################################
# survfit生成一条survival curves
####################################
plot(survfit(Surv(time) ~ ag, data = leuk), lty = 2:3, col = 2:3)
legend(110, 0.8, c("ag absent", "ag present"), lty = 2:3, col = 2:3)



###########################################################################
# survfit也可用于处理censored data(删失数据) 
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
# 可检测groups间的差异性
#########################################################
survdiff(Surv(time, cens) ~ treat, data = gehan)
survdiff(Surv(time) ~ ag, data = leuk)

#################################################
# Parametric Models
#可用于exploratory Models
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
# 可计算出groups中的difference ，使用Weibull model中的Wald test
# 或使用anova方法来计算 likelihood ration
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
# lh与deaths数据集
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






































