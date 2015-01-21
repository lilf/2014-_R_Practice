library(TSA)
#########################################################
#R Code 1.1 Simulation of AR(1)-process with φ = 0.9
#########################################################
set.seed(123456)
y <- arima.sim(n = 100, list(ar = 0.9), innov = rnorm(100))
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot.ts(y, ylab = '')
acf(y, main = "Autocorrelations", ylab = '', ci.col = 'black')
pacf(y, main = "Partial Autocorrelations", ylab = '', ylim = c(-1, 1), ci.col = 'black')
par(op)


#########################################################
#可使用polyroot（）函数来检验过程的稳健性
#filter（）与arima.sim（）类似，但前者也可生成非稳定性的AR(p)过程
#########################################################





#######################################################################
#R Code 1.2 Estimation of AR(2)-process with φ1 = 0.6 and φ2 = ???0.28
#######################################################################
series <- rnorm(1000)
y.st <- filter(series, filter = c(0.6, -0.28), method = 'recursive')
#############################################################
#filter可用于单变量时间序列的拟合，或多变量中的每一个时间序列
#############################################################
ar2.st <- arima(y.st, c(2, 0, 0), include.mean = FALSE, transform.pars = FALSE, method = "ML")
#############################
#arima用于单变量ts的模型拟合
#############################
ar2.st$coef
#############################
#      ar1        ar2 
#0.6147474 -0.2835138 
#############################
polyroot(c(1, -ar2.st$coef))

#####################################
#polyroot查找一个多项式的全零矩阵
#####################################
#########################################
#  1.084158+1.533547i 1.084158-1.533547i
#########################################
?Mod
Mod(polyroot(c(1, -ar2.st$coef)))
#####################################
#1.878075 1.878075
#####################################
root.comp <- lm(polyroot(c(1, -ar2.st$coef)))##运行的有问题
root.real <- Re(polyroot(c(1, -ar2.st$coef)))
#Plotting the roots in a unit circle
x <- seq(-1, 1, length = 1000)
y1 <- sqrt(1- x^2)
y2 <- -sqrt(1- x^2)
plot(c(x,x), c(y1, y2), xlab = 'Real part', ylab = 'Complex part', type = 'l',
     main = 'Unit Circle', ylim = c(-2, 2), xlim = c(-2, 2))

abline(h = 0)
abline(v = 0)
points(Re(polyroot(c(1, -ar2.st$coef))),   #运行的有问题
       lm(polyroot(c(1, -ar2.st$coef))), pch = 19)
legend(-1.5, -1.5, legend = 'Roots of AR(2)', pch = 19)
#####################################
#lm, Re是有问题的
#####################################






#######################################################################
#R Code 1.3 Box-Jenkins: U.S. unemployment rate
#######################################################################
###########################################
#logLik：所谓的对数似然值就是对数似然函数使其达到最大的取值。或都说是对数似然方程dlnL(t)/dt=0的值。模型比较时越大越好。
#AIC:假设模型的误差服从独立正态分布，鼓励数据拟合的优良性但避免出现过度拟合的情况，因此考虑模型就是AIC 值最小的那一个
#AICc：当样本小的情况下，AIC转变为AICc
#QAIC：可以调整过度离散
#BIC:乘法函数的线性组合， 值越小越好
###########################################
install.packages('urca')
library(urca)
data(npext)
y <- ts(na.omit(npext$unemploy), start = 1909, end = 1988, frequency = 1)
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(y, ylab = 'unemployment rate(logarithm')
acf(y, main = 'Autocorrelations', ylab = '')
pacf(y, main = 'Partial Autocorrelations', ylab = '', ylim = c(-1, 1))
par(op)
##tentative ARMA（2，0）
arma20 <- arima(y, order = c(2, 0, 0))
ll20 <- logLik(arma20)
#logLik返回的是一个model中参数的估计值，df为自由度
aic20 <- arma20$aic
res20 <- residuals(arma20)
Box.test(res20, lag = 20, type = 'Ljung-Box')
#####################################
#Box-Ljung test

#data:  res20
#X-squared = 21.914, df = 20, p-value = 0.3452
#####################################
shapiro.test(res20)
#####################################
#Shapiro-Wilk normality test

#data:  res20
#W = 0.9931, p-value = 0.9501
#####################################
#alternative specifications
#ARMA(3, 0)
arma30 <- arima(y, order = c(3, 0, 0))
ll30 <- logLik(arma30)
aic30 <- arma30$aic
lrtest <- as.numeric(2*(ll30 - ll20))
chi.pval <- pchisq(lrtest, df = 1, lower.tail = FALSE)
#ARMA(1, 1)
arma11 <- arima(y, order = c(1, 0, 1))
ll11 <- logLik(arma11)
aic11 <- arma11$aic
tsdiag(arma11)
#tsdiag时间序列的诊断图
res11 <- residuals(arma11)
Box.test(res11, lag = 20, type = 'Ljung-Box')
#####################################
#Box-Ljung test

#data:  res11
#X-squared = 15.1402, df = 20, p-value = 0.7683
#####################################
shapiro.test(res11)
#shapiro.test正态性检验
#####################################
#Shapiro-Wilk normality test

#data:  res11
#W = 0.9862, p-value = 0.5456
#####################################
tsdiag(arma11)
##using auto.arima()
library(forecast)
auto.arima(y, max.p = 3, max.q = 3, start.p = 1, start.q = 1, ic = 'aic')
#####################################
#Series: y 
#ARIMA(1,0,1) with non-zero mean 

#Coefficients:
#  ar1     ma1  intercept
#0.5272  0.5487     1.6934
#s.e.  0.1221  0.1456     0.1546

#sigma^2 estimated as 0.1845:  log likelihood=-46.51
#AIC=101.01   AICc=101.55   BIC=110.54
#####################################



#######################################################################
#R Code 1.4 Box-Jenkins: Predictions of the U.S. unemployment rate
#######################################################################
#Forecasts
arma11.pred <- predict(arma11, n.ahead = 10)
predict <- ts(c(rep(NA, length(y) - 1), y[length(y)], arma11.pred$pred))
upper <- ts(c(rep(NA, length(y) - 1), y[length(y)], arma11.pred$pred + 2 * arma11.pred$se), start = 1909, frequency = 1)
lower <- ts(c(rep(NA, length(y) - 1), y[length(y)], arma11.pred$pred - 2 * arma11.pred$se), start = 1909, frequency = 1)
observed <- ts(c(y, rep(NA, 10)), start = 1909, frequency = 1)
##plot of actual and forecasted values
plot(observed, type = 'l', ylab = 'Actual and predicated values', xlab = '')
lines(predict, col = 'blue', lty = 2)
lines(lower, col = 'red', lty = 5)
lines(upper, col = 'red', lty = 5)
abline(v = 1988, col = 'gray', lty = 3)





#########################################################
#R Code 2.1 Simulation of VAR(2)-process
#########################################################
##Simulate VAR(2)-data
install.packages("dse1")
install.packages("vars")
library(dse1)
library(vars)
##Setting the lag-polynomial A(L)
Apoly <- array(c(1.0, -0.5, 0.3, 0, 
                 0.2, 0.1, 0, -.02,
                 0.7, 1, 0.5, -0.3), 
               c(3, 2, 2))
##Setting Covariance to identity-matrix
B <- diag(2)
##Setting constant term to 5 and 100
TRD <- c(5, 10)
#Generating the VAR(2) model
var2 <- ARMA(A = Apoly, B = B, TREND = TRD)
##Simulating 500 observations
varsim <- simulate(var2, sampleT = 500, noise = list(w = matrix(rnorm(1000), 
            nrow = 500, ncol = 2)), rng = list(seed = c(123456)))
##Obtaining the generated series
vardat <- matrix(varsim$output, nrow = 500, ncol = 2 )
colnames(vardat) <- c("y1", "y2")
##Plotting the series
plot.ts(vardat, main = '', xlab = "")
##Determing an appropriate lag-order
infocrit <- VARselect(vardat, lag.max = 3, type = 'const')
##Estimating the model
varsimest <- VAR(vardat, p = 2, type = 'const', season = NULL, exogen = NULL)
##Alternatively , selection according to AIC
varsimest <- VAR(vardat, type = 'const', lag.max = 3, ic = 'SC')
##Checking the roots
##roots用于测试stability
roots <- roots(varsimest)


#########################################################
#R Code 2.2 Diagnostic tests of VAR(2)-process
#########################################################
##testing serial correlation
args(serial.test)  ##列出serial.test中的args
##Portmanteau-Test   序列化的相关误差的检验
?serial.test
var2c.serial <- serial.test(varsimest, lags.pt = 16, type = 'PT.asymptotic')
var2c.serial
plot(var2c.serial, names = 'y1')
plot(var2c.serial, names = 'y2')
##testing heteroscedasticity 检验异方差性
args(arch.test)
var2c.arch <- arch.test(varsimest, lags.multi = 5, multivariate.only = TRUE)
var2c.arch
#arch.test（）返回三个值：
#第一个值是matrix的residuals
#第二值是由arch.uni来标识的，是一个list对象，存储着每一个series单一test的结果
#第三什值存储着多元test的结果，由arch.nul标识
##testing for normality
args(normality.test)
var2c.norm <- normality.test(varsimest, multivariate.only = TRUE)
var2c.norm
##class and methods for diagnostic tests
class(var2c.serial)
class(var2c.arch)
class(var2c.norm)
methods(class = 'varcheck')
##Plot of objects"varcheck"
args(vars:::plot.varcheck)
plot(var2c.serial, names = 'y1')
??vars



#########################################################
#R Code 2.3 Empirical fluctuation processes
#########################################################
#########################################################
# struchange包中的efp（）可实现CUSUM、CUSUM-of-squares,MOSUM及fluctuation test
# vars包中的stability（）是对efp()函数的封装
#########################################################
reccusum <- stability(varsimest, type = "OLS-CUSUM")
par(mfrow = c(2, 1))

plot(reccusum)
fluctuation <- stability(varsimest, type = "fluctuation")
plot(fluctuation)
#########################################################
#R Code 2.4 Causality analysis of VAR(2)-process
#########################################################
#3# Causality tests
## Granger and instantaneous causality
var.causal <- causality(varsimest, cause = 'y2')
var.causal


#########################################################
#R Code 2.5 Forecasts of VAR-process
#########################################################
#########################################################
#除了forecasts，其他用于探索变量间关系的方法有：impulse response analysis 及forecast error variance decomposition
#########################################################
##Forecasting objects of class varest
args(vars:::predict.varest)
predictions <- predict(varsimest, n.ahead = 25, ci = 0.95)
class(predictions)
args(vars:::plot.varprd)
##plot of predictions for y1
plot(predictions, names = "y1")
## Fanchart for y2
args(fanchart)
fanchart(predictions, names = "y2")
predictions

#########################################################
#R Code 2.6 IRA of VAR-process
#########################################################
##Impulse response analysis 
irf.y1 <- irf(varsimest, impulse = 'y1', 
              response = "y2", n.ahead = 10, 
              ortho = FALSE, cumulative = FALSE, 
              boot = FALSE, seed = 12345)
args(vars:::plot.varirf)
plot(irf.y1)
irf.y2 <- irf(varsimest, impulse = 'y2',
              response = "y1", n.ahead = 10, 
              ortho = TRUE, cumulative = TRUE, 
              boot = FALSE, seed = 12345)
plot(irf.y2)


#########################################################
#R Code 2.6 FEVD of VAR-process
#########################################################
##Forecast error variance decomposition
fevd.var2 <- fevd(varsimest, n.ahead = 10)
args(vars:::plot.varfevd)
par(mfrow = c(2,1))
plot(fevd.var2, addbars = 2)
?plot
fevd.var2

#########################################################
#使用des1，vars包
#确定多元系数：A、B、TRD
#用ARMA（）生成VAR(2)模型
#用simulate（）模拟n个onservations，存入varsim中
#获得生成的系列数据，存入vardat中
#画出series图，plot.ts（）
#使用VARselect（）确定适当的lag-order的value
#model的estimate
#使用VAR（）来通过type为“AIC”或“const”来estimate model
#roots()函数来checking roots

## VAR-model完成estimated后，需探索其residuals是否与model的假设相附合
#需check两部分: absence of serial correlation（相关性）[Portmanteau test 及 LM test]及hereoscedasticity（异方差性），来判断是否队伍正态分布
#serial.test()：返回值在list元素中的serial， 有一个class属性htest

#最终check，执行structural stability test： CUSUM、CUSUM-of-squares（多元统计）及fluctuation tests（每个方程等式偏差的检验），均可通过vars包来实现
#

#Jarque-Bera正态性检验可用于单变量或多元series的residuals的分析，包括多元的峰度及峭度

# 通过vars包中的normality.test（）来计算


#causality analysis
#寻找变量之间的关系
#较为有名的是Granger causality test


# forecasting
# 通过诊断检验的VAR-model可用于forecasting
# 主要目的是发现模型中各变量之间隐含的关系
#其他的工具impulse response analysis及forecast error variance decomposition
# VAR-processes是通过vars包中的predict（）中的varest属性

# Impulse Response Functions
# Forecast Error Variance Decomposition（FEVD）是以impulse response coefficient matrix为基础的
#########################################################








#########################################################
# Structural Vector Autoregressive Models
#########################################################
#########################################################
# R Code 2.8 SVAR: A-model
#########################################################
library(dse1)
library(vars)
Apoly <- array(c(1.0, -0.5, 0.3, 0.8, 
                 0.2, 0.1, -0.7, -0.2,
                 0.7, 1, 0.5, -0.3), 
               c(3, 2, 2))
## Setting covariance to identity-matrix
B <- diag(2)
# Generating the VAR（2）model
svarA <- ARMA(A = Apoly, B = B)
svarA
## Simulating 500 observations
svarsim <- simulate(svarA, sampleT = 500, 
                    rng = list(seed = c(123456)))
## Obtaining the generated series
svardat <- matrix(svarsim$output, nrow = 500, ncol = 2)
colnames(svardat) <- c("y1", "y2")
#Estimating the VAR
varest <- VAR(svardat, p = 2, type = 'none')
#Setting up matrices for A-model
Amat <- diag(2)
Amat[2, 1] <- NA
Amat[1, 2] <- NA
## Estimating the SVAR A-type by direct maximisation of the log-likelihood
args(SVAR)
svar.A <- SVAR(varest, estmethod = 'direct', Amat = Amat, hessian = TRUE)
svar.A



#########################################################
# R Code 2.9 SVAR: B-model
#########################################################
library(dse1)
library(vars)
## B-model
Apoly <- array(c(1.0, -0.5, 0.3, 0,
                 0.2, 0.1, 0, -0.2,
                 0.7, 1, 0.5, -0.3),
               c(3, 2, 2))
## Setting covariance to identity-matrix
B <- diag(2)
B[2, 1] <- -0.8
## Generating the VAR(2) model
svarB <- ARMA(A = Apoly, B = B)
## Simulating 500 observatins
svarsim <- simulate(svarB, sampleT = 500, rng = list(seed = c(12346)))
svardat <- matrix(svarsim$output, nrow = 500, ncol = 2)
colnames(svardat) <- c("y1", "y2")
varest <- VAR(svardat, p = 2, type = 'none')
## Estimating the SVAR B-type by scoring algorithm
## Setting up the restriction matrix and vector for B-model
Bmat <- diag(2)
Bmat[2, 1] <- NA
svar.B <- SVAR(varest, estmethod = "scoring", Bmat = Bmat, max.iter = 200)


#########################################################
# R Code 2.10 SVAR: Impulse response analysis 
#########################################################
## Impulse response analysis of SVAR A-type model
args(vars:::irf.svarest)
irf.svara <- irf(svar.A, impulse = 'y1', response = 'y2', boot = FALSE)
args(vars:::plot.varirf)
plot(irf.svara)

#########################################################
# R Code 2.11 SVAR: Forecast error variance decomposition
#########################################################
##FEVD analysis oof SVAR B-type model
args(vars:::fevd.svarest)
fevd.svarb <- fevd(svar.B, n.ahead = 5)
class(fevd.svarb)
methods(class = 'varfevd')
par(mfrow = c(2, 1))
plot(fevd.svarb)



#########################################################
# R Code 3.1 Stochastic and deterministic trends 
# 随机趋势与确定性趋势
#########################################################
set.seed(123456)
e <- rnorm(500)
## pure random walk
rw.nd <- cumsum(e)
# trend
trd <- 1:500
## random walk with drift
rw.wd <- 0.5*trd + cumsum(e)
## deterministic trend and noise
dt <- e +0.5*trd
lines(dt)
##plotting
par(mar = rep(5, 4))
plot.ts(dt, lty = 1, ylab = '', xlab = '')
lines(rw.wd, lty = 2)
par(new = T)
plot.ts(rw.nd, lty = 3, axes = FALSE)
axis(4, pretty(range(rw.nd)))
lines(rw.nd, lty = 3)
legend(5, 19, legend = c('det.trend + noise(ls)', 'rw drift(ls)', 'rw(rs)'), lty = c(1, 2, 3))


#########################################################
# R Code 3.2 ARMA versus ARFIMA model
#########################################################
install.packages("fracdiff")
library(fracdiff)
set.seed(123456)
#ARFIMA(0.4, 0, 0.0)
y1 <- fracdiff.sim(n = 100, ar = 0.4, ma = 0.0, d = 0.4) ##使用fracdiff包中的fracdiff.sim()生成一100个long-memory series
# ARIMA(0.4, 0.0, 0.0)
y2 <- arima.sim(model = list(ar = 0.4), n = 1000) ##使用arima.sim()生成short memory series
# Graphics
op <- par(no.readonly = TRUE)
layout(matrix(1:6, 3, 2, byrow = FALSE))

plot.ts(y1$series, main = 'Time series plot of long memory', ylab = '')
acf(y1$series, lag.max = 100, main = 'Autocorrelations of long memeory')
spectrum(y1$series, main = 'Spectral density of long memeory')

plot.ts(y2, main = 'Time series plot of short memory', ylab = '')
acf(y2, lag.max = 100, main = 'Autocorrelations of short memory')
spectrum(y2, main = 'Spectral density of short memory')
par(op)


###################################################
# estimate the fractional difference parameter d
# detect the long-memory behavior in a time  series
# 方法Hurst 
###################################################

#########################################################
# R Code 3.3 R/S statistic
#########################################################
library(fracdiff)
set.seed(123456)
# ARFIMA(0.0, 0.3, 0.0)
y <- fracdiff.sim(n = 1000, ar = 0.0, ma = 0.0, d = 0.3)
# Get the data series, demean this if necessary
y.dm <- y$series
max.y <- max(cumsum(y.dm))
min.y <- min(cumsum(y.dm))
sd.y <- sd(y$series)
RS <- (max.y - min.y)/sd.y
H <- log(RS)/log(1000)
d <- H - 0.5

#########################################################
# R Code 3.4 Geweke and Porter-Hudak method
#########################################################
library(fracdiff)
set.seed(123456)
y <- fracdiff.sim(n = 1000, ar = 0.0, ma = 0.0, d = 0.3)
y.spec <- spectrum(y$series, plot = FALSE)
lhs <- log(y.spec$spec)
rhs <- log(4*(sin(y.spec$freq/2))^2)
gph.reg <- lm(lhs ~ rhs)
gph.sum <- summary(gph.reg)
sqrt(gph.sum$cov.unscaled*pi/6)[2, 2]
gph.sum


#########################################################
# Spurious regression
#########################################################
#########################################################
# R Code 4.1 Spurious regression
#########################################################
library(lmtest)
set.seed(123456)
e1 <- rnorm(500)
e2 <- rnorm(500)
trd <- 1:500
y1 <- 0.8*trd + cumsum(e1)
y2 <- 0.6*trd + cumsum(e2)
sr.reg <- lm(y1 ~ y2)
sr.dw <- dwtest(sr.reg)$statistic
summary(sr.reg)
sr.dw 

#########################################################
# R Code 4.2 Engle-Granger procedure with generated data
#########################################################
set.seed(123456)
e1 <- rnorm(100)
e2 <- rnorm(100)
y1 <- cumsum(e1)
y2 <- 0.6*y1 + e2
lr.reg <- lm(y2 ~ y1)
error <- residuals(lr.reg)
error.lagged <- error[-c(99, 100)]
dy1 <- diff(y1)
dy2 <- diff(y2)
diff.dat <- data.frame(embed(cbind(dy1, dy2), 2))
colnames(diff.dat) <- c('dy1', 'dy2', 'dy1.1', 'dy2.1')
ecm.reg <- lm(dy2 ~ error.lagged + dy1.1 + dy2.1, data = diff.dat)


#################################################################
# R Code 4.3 Johansn method with artificially generated data
#################################################################
library(urca)
set.seed(123456)
e1 <- rnorm(250, 0, 0.5)
e2 <- rnorm(250, 0, 0.5)
e3 <- rnorm(250, 0, 0.5)
u1.ar1 <- arima.sim(model = list(ar = 0.75), innov = e1, n = 250)
u2.ar1 <- arima.sim(model = list(ar = 0.3), innov = e2, n = 250)
y3 <- cumsum(e3)
y1 <- 0.8*y3 + u1.ar1
y2 <- -0.3*y3 + u2.ar1  
y.mat <- data.frame(y1, y2, y3)
y.mat
vecm <- ca.jo(y.mat)
jo.results <- summary(vecm)
vecm.r2 <- cajorls(vecm, r = 2)
vecm.r2 <- cajorls(vecm, r = 2)
class(jo.results)
slotNames(jo.results)


#################################################################
# R Code 4.4 VECM as VAR in levels
#################################################################
library(vars)
vecm.level <- vec2var(vecm, r = 2)
arch.test(vecm.level)
normality.test(vecm.level)
serial.test(vecm.level)
predict(vecm.level)
irf(vecm.level, boot = FALSE)
fevd(vecm.level)
class(vecm.level)
methods(class = 'vec2var')





###########################################################################
# R Code 5.1 ADF test：Integration for consumption in the United Kingdom
###########################################################################
library(urca)
data(Raotbl3)
attach(Raotbl3)
lc <- ts(lc, start = c(1966, 4), end = c(1991, 2), frequency = 4)
lc.ct <- ur.df(lc, lags = 3, type = 'trend')
plot(lc.ct)
lc.co <- ur.df(lc, lags = 3, type = 'drift')
lc2 <- diff(lc)
lc2.ct <- ur.df(lc2, type = 'trend', lags = 3)


###########################################################################
# R Code 5.2 PP test：Integration for consumption in the United Kingdom
###########################################################################
library(urca)
data(Raotbl3)
attach(Raotbl3)
lc <- ts(lc, start = c(1966, 3), end = c(1991, 2), frequency = 4)
lc.ct <- ur.pp(lc, type = 'Z-tau', model = 'trend', lags = 'long')
lc.co <- ur.pp(lc, type = 'Z-tau', model = 'constant', lags = 'long')
lc2 <- diff(lc)
lc2.ct <- ur.pp(lc2, type = 'Z-tau', model = 'trend', lags = 'long')


###########################################################################
# R Code 5.3 ERS test：Integration order for real GNP in the United States
###########################################################################
library(urca)
data(nporg)
gnp <- log(na.omit(nporg[, 'gnp.r']))
gnp.d <- diff(gnp)
gnp.ct.df <- ur.ers(gnp, type = 'DF-GLS', model = 'trend', lag.max = 4)
gnp.ct.pt <- ur.ers(gnp, type = 'P-test', model = 'trend')
gnp.d.ct.df <- ur.ers(gnp.d, type = 'DF-GLS', model = 'trend', lag.max = 4)
gnp.d.ct.pt <- ur.ers(gnp.d, type = 'P-test', model = 'trend')



###########################################################################
#R Code 5.4 SP test：Integration order for nominal GNP of the United States
###########################################################################
library(urca)
data(nporg)
gnp <- na.omit(nporg[, 'gnp.n'])
gnp.tau.sp <- ur.sp(gnp, type = 'tau', pol.deg = 2, signif = 0.05)
gnp.rho.sp <- ur.sp(gnp, type = 'rho', pol.deg = 2, signif = 0.05)



###########################################################################
#R Code 5.5 KPSS test：Integration order for nominal GNP of the United States
###########################################################################
library(urca)
data(nporg)
ir <- na.omit(nporg[, 'bnd'])
wg <- log(na.omit(nporg[, ' wg.n']))
ir.kpss <- ur.kpss(ir, type = 'mu', use.lag = 8)
wg.kpss <- ur.kpss(wg, type = 'tau', use.lag =8 )



###########################################################################
#R Code 6.1 Random walk with drift and structural break
###########################################################################
set.seed(123456)
e <- rnorm(500)
##trend
trd <- 1: 500
S <- c(rep(0, 249), rep(1, 251))
##random walk with drift
y1 <- 0.1*trd + cimsum(e)
## random walk with drift and shift
y2 <- 0.1*trd + 10*S + cumsum(e)



###########################################################################
#R Code 6.2 Unit roots and structural break: Zivot-Andrews test 
#Zivot and Andrews test应用于nominal及real dataSet
###########################################################################
library(urca)
data(nporg)
#head(nporg)
wg.n <- log(na.omit(nporg[, ''wg.n]))
########################################
# > length(nporg$wg.n)
# [1] 111
# > length(na.omit(nporg$wg.n))
# [1] 71
########################################
za.wg.n <- ur.za(nporg$wg.n, model = 'intercept', lag = 7)
plot(za.wg.n)
wg.r <- log(na.omit(nporg[, 'wg.r']))


###########################################################################
#R Code 6.3 HEGY test for seasonal unit roots
###########################################################################
library(urca)
install.packages('uroot')
library(uroot)
data(UKconinc)
incl <- ts(UKconinc$incl, start = c(1955, 1), end = c(1984, 4), frequency = 4)
HEGY000 <- HEGY.test(wts = incl, itsd = c(0, 0, c(0)), selectlags = list(mode = c(1, 4, 5)))
HEGY101 <- HEGY.test(wts = incl, itsd = c(1, 0, c(1, 2, 3)), selectlags = list(mode = c(1, 4, 5))) 
HEGY111 <- HEGY.test(wts = incl, itsd = c(1, 1, c(1, 2, 3)), selectlags = list(mode = c(1, 4, 5)))

layout

layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
layout.show(2)


































