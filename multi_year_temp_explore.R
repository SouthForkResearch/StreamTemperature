

############################################################################################################
#Code exploring fitting the Wenatchee mean temperature models across years and including discharge at the bottom of the watershed
#(USGS stream gage #12462500) as an average 8 day variable.  

#updated by Jared Siegel 7/17/2017
##############################################################################################


library(timeSeries)
library(lattice)
library(foreign)
library(doBy)
library(qpcR)
library(pls)
library(boot)
library(Hmisc)
library(readxl)
library(lubridate)
library(zoo)
library(car)
library(gvlma)
library(ggplot2)
library(AICcmodavg)


################################
# full year
###################################
setwd("C:/Users/jsiegel/Documents/South Fork Research/")
getwd()
data<-read.csv("Wenatchee_mean_multi.csv")
head(data)

xyplot(y~z|factor(year), data = data,
       type='p',layout=c(3,2),
       xlab='Interval',ylab='Number of Steps')
xyplot(y~d|factor(year), data = data,
       type='p',layout=c(3,2),
       xlab='Interval',ylab='Number of Steps')

#subsetting spring and fall
data.sp <- subset(data, z < 183)
data.fall <- subset(data, z > 183)


###fitting whole year combined
y <- data$y
x <- data$x
z <- data$z
e <- data$e
d <- data$d
plot(x, y)


mod <- lm(y ~ x + I(x^2) + z + e)
mod.d <- lm(y ~ x + I(x^2) + z + e +x*d*z)
sum_mod <- summary(mod)
sum_mod.d <- summary(mod.d)
plot(mod)
plot(mod.d)

AIC(mod);AIC(mod.d)

data$resid<-resid(mod)
data$resid.d<-resid(mod.d)

data$pred <-predict(mod)
data$pred.d <-predict(mod.d)
data$residsq <-(data$y - data$pred)^2
data$residsq.d <-(data$y- data$pred.d)^2
sqrt(mean(data$residsq)) #RMSE original model
sqrt(mean(data$residsq.d)) #RMSE discharge model
sqrt(tapply(data$residsq, data$year, mean)) #RMSE by year original model
sqrt(tapply(data$residsq.d, data$year, mean)) #RMSE by year discharge model

pred.y <- predict(mod)
plot(pred.y, y, main = "8-day Mean Full Year")
abline(0,1)
post_mod <- summary(lm(y ~ pred.y))
gvmodel <- gvlma(mod)
summary(gvmodel)
gvmodel.d <- gvlma(mod.d)
summary(gvmodel.d)
plot(mod, which= 1:6)
outlierTest(mod)
qqPlot(mod, main="QQ Plot Full Year")
spreadLevelPlot(mod)
plot(pred.y, mod$residuals, main="Model diagnostics Full Year", xlab="Predicted", ylab="Residuals")
data$resid<-resid(mod)
plot(data$z, data$resid)

win.graph(width = 8, height = 3.5)
xyplot(resid~z|factor(year), data = data,
       type=c('p', 'g', 'a'),layout=c(5,1),
       xlab='julian data',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-8,8))
win.graph(width = 8, height = 3.5)
xyplot(resid~e|factor(year), data = data,
       type=c('p', 'smooth', 'g'),layout=c(5,1),
       xlab='elevation',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-8,8))

win.graph(width = 8, height = 3.5)
xyplot(resid.d~z|factor(year), data = data,
       type=c('p', 'g', 'a'),layout=c(5,1),
       xlab='julian data',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-8,8))
win.graph(width = 8, height = 3.5)
xyplot(resid.d~e|factor(year), data = data,
       type=c('p', 'smooth', 'g'),layout=c(5,1),
       xlab='elevation',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-8,8))

###fitting fall only
y <- data.fall$y
x <- data.fall$x
z <- data.fall$z
e <- data.fall$e
d <- data.fall$d

plot(x, y)

mod.fall <- lm(y ~ x + I(x^2) + z + e)
mod.fall.d <- lm(y ~ x + I(x^2) + z + e +x*d*z)


AICc(mod.fall)-AICc(mod.fall.d)

sum_mod.fall <- summary(mod.fall)
sum_mod.fall.d <- summary(mod.fall.d)
plot(mod.fall)
plot(mod.fall.d)

data.fall$resid<-resid(mod.fall)
data.fall$resid.d<-resid(mod.fall.d)

data.fall$pred <-predict(mod.fall)
data.fall$pred.d <-predict(mod.fall.d)
data.fall$residsq <-(data.fall$y - data.fall$pred)^2
data.fall$residsq.d <-(data.fall$y- data.fall$pred.d)^2
sqrt(mean(data.fall$residsq)) #RMSE original model
sqrt(mean(data.fall$residsq.d)) #RMSE discharge model
sqrt(tapply(data.fall$residsq, data.fall$year, mean)) #RMSE by year original model
sqrt(tapply(data.fall$residsq.d, data.fall$year, mean)) #RMSE by year discharge model

win.graph(width = 8, height = 3.5)
xyplot(resid~z|factor(year), data = data.fall,
       type=c('p', 'g', 'a'),layout=c(5,1),
       xlab='julian data',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-8,8))
win.graph(width = 8, height = 3.5)
xyplot(resid~e|factor(year), data = data.fall,
       type=c('p', 'smooth', 'g'),layout=c(5,1),
       xlab='elevation',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-8,8))

win.graph(width = 8, height = 3.5)
xyplot(resid.d~z|factor(year), data = data.fall,
       type=c('p', 'g', 'a'),layout=c(5,1),
       xlab='julian data',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-8,8))
win.graph(width = 8, height = 3.5)
xyplot(resid.d~e|factor(year), data = data.fall,
       type=c('p', 'smooth', 'g'),layout=c(5,1),
       xlab='elevation',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-8,8))
xyplot(resid.d~e|factor(year), data = data.fall,
       type=c('p', 'smooth', 'g'),layout=c(5,1),
       xlab='elevation',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-8,8))

win.graph(width = 12, height = 8)
xyplot(resid.d~d|factor(e), data = data.fall,
       type=c('p', 'smooth', 'g'),
       xlab='discharge',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-6,8))

###fitting spring only
y <- data.sp$y
x <- data.sp$x
z<- data.sp$z
e <- data.sp$e
d <- data.sp$d
plot(x, y)


mod.sp <- lm(y ~ x + I(x^2) + z + e)
mod.sp.d <- lm(y ~ x + I(x^2) + z + e + x*d*z)
sum_mod.sp <- summary(mod.sp)
sum_mod.sp.d <- summary(mod.sp.d)

AIC(mod.sp);AIC(mod.sp.d)
AIC(mod.sp)-AIC(mod.sp.d)



data.sp$resid<-resid(mod.sp)
data.sp$resid.d<-resid(mod.sp.d)

data.sp$pred <-predict(mod.sp)
data.sp$pred.d <-predict(mod.sp.d)
data.sp$residsq <-(data.sp$y - data.sp$pred)^2
data.sp$residsq.d <-(data.sp$y- data.sp$pred.d)^2
sqrt(mean(data.sp$residsq)) #RMSE original model
sqrt(mean(data.sp$residsq.d)) #RMSE discharge model
sqrt(tapply(data.sp$residsq, data.fall$year, mean)) #RMSE by year original model
sqrt(tapply(data.sp$residsq.d, data.fall$year, mean)) #RMSE by year discharge model

win.graph(width = 8, height = 3.5)
xyplot(resid~z|factor(year), data = data.sp,
       type=c('p', 'g', 'a'),layout=c(4,1),
       xlab='julian data',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"))
win.graph(width = 8, height = 3.5)
xyplot(resid~e|factor(year), data = data.sp,
       type=c('p', 'smooth', 'g'),layout=c(4,1),
       xlab='elevation',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"))

win.graph(width = 8, height = 3.5)
xyplot(resid.d~z|factor(year), data = data.sp,
       type=c('p', 'g', 'a'),layout=c(4,1),
       xlab='julian data',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"), ylim = c(-6,8))
win.graph(width = 8, height = 3.5)
xyplot(resid.d~e|factor(year), data = data.sp,
       type=c('p', 'smooth', 'g'),layout=c(4,1),
       xlab='elevation',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-6,8))

win.graph(width = 12, height = 8)
xyplot(resid.d~d|factor(e), data = data.sp,
       type=c('p', 'smooth', 'g'),
       xlab='discharge',ylab='resid (C)', par.settings=simpleTheme(col="blue", col.line="red"),ylim = c(-6,8))

########################################



