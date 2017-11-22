library(vars)
library(urca)

manhours <- read.csv("AvgManHours_Ohio.csv", header = TRUE)
manHours <- manhours$SMU39000003000000007SA

nonFarm <- read.csv("Nonfarm_Ohio.csv", header = TRUE)
nonfarm <- nonFarm$OHNA

ohio <- read.csv("Ohio_formatted.csv", header = TRUE)
colnames(ohio) <- c("elect", "income", "unemp")
attach(ohio)

manHours.ts <- ts(manHours, start = 2001, frequency = 12)
nonfarm.ts <- ts(nonfarm, start = 2001, frequency = 12)
elect.ts <- ts(elect, start = 2001, frequency = 12)
income.ts <- ts(income, start = 2001, frequency = 12)
unemp.ts <- ts(unemp, start = 2001, frequency = 12)



## Augment Dickey-Fuller Test


#Avg Manufacturing Hours
summary(ur.df(manHours.ts, type = 'trend', lags = 3))
summary(ur.df(manHours.ts, type = 'drift', lags = 3))
summary(ur.df(manHours.ts, type = 'none', lags = 3))

manHours.ln <- diff(log(manHours.ts))
summary(ur.df(manHours.ln, type = 'trend', lags = 3))
summary(ur.df(manHours.ln, type = 'drift', lags = 3))
summary(ur.df(manHours.ln, type = 'none', lags = 3))

#Nonfarm Employment
summary(ur.df(nonfarm.ts, type = 'trend', lags = 3))
summary(ur.df(nonfarm.ts, type = 'drift', lags = 3))
summary(ur.df(nonfarm.ts, type = 'none', lags = 3))

nonfarm.ln <- diff(log(nonfarm.ts))
summary(ur.df(nonfarm.ln, type = 'trend', lags = 3))

#Industrial Electricity Sales
summary(ur.df(elect.ts, type = 'trend', lags = 3))
summary(ur.df(elect.ts, type = 'drift', lags = 3))
summary(ur.df(elect.ts, type = 'none', lags = 3))

elect.ln <- diff(log(elect.ts))
summary(ur.df(elect.ln, type = 'trend', lags = 3))

#Total Personal Income Minus Transfer Payments
summary(ur.df(income.ts, type = 'trend', lags = 3))
summary(ur.df(income.ts, type = 'drift', lags = 3))
summary(ur.df(income.ts, type = 'none', lags = 3))

income.ln <- diff(log(income.ts))


#Unemployment Rate
summary(ur.df(unemp.ts, type = 'trend', lags = 3))
summary(ur.df(unemp.ts, type = 'drift', lags = 3))
summary(ur.df(unemp.ts, type = 'none', lags = 3))

unemp.ln <- diff(log(unemp.ts))
summary(ur.df(unemp.ln, type = 'trend', lags = 3))


# Check for cointegration
plot.ts(cbind(manHours.ts, nonfarm.ts, unemp.ts))

manHours.nonfarm <- lm(nonfarm.ts ~ manHours.ts)
summary(manHours.nonfarm)
plot(manHours.ts, nonfarm.ts)
abline(manHours.nonfarm)

qqnorm(manHours.nonfarm$residuals)
qqline(manHours.nonfarm$residuals)

summary(ur.df(manHours.nonfarm$residuals, type = 'trend', lags = 1))
summary(ur.df(manHours.nonfarm$residuals, type = 'drift', lags = 1))
summary(ur.df(manHours.nonfarm$residuals, type = 'none', lags = 1))



#Sample replication
y <- rep(0, 100)
b1 <- .5
b0 = 3
e = rnorm(100, 0 ,1)

for(i in 1:100){
  y[i+1] = b0 + b1*y[i] + e[i]
}
plot(y, type = 'l')


# fit var with avgmanhours, nonfarm, electricity

ohio.df <- data.frame(manHours = manHours, nonfarm = nonfarm, electricity = elect)
plot.ts(ohio.df)

ohio.var <- VAR(ohio.df, p = 2, type = 'both')
summary(ohio.var)

#Let y be nonfarm
y = nonfarm[1:2] + rep(0, 191)

manHours.l1 <- manHours[2] + rep(0, 191)

nonfarm.l1 <- nonfarm[2] + rep(0, 191)

manHours.l2 <- manHours[1] + rep(0, 192)

nonfarm.l2 <- nonfarm[1] + rep(0, 192)

set.seed(18495)
e <- rnorm(193, 0, 1)

const = -129.77918


for(i in 1:191){
  y[i+2] = 1.19416 * y[i+1] + -0.23452 * y[i] + 5.69817 * manHours[i]  + e[i] -25
}

plot.ts(y, ylim=c(5000, 6100))
lines(nonfarm, type = 'l', col = 'red')
