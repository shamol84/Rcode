## -----------------------------
## Read data & clean it.
## -----------------------------
setwd("/Users/yifanxu/gdrive/aawork/--VUVlab/Mohammad")
dt <- read.csv("mydt.csv")
contrasts(dt$Brand) <- contr.sum
dt$date <- as.POSIXct(strptime(as.character(dt$Date), "%m/%d/%Y %H:%M"))
dt <- subset(dt, select = -Date)

## Remove malfunctioning day and brand
dt <- subset(dt, !(day == "9/18/2013" & Brand %in% c("Bosch", "ET", "Trina")))

dt1 <- subset(dt, hour > 10 & hour < 14)
dt2 <- subset(dt, hour <= 10)
dt3 <- subset(dt, hour >= 14)

## -----------------------------
## Correlation, Partial correlations and semi-partial correlation
## -----------------------------
library(ppcor)
## Correlation
round(cor(dt1[c(4,1:3,5)]), 3)

## Partial correlation
# round(pcor(dt[c(4,1:3,5)])$estimate, 3)
round(pcor(dt1[c(4,1:3,5)])$estimate, 3)
## round(pcor(dt2[c(4,1:3,5)])$estimate, 3)
## round(pcor(dt3[c(4,1:3,5)])$estimate, 3)

## Squared partial correlation
round(pcor(dt1[c(4,1:3,5)])$estimate^2, 3)

## Semi-partial correlation
round(spcor(dt1[c(4,1:3,5)])$estimate, 3)

## Contribution to R^2
round(spcor(dt1[c(4,1:3,5)])$estimate^2, 3)
round(spcor(dt1[c(4,1:3)])$estimate^2, 3)

## Another way to find partial correlation
mod1 <- lm(Micro.T ~ Ambient.T + Module.T + Power + Irradiance, data = dt1)
mod2 <- lm(Micro.T ~ Ambient.T + Module.T + Power, data = dt1)
mod2 <- lm(Irradiance ~ Ambient.T + Module.T + Power, data = dt)
mod3 <- lm(Micro.T ~ Ambient.T + Module.T + Power, data = dt)
cor(residuals(mod1), residuals(mod2))

## -----------------------------
## Build model
## -----------------------------
library(nlme)
## Noon
mod <- lm(Micro.T ~ Brand * (Ambient.T + Module.T) ^ 2, data = dt1, na.action = na.omit)
summary(mod)

## Morning
mod2 <- lm(Micro.T ~ Brand * (Ambient.T + Module.T) ^ 2, data = dt2, na.action = na.omit)
summary(mod2)

## Afternoon
mod3 <- lm(Micro.T ~ Brand * (Ambient.T + Module.T) ^ 2, data = dt3, na.action = na.omit)
summary(mod3)

## -----------------------------
## Visualize
## -----------------------------
library(ggplot2)
library(reshape)
dt1$pred <- predict(mod)
dt1$pred1 <- predict(gls.mod1)
to.plot <- melt(dt1, id.vars = c("date", "Brand", "day"), measure.vars = c("Micro.T", "pred"))


#ggplot(subset(to.plot, day == levels(day)[46]) or [10]
ggplot(subset(to.plot, day == levels(day)[46]),
       aes(x = date, y = value, color = variable)) +
  geom_line() + facet_wrap(~Brand, ncol = 2)
# +  geom_point()

## -----------------------------
## Time series residuals (not used in paper)
## -----------------------------
dt1$res <- residuals(mod)

temp <- subset(dt1, Brand == levels(Brand)[1])

plot(temp$date[1:100], temp$res[1:100], type = "h")

gls.mod0 <- gls(Micro.T ~ Brand * (Module.T + Ambient.T)^2,
               data = dt1, correlation = corARMA(value = 0, form = ~1|Brand, p = 1, q = 0, fixed = TRUE))

summary(gls.mod)

dt1$day = factor(dt1$day)
library(nlme)

gls.mod1 <- gls(Micro.T ~ Brand * (Module.T + Ambient.T) ^2,
               data = dt1, correlation = corARMA(form = ~ 1|Brand, p = 2))

dt1$branday = paste0(as.character(dt1$Brand),"-", as.character(dt1$day))

gls.mod2 <- gls(Micro.T ~ Brand * (Module.T + Ambient.T + I(hour + min/60))^2 ,
               data = dt1, correlation = corARMA(form = ~ 1|branday, p = 1))

gls.mod3 <- gls(Micro.T ~ Brand * (Module.T + Ambient.T)^2,
               data = dt1, correlation = corARMA(form = ~ 1|branday, p = 2))

gls.mod4 <- gls(Micro.T ~ Brand * (Module.T + Ambient.T)^2,
               data = dt1, correlation = corARMA(form = ~ 1|branday, p = 2, q = 2))

anova(gls.mod0, gls.mod1, gls.mod2, gls.mod3, gls.mod4)

plot(gls.mod1)
plot(gls.mod3)

## Select gls.mod1

acf(residuals(gls.mod2)[dt1$Brand == levels(dt1$Brand)[1]])
pacf(residuals(gls.mod2)[dt1$Brand == levels(dt1$Brand)[1]])

plot(date, residuals())
