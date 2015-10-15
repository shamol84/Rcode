library(ggplot2)
library(GGally)
library(lme4)
library(ggplot2)
library(reshape)
mydt = read.csv("1403averagedata1.csv")
mydt$date = as.POSIXct((strptime(as.character(mydt$Date), "%m/%d/%Y %H:%M")))

a = ggplot(data = mydt, aes(x = date, y = Micro.T)) + facet_grid(Brand ~ .) + geom_point(size = 0.3)


## ------------------------------------------
## Pair plots (NOT FINISHED, NEED TO FIX DIAG MATRICES)
## ------------------------------------------

pairs(mydt[sample(1:nrow(mydt), size = 300), c("Micro.T", "Irradiance", "Ambient.T", "Module.T", "Power")],
      upper.panel = function(x, y, ...){
          points(x, y, col = "red", cex = 0.3,  pch = 19)
      },
      lower.panel = function(x, y,...){
          cors = cor(x,y)
          text(sum(range(x))/2, sum(range(y))/2, label = round(cors,2), cex = 1 + abs(cors) * 2)
      },
      diag.panel = function(x, ...){
          h <- hist(x, plot = FALSE)
          breaks <- h$breaks; nB <- length(breaks)
          y <- h$counts; y <- y/max(y)
          rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
      }
      )

mydt.subset = mydt[sample(1:nrow(mydt), size = 2000, replace = FALSE),]


## ------------------------------------------
## Random effect model for all data
## ------------------------------------------
res = lmer(Micro.T ~ Irradiance + Ambient.T + Module.T + Power + (Irradiance + Ambient.T + Module.T + Power | Brand), data = mydt)

## res1 = lmer(Micro.T ~ Irradiance + Ambient.T + Module.T + (Irradiance + Ambient.T + Module.T | Brand), data = mydt.subset)

## res2 = lmer(Micro.T ~ Irradiance + Ambient.T + Power + (Irradiance + Ambient.T + Power | Brand), data = mydt.subset)
AIC(res, res1, res2)

## ------------------------------------------
## Random effect model on only solar noon time data
## ------------------------------------------
mydt.noon = subset(mydt, subset = Time.Period == "Noon")
res.noon = lmer(Micro.T ~ Irradiance + Ambient.T + Module.T + Power + (Irradiance + Ambient.T + Module.T + Power | Brand), data = mydt.noon)

## Do res
mydt.noon$pred = predict(res.noon)

## ------------------------------------------
## Visualization using ggplot on ONE DAY only.
## ------------------------------------------

## Everything
oneday = subset(mydt, subset = substr(mydt$date, 1,10) == "2013-08-03")
oneday = melt(oneday, measure.vars = c("Micro.T", "pred"))
a = ggplot(data = oneday, aes(x = date, y = value, color = variable)) + facet_wrap(~Brand, nrow = 4) + geom_point(size = 0.5) + geom_line(size = 0.3)
png("oneday_rand.png", unit = "in", res = 300, width = 12, height = 5)
a
dev.off()

## Noon time
oneday.noon = subset(mydt.noon, subset = substr(mydt.noon$date, 1,10) == "2013-08-03")
oneday.noon = melt(oneday.noon, measure.vars = c("Micro.T", "pred"))
a = ggplot(data = oneday.noon, aes(x = date, y = value, color = variable)) + facet_wrap(~Brand, nrow = 4) + geom_point(size = 0.5) + geom_line(size = 0.3) + labs(x = "Time", y = "Micro.T", title = "Prediction")
a

png("oneday_rand_noon.png", unit = "in", res = 300, width = 12, height = 6.2)
a
dev.off()
