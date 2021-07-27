##Latest COVID-19 Data in Asia##
##Courtesy: Anandhu H of Kaggle##
##©The Nerd Chronicles. All Rights Reserved##

#Loading Packages & Excel File##
library(readr)
asiarona <- read_csv("Latest Covid-19 Data in Asia.csv")
View(asiarona)

library(corrplot)
library(faraway)
library(tidyverse)
library(broom)
library(psych)
library(skimr)
library(jmv)

#Correlation Tests for Each Key Metric per Population#
a <- cor.test(asiarona$`Total Cases`, asiarona$Population)
b <- cor.test(asiarona$`Total Deaths`, asiarona$Population)
c <- cor.test(asiarona$`Total Recovered`, asiarona$Population)
d <- cor.test(asiarona$`Active Cases`, asiarona$Population)

#Tidying Up The Correlation Tests#
at <- tidy(a)
bt <- tidy(b)
ct <- tidy(c)
dt <- tidy(d)

#Searching for the P-values of the correlation tests#
atp <- at$p.value
btp <- bt$p.value
ctp <- ct$p.value
dtp <- dt$p.value

atp
btp
ctp
dtp

#Setting Up The Scatter Plots#
ggstatsplot::ggscatterstats(asiarona, x = `Total Cases`, y = `Population`)
t.test(`Population` ~ `Active Cases`, data = asiarona)

#Creating Our Linear Model#
g <- lm(Population ~ `Total Deaths` + `Total Recovered` + `Active Cases`, data = asiarona)
summary(g)
rmse <- function(x,y) sqrt(mean(x-y)^2)
rmse(g$fitted.values,asiarona$Population)
plot(fitted(g), residuals(g), xlab= "Fitted", ylab = "Residuals")
abline (h=0)
qqnorm(residuals(g))
qqline(residuals(g))
halfnorm(cooks.distance(g))
gi <- influence(g)
summary(gi)
show(gi)
library(MASS)
boxcox(g,plotit=T)
shapiro.test (residuals(g))
gp <- g$p.value
gp
prediction <- predict(g, asiarona, type = "response")
mo <- cbind(prediction)
plot(prediction)
plot(mo)
plot(log(prediction))
plot(log(mo))
m <- log(prediction)
n <- log(mo)
plot(m,n)

#Creating Our Logarithmic Model#
k <- lm(log(Population) ~ `Total Deaths` + `Total Recovered` + `Active Cases`, data = asiarona)
summary(k)
rmse(k$fitted.values,asiarona$Population)
plot(fitted(k), residuals(k), xlab= "Fitted", ylab = "Residuals")
abline (h=0)
qqnorm(residuals(k))
qqline(residuals(k))
halfnorm(cooks.distance(k))
ki <- influence(k)
summary(ki)
show(ki)


#Citations#
citation(package = 'readr')
citation(package = 'corrplot')
citation(package = 'faraway')
citation(package = 'tidyverse')
citation(package = 'broom')
citation(package = 'psych')
citation(package = 'skimr')
citation(package = 'jmv')
citation(package = 'MASS')