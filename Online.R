#Online Shoppers Intention Purchasing Data set from UCI#
#Courtesy: Akash Patel of Kaggle#
#©The Nerd Chronicles. All Rights Reserved#

#Loading up readr and faraway packages & Excel file#
library(readr)
library(faraway)
online <- read_csv("online_shoppers_intention.csv")

#Setting up the linear model for the dataset#
g <- lm(Administrative_Duration ~ Informational_Duration + ProductRelated_Duration + BounceRates + ExitRates, data = online)
summary(g)
p <- plot(fitted(g), residuals(g), xlab= "Fitted", ylab = "Residuals")
abline (h=0)
qqnorm(residuals(g))
qqline(residuals(g))
halfnorm(cooks.distance(g))
gi <- influence(g)
summary(gi)
show(gi)
#Loading up the MASS package#
library(MASS)
boxcox(g,plotit=T)
gp <- predict(g, type = "response")

#Setting up the general linear models#
g2 <- glm(Administrative_Duration ~ Informational_Duration + ProductRelated_Duration + BounceRates + ExitRates, data = online, family = poisson())
summary(g2)
p2 <- plot(fitted(g2), residuals(g2), xlab= "Fitted", ylab = "Residuals")
abline (h=0) 
qqnorm(residuals(g2))
qqline(residuals(g2))
halfnorm(cooks.distance(g2))
gi2 <- influence(g2)
summary(gi2)
show(gi2)
boxcox(g2,plotit=T)

g3 <- glm(Administrative ~ Informational + ProductRelated + BounceRates + ExitRates, data = online, family = poisson())
summary(g3)
p3 <- plot(fitted(g3), residuals(g3), xlab= "Fitted", ylab = "Residuals")
abline (h=0) 
qqnorm(residuals(g3))
qqline(residuals(g3))
halfnorm(cooks.distance(g3))
gi3 <- influence(g3)
summary(gi3)
show(gi3)
anova(g2, g3, test = "Chisq")
gp2 <- predict(g2, type = "response")
gp3 <- predict(g3, type = "response")

#Setting up the model output for all linear models#
mo <- cbind(g,gp)
mo2 <- cbind(g2,gp2)
mo3 <- cbind(g3,gp3)

#Checking the linear and logarithmic root mean square errors of all the linear models#
rmse <- function(x,y) sqrt(mean(x-y)^2)
rmse(g$fitted.values,online$Administrative_Duration)
rmse(g2$fitted.values,online$Administrative_Duration)
rmse(g3$fitted.values,online$Administrative_Duration)
rmse(log(g$fitted.values),log(online$Administrative_Duration))
rmse(log(g2$fitted.values),log(online$Administrative_Duration))
rmse(log(g3$fitted.values),log(online$Administrative_Duration))

#Loading the jmv package and finding the correlation matrices and scatter plots of the relationships between Informational Duration and Administrative Duration & Informational and Administrative#
library(jmv)
corrMatrix(online, vars = vars(Informational_Duration, Administrative_Duration), ci = T, plots = T, plotDens = T)
corrMatrix(online, vars = vars(Informational, Administrative), ci = T, plots = T, plotDens = T)
ggstatsplot::ggscatterstats(online, x = Informational_Duration, y = Administrative_Duration)
ggstatsplot::ggscatterstats(online, x = Informational, y = Administrative)

#Citations#
citation(package = 'readr')
citation(package = 'faraway')
citation(package = 'MASS')
citation(package = 'jmv')