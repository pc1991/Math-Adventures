##Cirrhosis Data from 1974 to 1984##
##Courtesy: fedesoriano of Kaggle##
##©The Nerd Chronicles. All Rights Reserved##

#Loading Packages & Excel File##
library(readr)
cirrhosis <- read_csv("cirrhosis.csv")

#Checking specifications of the data#
spec(cirrhosis)

#Editing & Cleaning up the data#
cirrhosis$Status[cirrhosis$Status == "D"] <- 0
cirrhosis$Status[cirrhosis$Status == "CL"] <- 1
cirrhosis$Status[cirrhosis$Status == "C"] <- 2

cirrhosis$Drug[cirrhosis$Drug == "D-penicillamine"] <- 1
cirrhosis$Drug[cirrhosis$Drug == "Placebo"] <- 2

cirrhosis$Sex[cirrhosis$Sex == "M"] <- 1
cirrhosis$Sex[cirrhosis$Sex != "M"] <- 0

cirrhosis$Ascites[cirrhosis$Ascites == "Y"] <- 1
cirrhosis$Ascites[cirrhosis$Ascites != "Y"] <- 0

cirrhosis$Hepatomegaly[cirrhosis$Hepatomegaly == "Y"] <- 1
cirrhosis$Hepatomegaly[cirrhosis$Hepatomegaly != "Y"] <- 0

cirrhosis$Spiders[cirrhosis$Spiders == "Y"] <- 1
cirrhosis$Spiders[cirrhosis$Spiders != "Y"] <- 0

cirrhosis$Edema[cirrhosis$Edema == "N"] <- 0
cirrhosis$Edema[cirrhosis$Edema == "S"] <- 1
cirrhosis$Edema[cirrhosis$Edema == "Y"] <- 2

#Correlation Matrix#
library(jmv)
corrMatrix(cirrhosis, vars = vars(N_Days,Alk_Phos), ci = T, plots = T)
ggstatsplot::ggscatterstats(cirrhosis, x = N_Days, y = Alk_Phos)

#Linear Model#
a <- lm(ID ~ N_Days + Status + Stage, data = cirrhosis)
summary(a)

p <- predict(a, type = "response")
plot(p, xlab = "Patient", ylab = "Condition")
qqnorm(p)
qqline(p)

#Checking RMSE#
rmse <- function(x,y)sqrt(mean(x-y)^2)
rmse(p,cirrhosis$ID)

#Citations#
citation(package = 'readr')
citation(package = 'jmv')