#Obesity Issues Part 1#
#Courtesy: Ankur Singh of Kaggle#
##©The Nerd Chronicles. All Rights Reserved##

#Loading Packages & Excel File##
library(readr)
library(tidyverse)
obese <- read.csv("Obesity2.csv", stringsAsFactors = FALSE)
View(obese)
df = subset(obese, select = -c(Email,Name,Start.time,Completion.time))
View(df)
names(df)

#Cleaning up & organizing the original table#
table(df$Gender)
df$Gender[df$What.is.your.gender.. != "Female"] <- 1
df$Gender[df$What.is.your.gender.. == "Female"] <- 0

table(df$AgeGroup)
df$AgeGroup[df$Select.your.age.group.. == "Less than 20 years old."] <- 1
df$AgeGroup[df$Select.your.age.group.. == "20- 30 years old ."] <- 2
df$AgeGroup[df$Select.your.age.group.. == "20- 35 years old ."] <- 2
df$AgeGroup[df$Select.your.age.group.. == "36 - 60 years old."] <- 3
df$AgeGroup[df$Select.your.age.group.. == "40 - 60 years old."] <- 3
df$AgeGroup[df$Select.your.age.group.. == "Above 60 years old."] <- 4

table(df$Maintenance)
df$Maintenance[df$Do.you.try.to.maintain.balance.diet.. == "Yes"] <- 1
df$Maintenance[df$Do.you.try.to.maintain.balance.diet.. == "Maybe"] <- 0
df$Maintenance[df$Do.you.try.to.maintain.balance.diet.. == "No"] <- -1

table(df$Junkies)
df$Junkies[df$How.often.do.you.eat.junk.food.at.your.home.or.your.workplace.. == "Once a week "] <- 1
df$Junkies[df$How.often.do.you.eat.junk.food.at.your.home.or.your.workplace.. == "2-4 times per week"] <- 2
df$Junkies[df$How.often.do.you.eat.junk.food.at.your.home.or.your.workplace.. == "Daily"] <- 3

table(df$Weight)
df$Weight[df$Select.the.weight.category.you.belong.to.. == "Slim"] <- 1
df$Weight[df$Select.the.weight.category.you.belong.to.. == "Healthy"] <- 2
df$Weight[df$Select.the.weight.category.you.belong.to.. == "Fit"] <- 3
df$Weight[df$Select.the.weight.category.you.belong.to.. == "Underweight"] <- -1
df$Weight[df$Select.the.weight.category.you.belong.to.. == "Overweight"] <- -2
df$Weight[df$Select.the.weight.category.you.belong.to.. == "Obese"] <- -3

table(df$Plants)
df$Plants[df$How.often.you.eat.fruits.and.salad.. == "Sometimes"] <- 1
df$Plants[df$How.often.you.eat.fruits.and.salad.. == "Often "] <- 2
df$Plants[df$How.often.you.eat.fruits.and.salad.. == "Always"] <- 3

table(df$Beverages)
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;"] <- 1
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Coffee;"] <- 1
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Juices;"] <- 1
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Green tea ;"] <- 1
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;"] <- 1
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Cold drinks;"] <- 1
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Juices;Coffee;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Juices;Cold drinks;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Coffee;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Tea ;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Coffee;Milk;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Coffee;Juices;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Juices;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Milk;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Cold drinks;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Green tea ;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Green tea ;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Cold drinks;Milk;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Coffee;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Green tea ;Juices;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Juices;Milk;"] <- 2
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Green tea ;Juices;Milk;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Coffee;Milk;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Juices;Milk;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Green tea ;Milk;Juices;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Coffee;Green tea ;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Juices;Milk;Coffee;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Cold drinks;Tea ;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Juices;Tea ;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Coffee;Cold drinks;Juices;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Cold drinks;Tea ;Juices;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Juices;Green tea ;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Milk;Cold drinks;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Cold drinks;Juices;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Coffee;Tea ;Juices;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Coffee;Green tea ;Milk;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Coffee;Tea ;Milk;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Milk;Coffee;"] <- 3
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Coffee;Juices;Milk;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Coffee;Green tea ;Milk;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Juices;Coffee;Cold drinks;Milk;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Juices;Milk;Cold drinks;Coffee;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Coffee;Green tea ;Juices;Milk;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Juices;Green tea ;Tea ;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Milk;Cold drinks;Coffee;Green tea ;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Cold drinks;Juices;Milk;Coffee;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Coffee;Cold drinks;Juices;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Milk;Cold drinks;Green tea ;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Green tea ;Milk;Tea ;Juices;"] <- 4
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Coffee;Cold drinks;Juices;Milk;"] <- 5
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Coffee;Juices;Milk;Green tea ;Cold drinks;Tea ;"] <- 6
df$Beverages[df$What.sort.of.baverages.you.take.very.often.or.on.daily.basis....Choose.all.options.that.apply... == "Tea ;Coffee;Green tea ;Cold drinks;Juices;Milk;"] <- 6

table(df$Alcohol)
df$Alcohol[df$How.often.you.take.alcoholic.beverages.. == "Never"] <- -1
df$Alcohol[df$How.often.you.take.alcoholic.beverages.. == "Sometimes"] <- 0
df$Alcohol[df$How.often.you.take.alcoholic.beverages.. == "Frequently"] <- 1

table(df$Diabetes)
df$Diabetes[df$Have.you.ever.diagnosed.with.any.of.the.conditions.before....Diabetes == "No"] <- -1
df$Diabetes[df$Have.you.ever.diagnosed.with.any.of.the.conditions.before....Diabetes == "Maybe"] <- 0
df$Diabetes[df$Have.you.ever.diagnosed.with.any.of.the.conditions.before....Diabetes == "Yes"] <- 1

table(df$Hypo)
df$Hypo[df$Hypothyroidism == "No "] <- -1
df$Hypo[df$Hypothyroidism == "May be"] <- 0
df$Hypo[df$Hypothyroidism == "Yes"] <- 1

table(df$PCOD)
df$PCOD[df$PCOD..Polycystic.ovarian.disorder. == "No"] <- -1
df$PCOD[df$PCOD..Polycystic.ovarian.disorder. == "May be "] <- 0
df$PCOD[df$PCOD..Polycystic.ovarian.disorder. == "Yes"] <- 1

table(df$Genes)
df$Genes[df$Are.you.aware.that.genes.play.a.role.in.Obesity.. == "No"] <- -1
df$Genes[df$Are.you.aware.that.genes.play.a.role.in.Obesity.. == "Maybe"] <- 0
df$Genes[df$Are.you.aware.that.genes.play.a.role.in.Obesity.. == "Yes"] <- 1

table(df$Causes)
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;"] <- 1
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Sedentary lifestyle;"] <- 1
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Less physical activities;"] <- 1
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;"] <- 1
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;"] <- 1
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;"] <- 2
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;"] <- 2
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Less physical activities;"] <- 2
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Less physical activities;Over eating ;"] <- 2
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Sedentary lifestyle;Over eating ;"] <- 2
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Less physical activities;Genes;"] <- 2
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Less physical activities;Genes;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;Sedentary lifestyle;Disorders like diabetes,PCOD, thyroidism etc.;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Less physical activities;Over eating ;Sedentary lifestyle;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;Genes;Sedentary lifestyle;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Sedentary lifestyle;Less physical activities;Over eating ;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;Less physical activities;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;Sedentary lifestyle;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Sedentary lifestyle;Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;Less physical activities;Over eating ;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;Over eating ;Less physical activities;"] <- 3
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;Disorders like diabetes,PCOD, thyroidism etc.;Over eating ;Less physical activities;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;Less physical activities;Sedentary lifestyle;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Less physical activities;Sedentary lifestyle;Disorders like diabetes,PCOD, thyroidism etc.;Genes;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Sedentary lifestyle;Less physical activities;Disorders like diabetes,PCOD, thyroidism etc.;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Less physical activities;Sedentary lifestyle;Genes;Disorders like diabetes,PCOD, thyroidism etc.;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;Sedentary lifestyle;Less physical activities;Genes;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;Genes;Over eating ;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;Sedentary lifestyle;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Less physical activities;Genes;Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;Less physical activities;Disorders like diabetes,PCOD, thyroidism etc.;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;Sedentary lifestyle;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Less physical activities;Genes;Sedentary lifestyle;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Less physical activities;Sedentary lifestyle;Genes;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;Sedentary lifestyle;Less physical activities;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;Sedentary lifestyle;Less physical activities;Over eating ;"] <- 4
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;Disorders like diabetes,PCOD, thyroidism etc.;Sedentary lifestyle;Less physical activities;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;Genes;Sedentary lifestyle;Less physical activities;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;Sedentary lifestyle;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Less physical activities;Sedentary lifestyle;Disorders like diabetes,PCOD, thyroidism etc.;Genes;Over eating ;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Sedentary lifestyle;Less physical activities;Over eating ;Genes;Disorders like diabetes,PCOD, thyroidism etc.;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;Sedentary lifestyle;Genes;Less physical activities;Over eating ;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;Less physical activities;Sedentary lifestyle;Disorders like diabetes,PCOD, thyroidism etc.;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Genes;Sedentary lifestyle;Less physical activities;Disorders like diabetes,PCOD, thyroidism etc.;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;Sedentary lifestyle;Less physical activities;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Less physical activities;Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;Sedentary lifestyle;Genes;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;Sedentary lifestyle;Less physical activities;Genes;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;Over eating ;Genes;Sedentary lifestyle;Less physical activities;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;Less physical activities;Genes;Over eating ;Sedentary lifestyle;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Sedentary lifestyle;Less physical activities;Disorders like diabetes,PCOD, thyroidism etc.;Genes;Over eating ;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Disorders like diabetes,PCOD, thyroidism etc.;Sedentary lifestyle;Over eating ;Less physical activities;Genes;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Sedentary lifestyle;Less physical activities;Over eating ;Disorders like diabetes,PCOD, thyroidism etc.;Genes;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Genes;Sedentary lifestyle;Disorders like diabetes,PCOD, thyroidism etc.;Over eating ;Less physical activities;"] <- 5
df$Causes[df$What.do.you.think.which.of.the.following.cause.Obesity...Select.all.options.that.apply. == "Sedentary lifestyle;Disorders like diabetes,PCOD, thyroidism etc.;Genes;Over eating ;Less physical activities;"] <- 5

table(df$SimilarConditions)
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "None of the above;"] <- 0
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Siblings;"] <- 1
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Son or daughter;"] <- 1
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Grandparents ;"] <- 1
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Parents;"] <- 1
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Relative;"] <- 1
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Parents;Siblings;"] <- 2
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Grandparents ;Son or daughter;"] <- 2
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Grandparents ;Parents;"] <- 2
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Grandparents ;Relative;"] <- 2
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Parents;Relative;"] <- 2
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Parents;Grandparents ;"] <- 2
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Son or daughter;Relative;"] <- 2
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Grandparents ;Siblings;"] <- 2
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Grandparents ;Parents;Siblings;None of the above;"] <- 3
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Grandparents ;Parents;Siblings;"] <- 3
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Grandparents ;Parents;Relative;"] <- 3
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Relative;Siblings;"] <- 2
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Parents;Grandparents ;Relative;"] <- 3
df$SimilarConditions[df$Who.among.the.following.in.your.family..suffer.from.similar.condition. == "Relative;Parents;Siblings;"] <- 3

#Running and comparing linear models#
library(randomForest)
library(faraway)
library(corrplot)

#Regular linear model#
g <- lm(ID ~ Gender + Maintenance + Junkies + Weight + Plants + Beverages + Alcohol + Diabetes + Hypo + PCOD + Genes + Causes + SimilarConditions, data = df)
summary(g)

#Predicting the oldschool linear model# 
p <- predict(g, type = "response")
plot(p, xlab = "Participant", ylab = "Awareness Level")   
hist(p, ylim = c(0,6), breaks = 130)
qqnorm(p)
qqline(p)

#Correlation Matrix of the revamped table#
library(jmv)
corrMatrix(df, vars = vars(ID, Causes), ci = T, plots = T, plotDens = T)
ggstatsplot::ggscatterstats(df, x = ID, y = Causes)

#RMSE of the oldschool linear model#
rmse <- function(x,y) sqrt(mean(x-y)^2)
rmse(p,obese$ID) 

#Now we're going to generate a new linear model from the same response and predictors using randomForest#
r <- randomForest(ID ~ Gender + Maintenance + Junkies + Weight + Plants + Beverages + Alcohol + Diabetes + Hypo + PCOD + Genes + Causes + SimilarConditions, data = df)
summary(r)

#Predictor of the randomForest model#
rp <- predict(r, type = "response")
plot(rp, xlab = "Participant", ylab = "Awareness Level")
hist(rp, ylim = c(0,6), breaks = 130)
qqnorm(rp)
qqline(rp)

#RMSE of the randomForest model#
rmse(rp,obese$ID)

#Based on the data given with minimal error, oldschool wins!!!!#

#Citations#
citation(package = 'readr')
citation(package = 'tidyverse')
citation(package = 'randomForest')
citation(package = 'faraway')
citation(package = 'corrplot')
