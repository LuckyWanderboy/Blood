rm(list = ls())

Test <- read.csv("~/R/blood/Test.csv")
Train <- read.csv("~/R/blood/Train.csv")

names = names(Train)
blah = paste(names[2],names[3],names[5], sep ="+")

myformula = paste(names[6],"~",blah)

names[6]

myformula

model = glm(myformula, data=Train, family = binomial)

summary(model)

Test$predict = predict(model, Test, type = "response")
