
Test  <- read.csv("Test.csv")
Train <- read.csv("Train.csv")

myformula <- Made.Donation.in.March.2007 ~ . ^2 

  
Train <- within(Train, {
  
  Total.Volume.Donated..c.c.. <- NULL
  More.than.5year <- Months.since.First.Donation > 60
  
  
})  
  
  
model = glm(myformula, data=Train, family = binomial)

summary(model)

Test$predict = predict(model, Test, type = "response")
