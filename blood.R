library(data.table)
library(corrplot)
library(party)

#### Logistic
traindata <- read.csv("~/Dropbox/Nexus/Selflearn/train.csv")
traindata <- read_feather("~/Dropbox/Nexus/Selflearn/train.csv")

testdata <- read.csv("~/Dropbox/Nexus/Selflearn/test.csv")
setDT(traindata)
setDT(testdata)

colnames(traindata) <- c("X", "MonthsLastDonation", "Donations", "TotalCCvolume", "MonthssinceFirst", "Target")
colnames(testdata) <- c("X", "MonthsLastDonation", "Donations", "TotalCCvolume", "MonthssinceFirst")

traindata[, regulardonor := ifelse(MonthsLastDonation < 5 & MonthssinceFirst > 6, 1, 0)]
traindata[, ratiodonated := MonthssinceFirst/Donations]

prop.table(table(traindata$Target))
prop.table(table(traindata[ratiodonated <= 6.6 & regulardonor == 1]$Target))

table(traindata[ratiodonated > 6.6 & regulardonor == 1]$Target)

testdata[, regulardonor := ifelse(MonthsLastDonation < 5 & MonthssinceFirst > 6, 1, 0)]


plot(ctree(Target ~ regulardonor + ratiodonated, data = traindata))

corrplot(cor(traindata))


logmodel <- glm(Target ~  regulardonor + ratiodonated, data = traindata, family = "binomial")

summary(logmodel)


testdata$preds <- predict(logmodel, testdata, type = "response")

final <-testdata[, .(X, `Made Donation in March 2007` = preds)]

write.csv(x = final, file = "~/Dropbox/Nexus/submit.csv", row.names = FALSE)

###Xgboost
