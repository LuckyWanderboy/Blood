

blood1 = read.csv("blood.csv")



as.factor(blood1$Donated)
blood1$Donated = as.factor(blood1$Donated)
str(blood1)
g = ggplot(blood1, aes(Donated, VolumeBlood))
g + geom_boxplot()

g = ggplot(blood1, aes(Donated, LastDonation))
