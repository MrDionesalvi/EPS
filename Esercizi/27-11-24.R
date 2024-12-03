library(UsingR)
data("babies")


babies$eta <- babies$age
babies$eta[babies$age == 99] <- NA
babies$etaP <- babies$dage
babies$etaP[babies$dage == 99] <- NA

mean(babies$eta, na.rm = TRUE) -> etaMean
mean(babies$etaP, na.rm = TRUE) -> etaPMean

t.test(babies$etaP, babies$eta, paired = TRUE, conf.level = 0.95)

babies$diff <- babies$etaP - babies$eta
hist(babies$diff, breaks = 20, col = "lightblue", main = "Histogram of differences", xlab = "Difference in ages")
var(babies$diff, na.rm = TRUE)
sd(babies$diff, na.rm = TRUE)

babies$inc[babies$inc == 99] <- NA
babies$inc[babies$inc == 98] <- NA
babies$race[babies$race == 99] <- NA
babies$race[babies$drace == 99] <- NA
babies$ed[babies$ed == 9] <- NA
#Display the income for the race
boxplot(babies$inc ~ babies$race)
boxplot(babies$ed ~ babies$race, col = "lightblue")

#babies$race if race > 0 and race < 5
subset1 <- babies$race > 0 & babies$race < 5

mom <- c(sum(subset1, na.rm = TRUE), sum(babies$race == 6, na.rm = TRUE), sum(babies$race == 7, na.rm = TRUE), sum(babies$race == 8, na.rm = TRUE), sum(babies$race == 9, na.rm = TRUE))
names(mom) <- c("White", "Mex", "Black", "Asian", "Mixed")

subset2 <- babies$drace > 0 & babies$drace < 5
father <- c(sum(subset2, na.rm = TRUE), sum(babies$drace == 6, na.rm = TRUE), sum(babies$drace == 7, na.rm = TRUE), sum(babies$drace == 8, na.rm = TRUE), sum(babies$drace == 9, na.rm = TRUE))
names(father) <- c("White", "Mex", "Black", "Asian", "Mixed")

barplot(rbind(mom, father), beside = TRUE, col = c("lightblue", "lightgreen"), legend = c("Mother", "Father"))

babies$race[babies$race <= 5] = 5
babies$drace[babies$drace <= 5] = 5

boxplot(babies$drace ~ babies$race, col = "lightblue")



## esercizio 2
mean(babies$eta, na.rm = TRUE)
hist(babies$eta, breaks = 20, col = "lightblue", main = "Histogram of ages", xlab = "Age")

t.test(babies$eta, alternative = "greater", mu = 26, conf.level = 0.95)

# Per essere nel 10% del range dell'età quanti anni devi avere
quantile(babies$eta, 0.01, na.rm = TRUE)
