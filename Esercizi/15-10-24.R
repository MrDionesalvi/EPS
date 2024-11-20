
data("ToothGrowth")


oj <- ToothGrowth$len[ToothGrowth$supp == "OJ"]
vc <- ToothGrowth$len[ToothGrowth$supp == "VC"]

mean(oj)
mean(vc)

median(oj)
median(vc)

hist(oj)
density(oj) -> d
plot(d$x, d$y, type="l", col="red", lwd=2, xlab="Length", ylab="Density")
#lines(density(vc)$x, density(vc)$y, col="blue", lwd=2)

density(oj, bw = 1) -> d1
lines(d1$x, d1$y, type="l", col="yellow", lwd=2, xlab="Length", ylab="Density")


density(oj) -> doj
density(vc) -> dvc
plot(doj$x, doj$y, type="l", col="red", lwd=2, xlab="Length", ylab="Density")
lines(dvc$x, dvc$y, col="blue", lwd=2)

boxplot(oj, vc)


#######

data(babies)

babies$smoke[babies$smoke == 9] <- NA
smoke_factor <- as.factor(babies$smoke)
levels(smoke_factor) <- c("never", "now", "until_p", "once")
levels(smoke_factor)
sum(is.na(smoke_factor))

babies$wt[babies$wt == 999] <- NA
babies$wt_gr <- babies$wt * 28.3495


boxplot(babies$wt_gr ~ smoke_factor, col=c("red", "blue", "green", "yellow"))

#boxplot(babies$ed  ~ babies$race) Educazione in base alla razza

mean(babies$wt_gr[smoke_factor == "now"], na.rm=TRUE)
#####

data("fat")
plot(fat$wrist, fat$neck, xlab="Wrists", ylab="Hips", col="red") # Molto correlato
cor(fat$wrist, fat$neck) # cor(fat$wrist, fat$neck, method="spearman")

plot(fat$abdomen, fat$ankle, xlab="Abdomen", ylab="Neck", col="blue") # Medio correlato
cor(fat$abdomen, fat$ankle)

plot(fat$age, fat$abdomen, xlab="Age", ylab="Abdomen", col="green") # Poco correlato
cor(fat$age, fat$abdomen)


####
library(MASS)
data("Animals")

plot(Animals$body, Animals$brain, xlab="Body", ylab="Brain", col="red")
cor(Animals$body, Animals$brain)
cor(Animals$body, Animals$brain, method="spearman")
