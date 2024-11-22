library(UsingR)
data("stud.recs")
options(digits=10)

#Find 90% confidence interval fro the mean of sat.m
hist(stud.recs$sat.m)
boxplot(stud.recs$sat.m)

mean(stud.recs$sat.m)

t.test(stud.recs$sat.m, conf.level = 0.90) -> ic
ic$conf.int


## Ex 2

data("normtemp")
normtemp$temperatureC <- (normtemp$temperature - 32) * 5/9
plot(normtemp$temperatureC)
hist(normtemp$temperatureC)

soglia <- 98.6
sogliaC <- (soglia - 32) * 5/9

t.test(normtemp$temperatureC, conf.level = 0.9)


## Ex 3
binom.test(5, 100, conf.level = 0.95)

pcappello <- 4/5
n1 <- 5
n2 <- 100
n3 <- 1000

binom.test(pcappello*n1, n1, conf.level = 0.9)
binom.test(pcappello*n2, n2, conf.level = 0.9)
binom.test(pcappello*n3, n3, conf.level = 0.9)
