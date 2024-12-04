# 10.11

dati <- read.csv("Documents/Uni/EPS/Esercizi/Datasets-20241204/accident.csv")
load("Documents/Uni/EPS/Esercizi/Datasets-20241204/accident.RData")


accident$AccidentType <- as.factor(accident$AccidentType)
accident$Age <- as.factor(accident$Age)

barplot(table(accident$AccidentType), col = "red",
        main = "Accident Type", xlab = "Type", ylab = "Frequency")
barplot(table(accident$Age), col = "red",
        main = "Age", xlab = "Age", ylab = "Frequency")


mosaicplot(table(accident$AccidentType, accident$Age),
           main = "Accident Type vs Age", xlab = "Type", ylab = "Age",
           color = c("red", "blue", "green", "yellow", "orange"))

# mosaicplot without "none" type
mosaicplot(table(accident)[1:2, ],
           main = "Accident Type vs Age", xlab = "Type", ylab = "Age",
           color = c("red", "blue", "green", "yellow", "orange"))


test <- chisq.test(table(accident))
test$p.value

test$expected

# Lege di Cochran
# H0: le variabili sono indipendenti
# H1: le variabili non sono indipendenti
# Se p-value < alpha, rifiuto H0
# Se p-value > alpha, non rifiuto H0
# Almeno l'80% delle celle deve avere frequenza attesa > 5
#Tutte le frequenze >= 1

# In questo caso il 27% delle celle ha frequenza attesa < 5
#   (major(18-25), major(over65) etc..)
# Quindi la regola non è verificata


####
#install.packages("readxl") # nolint: commented_code_linter.
library(readxl)
settimana <- readxl::read_excel("Documents/Uni/EPS/Esercizi/Datasets-20241204/Births.xlsx") # nolint


giorno <- as.factor(settimana$DISCHARGED)
table(giorno)
barplot(table(giorno), col = "lightblue",
        main = "Day of the week", xlab = "Day", ylab = "Frequency")

test2 <- chisq.test(table(giorno), p = rep(1 / 7, 7))
test2$p.value
test2$expected

# H0: le variabili sono indipendenti
# H1: le variabili non sono indipendenti
# Se p-value < alpha (0.001), rifiuto H0
# Se p-value > alpha (0.001), non rifiuto H0
# Almeno l'80% delle celle deve avere frequenza attesa > 5
#Tutte le frequenze >= 1

# In questo caso tutte le frequenze >= 1 && frequenza attesa > 5
# Quindi la regola è verificata

## Esercizio 3

giorni_arma <- c(74, 60, 66, 71, 51, 66, 76)
test3 <- chisq.test(giorni_arma, p = rep(1 / 7, 7))
test3$p.value
test3$expected

# Non posso rifiutare H0 perchè p-value > alpha (0.05)
# Legge di Cochran è verificata