library(UsingR)
data("grades")

table(grades)
table(grades$grade)

mosaicplot(table(grades), main="Mosaic Plot of Grades")

plot(grades)
barplot(table(grades), main="Bar Plot of Grades")

punif(1:9, 2, 10)

# Esercizio 2
data("rivers")
# Convert miles to km
milesToKm <- 1.60934