library(UsingR)
data("grades")

table(grades)
table(grades$grade)

mosaicplot(table(grades), main="Mosaic Plot of Grades")

plot(grades)
barplot(table(grades), main="Bar Plot of Grades")

punif(1:9, 2, 10)

rm(list=ls())

## legge dei grandi numeri sulla media

N <- 10
dati <- rexp(N,2)

media10 <- mean(dati)

N <- 30
dati <- rexp(N,2)

media30 <- mean(dati)

N <- 100
dati <- rexp(N,2)

media100 <- mean(dati)

N <- 1000
dati <- rexp(N,2)

media1000 <- mean(dati)

N <- 10000
dati <- rexp(N,2)

media10000 <- mean(dati)


plot(c(10,30,100,1000,10000),c(media10,media30,media100,media1000,media10000))
abline(h=0.5,col="red")

###tanti
N <- seq(5,10^4,10)
trueMedia <- 4
medie <- numeric(length(N))
posizione <- 1
for (indice in N){
  medie[posizione] <- mean(rexp(indice,1/trueMedia))
  posizione <- posizione + 1
}
plot(N,medie,type="l")
abline(h=trueMedia,col="red")
###tanti di più
N <- seq(5,10^5,100)
trueMedia <- 4
medie <- numeric(length(N))
posizione <- 1
for (indice in N){
  medie[posizione] <- mean(rexp(indice,1/trueMedia))
  posizione <- posizione + 1
}
plot(N,medie,type="l")
abline(h=trueMedia,col="red")

## stessa cosa per la varianza campionaria

###tanti
N <- seq(5,10^4,10)
trueMedia <- 4
p <- 1/trueMedia
trueVar <- 1/(p^2)
varianze <- numeric(length(N))
posizione <- 1
for (indice in N){
  varianze[posizione] <- var(rexp(indice,1/trueMedia))
  posizione <- posizione + 1
}
plot(N,varianze,type="l")
abline(h=trueVar,col="red")

## teorema del limite centrale

R <- 1000 # quanti campioni casuali di taglia N genero
N <- 100 # dimensione del campione casuale
trueMedia <- 0.9
p <- 1/trueMedia

trueSD <- sqrt(trueMedia*(1-trueMedia))


dati <- matrix(nrow = N, ncol = R)
medie <- numeric(R)
for (indice in 1:R){
  dati[,indice] <- rbinom(N,1,trueMedia)
  medie[indice] <- mean(dati[,indice])
}

hist(medie)

x <- seq(min(medie), max(medie), 0.001)
mediaNormale <- trueMedia
sdNormale <- trueSD/sqrt(N)
hist(medie,freq = FALSE)
lines(x,dnorm(x,mean = mediaNormale, sd = sdNormale),col="red")

