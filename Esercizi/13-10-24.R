data("rivers")
# Convert miles to km
milesToKm <- 1.60934

soglia <- 500 * milesToKm

# Point 1
riverskm <- rivers * milesToKm
sum(riverskm < soglia)/length(riverskm)

# Point 2
meanRivers <- mean(riverskm)
sum(riverskm < meanRivers)/length(riverskm)

# Point 3 | 75* Percentile campioanrio
quantile(riverskm)


### Esercizio 2

data("nym.2002")

# Percentuale di persone che hanno raggiunto il traguardo sotto 3 ore
soglia <- 180
sum(nym.2002$time < soglia)/length(nym.2002$time) * 100

# Top 10%
quantile(nym.2002$time, 0.1)

# Top 25%
quantile(nym.2002$time, 0.25)

# Top Bottom 10%
quantile(nym.2002$time, 0.9)

### Charts

barplot(table(nym.2002$gender))
dotchart(table(nym.2002$gender), main="NY Marathon 2002", xlab="Time", ylab="Frequency", col="blue")

barplot(table(nym.2002$home))
which.max(table(nym.2002$home))


### Esercizio 3
data("babies")

babies$smoke[babies$smoke == 9] <- NA
smoke_factor <- as.factor(babies$smoke)
levels(smoke_factor)

barplot(table(smoke_factor))
