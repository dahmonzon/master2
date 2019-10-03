library(MASS)

# Charger les données dans l'environnement
# (Morphological Measurements on Leptograpsus Crabs)
data(crabs)
# Visualisation de la structure de la table
str(crabs)

# Répartition des données 'Frontal lob size' selon les espèces
# (species - "B" or "O" for blue or orange)
dataB <- crabs[which(crabs$sp=="B"), 4]
dataO <- crabs[which(crabs$sp=="O"), 4]

# Histogramme avec plot
hist(dataB, col = "magenta")
# Histogramme sans plot
hist(dataB, plot = FALSE)

# Différents histogrammes suivant les breaks
par(mfrow=c(2,2))
hist(dataB, col = "grey")
hist(dataB, col = "blue", breaks = seq(from = 6, to = 22, by = 2))
hist(dataB, col = "magenta", breaks = 20)
hist(dataB, col = "green", breaks = seq(from = 6, to = 22, by = 1))
par(mfrow=c(1,1))

# Différents histogrammes suivant le nombre de classes
myX = c(14, 15, 17, 19, 21, 23, 25, 26, 27, 28, 29, 31, 
        38, 39, 41, 44, 45, 49, 51, 53)
par(mfrow=c(2,2))
hist(myX, col = "grey", main = "Default (... is Sturges!)")
hist(myX, col = "blue", breaks = "Sturges", 
     main = paste("Using Sturges (K =", nclass.Sturges(myX), ")", sep = ""))
hist(myX, col = "magenta", breaks = "Scott", 
     main = paste("Using Scott (K =", nclass.scott(myX), ")", sep = ""))
hist(myX, col = "green", breaks = "FD", 
     main = paste("Using FD (K = ", nclass.FD(myX), ")", sep = ""))
par(mfrow=c(1,1))

# Nombre de classes selon le nombre d'observations
par(mfrow=c(3,3))
for(n in c(100, 1000, 10000)){
  myX<-rnorm(n)  
  hist(myX, col = "grey", 
       main = paste("Using Sturges (K =", nclass.Sturges(myX), ")", sep = ""))
  hist(myX, col = "yellow", breaks = "Scott", 
       main = paste("Using Scott (K =", nclass.scott(myX), ")", sep = ""))
  hist(myX, col = "magenta", breaks = "FD", 
       main = paste("Using FD (K =", nclass.FD(myX), ")", sep = ""))
}
par(mfrow=c(1,1))

# Superposition des quantiles à l'histogramme
hist(dataB)
abline(v = quantile(dataB), col = "red", lwd = 3)

# Histogramme et courbe de densité
hist(dataB, probability = TRUE,
     ylim = range(0, 0.2), main = "Frontal Lobe Size",
     xlab = "FL (mm)", ylab = "Frequency per mm")

lines(density(dataB, bw = 0.25), col = "orange", lwd = 3, lty = 4)
lines(density(dataB, bw = 0.5), col = "red", lwd = 3, lty = 4)
lines(density(dataB, bw = 2), col = "magenta", lwd = 3, lty = 4)
lines(density(dataB), col = "blue", lwd = 3)

ds = density(dataB)

# Add a legend
legend("topleft", 
       legend = c("Density bw 0.25", "Density bw 0.5", 
                  "Density bw 2", "Density bw 1.082"),
       col = c("orange", "red", "magenta", "blue"), 
       lty = c(4, 4, 4, 1), lwd = rep(3, 4), cex = 0.8)


