# Recherche des données
getwd()
data <- read.csv("data (2).csv", encoding = "UTF-8", dec = ".")
str(data)
rownames(data) <- data [,2]
data <- data [, 3:6]
pairs(data)

# Normaliser sa donnée
summary(data)
rownames (data)

# supression des communes et des NA  
suppr <- c(6,16)
data <- data[-suppr,]
data[is.na(data$Taux.de.chômage.annuel.moyen.2020 ),]
data <- data [!is.na(data$Taux.de.chômage.annuel.moyen.2020),]

# Analyse des valeurs de dispersion/centrales
coeffVariation <- function (x) {mean(x)/sd(x)}
coeff <- sapply(data, coeffVariation)
sd <- sapply(data, sd)
moy <- sapply(data, mean)
tab <- cbind(moy,sd, coeff)
round(tab,0)

# Centrage et réduction
data_cr <- scale (data)
pairs(data_cr)

# axes factoriels 
acp <- prcomp(data_cr)
# par défaut direction négative ou inverse
acp$rotation <- -1 * acp$rotation
acp$rotation

# importations des axes
acp$sdev^2 / sum(acp$sdev^2)
head(acp$x*-1)
biplot(acp, scale = 0)

# centrage et réduction
data.cr <- scale(data, center = T, scale = T)
# matrice des distances entre les individus
data.d <- dist(data.cr)
# classification
cah <- hclust(data.d)
# dendogramme
plot(cah)

# matérialisation des groupes
rect.hclust(cah, k = 3)
groupes.cah <- cutree(cah, k = 3)
liste <- sort(groupes.cah)
acp <- princomp(data.cr, cor = T, scores = T)
par(bg = "lightgrey", mar = c(1,1,1,1))
plot(acp$scores[,1],acp$scores[,2], type = "p")
text(acp$scores[,1],acp$scores[,2],col=c(topo.colors(3))[groupes.cah],cex
     =1,labels=rownames(data))

