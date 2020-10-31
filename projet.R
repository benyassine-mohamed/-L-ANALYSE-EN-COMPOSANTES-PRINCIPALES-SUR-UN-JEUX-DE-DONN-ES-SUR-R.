rm(list = ls())
library(ade4)
View(seconde)
library(FactoMineR)
data(seconde)
#STAT ÉlÉMENTAIRES

#MOYENNE
mean(seconde$HGEO)
mean(seconde$FRAN)
mean(seconde$PHYS)
mean(seconde$MATH)
mean(seconde$BIOL)
mean(seconde$ECON)
mean(seconde$ANGL)
mean(seconde$ESPA)
#etendu 
max(seconde$HGEO)-min(seconde$HGEO)
max(seconde$FRAN)-min(seconde$FRAN)
max(seconde$PHYS)-min(seconde$PHYS)
max(seconde$MATH)-min(seconde$MATH)#très large
max(seconde$BIOL)-min(seconde$BIOL)#moyen
max(seconde$ECON)-min(seconde$ECON)
max(seconde$ANGL)-min(seconde$ANGL)
max(seconde$ESPA)-min(seconde$ESPA)
#ecart type
sd(seconde$ESPA)
sd(seconde$HGEO)
sd(seconde$FRAN)
sd(seconde$PHYS)
sd(seconde$MATH)
sd(seconde$BIOL)
sd(seconde$ECON)
sd(seconde$ANGL)


#statistique descriptive

summary(seconde)

#matrice de coorélation 

Mcorrel=cor(seconde)

#valeurs propres et visualisation

pca<- PCA(seconde,scale.unit=TRUE, graph = FALSE)
pca$eig 

# Affiche les valeurs propres/contributions des axes factoriels

pca$eig # Affiche les valeurs propres/contributions des axes factoriels

pca$var$coord  # Affiche les coordonnées des variables

pca$var$cor #Affiche les corrélations variables - axes factoriels

pca$var$cos2 #qualité de projection des variables

pca$var$contrib # contribution des variables aux axes factoriels

pca$ind$coord 

# coordonn?es des individus dans le plan factoriel

pca$ind$cos2 # Qualité de représentation des individus

pca$ind$contrib # Contribution des individus aux axes factoriels

cor?<- cor(seconde)
#
names(pca)

pca$svd
pca$eig

#eboulis
inertie<-pca$eig/sum(pca$eig)*100
barplot(pca$eig[,1],ylab="% d'inertie")
title("Éboulis des valeurs propres en %")


pca$ind
z<- dudi.pca(seconde, center = T, scale = T, scannf = F)
z
cl1<-z$li[,1]
cl2<-z$li[,2]

#le plan des individus

plot(cl1,cl2,type="n",main="Les individus",xlim=c(-7,7))
abline(h=0,v=0)
text(cl1,cl2,row.names(z$li))
pca
pca$call$row.w
pca$var
#variable cercle
cc1<-z$co[,1]
cc2<-z$co[,2]
plot(cc1,cc2,type="n", main="Les variables", xlim=c(-1,1), ylim=c(-1,1), asp=1, ylab= "Comp2 71.7%", xlab= "Comp1 12.6%")
abline(h=0,v=0)
text(cc1,cc2,row.names(z$co)) #plan
symbols(0,0,circles=1,inches=FALSE,add=TRUE) #cercle de corrélation
plot.PCA(pca, axes=c(1, 2), choix="var")
plot.PCA(pca, axes=c(1, 2), choix="ind")
dimdesc(pca, axes=c(1,2))
pca$var
pca$ind

#variable cercle dimension 1 et 3
cc1<-z$co[,1]
cc2<-z$co[,3]
plot(cc1,cc2,type="n", main="Les variables", xlim=c(-1,1), ylim=c(-1,1), asp=1, ylab= "Comp2 71.7%", xlab= "Comp1 12.6%")
abline(h=0,v=0)
text(cc1,cc2,row.names(z$co)) #plan
symbols(0,0,circles=1,inches=FALSE,add=TRUE) #cercle de corrélation
plot.PCA(pca, axes=c(1, 3), choix="var")
plot.PCA(pca, axes=c(1, 3), choix="ind")
dimdesc(pca, axes=c(1,3))
pca$var
pca$ind





