#VEGETATION DATA FOR PEA SEM

library(vegan)
library(MASS)

peaveg<-read.csv(file.choose())#PEA VEG MATRIX
attach(peaveg)
dim(peaveg)

peaenv<-read.csv(file.choose())#PEA ENV DATA
attach(peaenv)
dim(peaenv)

peaenvfull2<-read.csv(file.choose())#PEA ENV DATA FULL_updated
attach(peaenvfull2)
dim(peaenvfull2)
names(peaenvfull2)

colnames(peaenvfull)[15] <- "CN_unburnt_0_10"
colnames(peaenvfull2)[1] <- "FIRE"

#Check correlations between environmental/soil data
options(max.print=1000000)
names(peaenvfull2)
correlation_matrix2<-(corr.test(peaenvfull2,use = "pairwise",method="pearson",ci=TRUE,alpha=.05))
correlation_matrix2

## S3 method for class 'mantel.correlog'
plot(x, alpha=0.05, ...)

#First, running example to get an idea on how to do it myself
#Example includes: Tree counts in 1-hectare plots in the Barro Colorado Island and associated site information

data(BCI)	#loads BCI data
str(BCI)
head(BCI)
data(BCI.env)
head(BCI.env)
str(BCI.env)
dim(BCI)	#gives you the dimensions of the data set, (rows, columns)
BCI[1:10,20:25]
BCI[1:50, 71]	#addresses the row and column of the data set
BCI[,71]
sum(BCI$Faramea.occidentalis)
diversity(BCI, index = "shannon")
diversity(peaveg3, index = "shannon")
plots<- diversity(BCI, index = "shannon") 
plots
summary(plots) #gives summary statistics for the plots
median(plots) #gives the median
mean(plots) 

plots2<- diversity(peaveg, index = "shannon") 
plots2
summary(plots2)

#Fisher’s alpha is a measure of diversity that takes into account 
#variability in stem number.
#Fisher’s alpha is supposed to be invariant with sample size
  
fish.a<-fisher.alpha(peaveg, MARGIN = 1)
fish.a	#shows you the values in the object "fish.a" that you made.

bcitot<-apply(BCI, 2, sum) #gives you the total number of individuals for the 50 ha plot
bcitot.a<- fisher.alpha(bcitot, MARGIN = 1) #calculates fisher’s alpha on all 50 ha combined.
bcitot.a

bcitot

x<-1:50	#makes a sequence of numbers 1:50 that represent the hectares
a<-NULL	#sets up an empty object we’ll fill with results
for (i in x){	#tells R to give i each value in the object x
  b<- apply(BCI[1:i,], 2, sum)	#get the sum of i hectares.
  c<- fisher.alpha(b, MARGIN = 1) #work the fisher’s alpha magic on it
  a<- c(a,c)		#stick the new result on the end of the old data
}
plot(x, a)

rar <- rarefy(peaveg3, 20) #gives you the species per 20 individuals sampled for each of 50 ha
plot(rar)
rarsum<-rarefy(bcitot, 20, MARGIN=2) #species per 20 from whole plot, margin is 2 because bcitot has the data as a column and not a row

rarsum

spa <- specaccum(peaveg3)
plot(spa) #plots the species accumulation curve and the confidence intervals for sites.
plot(spa, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue") #males a prettier plot

spa2 <- specaccum(peaveg)
plot(spa2) #plots the species accumulation curve and the confidence intervals for sites.
plot(spa2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue") #males a prettier plot

spi<-specaccum(BCI, method="rarefaction")
plot(spi)

spi2<-specaccum(peaveg, method="rarefaction")
plot(spi2)

plot(spa2, add=TRUE, col=4) 
#You can see that the answers differ slightly.  
#Note how you can add plots.  
#This will become important when comparing among Manu plots, or comparing Manu and BCI.

bci.mds<-metaMDS(BCI, distance = "bray", k = 2, trymax = 20, autotransform =TRUE, noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE) #makes the object bci.mds using Bray-Curtis ordination
bci.mds2<-metaMDS(peaveg3, distance = "bray", k = 2, trymax = 20, autotransform =TRUE, noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE) #makes the object bci.mds using Bray-Curtis ordination
bci.mds2$species

plot(bci.mds2, choices = c(1, 2), type="n") #plots the ordination axes
points(bci.mds2, display = c("sites", "species"))#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
text(bci.mds2, display = c("sites", "species"), cex=1.6)
text(bci.mds2, display = ("species"))

bci.mds3<-metaMDS(peaenv, distance = "bray", k = 2, trymax = 20, autotransform =TRUE, noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE) #makes the object bci.mds using Bray-Curtis ordination
summary(bci.mds3)

bci.mds4<-metaMDS(peaenvfull, distance = "bray", k = 2, trymax = 6, autotransform =TRUE, noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE) #makes the object bci.mds using Bray-Curtis ordination
summary(bci.mds4)

par(mfrow=c(1,1))
forests
plot(bci.mds2, type="n") #plots the ordination axes
points(bci.mds2, display = "sites", col="black",cex=1.6)#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
points(bci.mds2, display = "sites", col="lightblue",cex=1.4)#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
identify(bci.mds2, "sites", labels=forests,col="blue",cex=1.7)
forests<- c("I1","I2","I3","I5","I6")
mtext("I7", side=3,cex=1.4,adj=0.64, line=-16.5,col="black")
mtext("I1", side=3,cex=1.4,adj=0.46, line=-10,col="black")
mtext("I6", side=3,cex=1.6,adj=0.98, line=-10,col="black")
mtext("I5", side=3,cex=1.6,adj=0.37, line=-11,col="black")
mtext("I2", side=3,cex=1.6,adj=0.42, line=-14,col="black")
mtext("I3", side=3,cex=1.6,adj=0.02, line=-11.5,col="black")
text(bci.mds4, display = "sites", cex=1.6)

text(bci.mds4, display = ("species"))
hrtrees<-read.table("hrtrees.txt", header = TRUE, sep = "\t")
manu<- read.table("manu.txt", header = TRUE, sep = "\t")

H <- diversity(peaveg3)
H
simp <- diversity(peaveg3, "simpson")
simp
invsimp <- diversity(peaveg3, "inv")
invsimp

## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(peaveg, 2) - 1
unbias.simp
## Fisher alpha
alpha <- fisher.alpha(peaveg)
## Plot all
pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")

## Species richness (S) and Pielou's evenness (J):
S <- specnumber(peaveg) ## rowSums(BCI > 0) does the same...
S
J <- H/log(S)
J

#TEST FOR ENVIRONMENTAL DATA

data(varespec)
str(varespec)
head(varespec)
data(varechem)
str(varechem)
head(varechem)
vare.cca <- cca(varespec ~ Al + P + K, varechem)
summary(vare.cca)
names(peaenv)
## overall test
anova(vare.cca)

bci.mds3<-metaMDS(peaenvfull, distance = "bray", k = 2, trymax = 20, autotransform =TRUE, noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE) #makes the object bci.mds using Bray-Curtis ordination
summary(bci.mds3)

plot(bci.mds3, choices = c(1, 2), type="n") #plots the ordination axes
points(bci.mds3, display = "sites")#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
text(bci.mds3, display = "sites", cex=1.6)

names(peaenvfull)
colnames(peaenvfull)[1] <- "Fire Legacy"
colnames(peaenvfull)[9] <- "CN.0.10.C1"
colnames(peaenv)[4] <- "LAT"
colnames(peaenvfull)[13] <- "ANF.0.10.C1"
colnames(peaenv)[11] <- "Fisher_div"



write.csv(correlation_matrix, "C:/R/corpeaenv.csv")

#using full dataset to find variables that best explain species composition

names(peaenvfull)#doing all variables, 6 at a time

corr.test(peaenvfull[2:9],peaenvfull[13:20],use = "pairwise",method="spearman",ci=TRUE,alpha=.05)

#wisconsin is function for double standardization of data
sola <- bioenv(wisconsin(peaveg) ~ FIRE + AGB + AGB.change.since.2007 + CN.top.C1 + LongitudeDecimal + N.Dead, peaenvfull)
sola#fire, agb, longitude,CN.top.C1 => r = 0.75
summary(sola)

solb <- bioenv(wisconsin(peaveg) ~ FIRE + AGB + PMN.top.C1 + CN.top.C1 + M3P.top.C1 + Soil.ANF.top.C1, peaenvfull)
solb#fire,CN.top.C1, M3P.top.C1, Soil.ANF.top.C1 => r = 0.7214
summary(solb)

solc <- bioenv(wisconsin(peaveg) ~ FIRE + AGB + PMN.top.C1 + CN.top.C1 + Soil.ANF.top.C1 + pH.top.C2, peaenvfull)
solc#fire,CN.top.C1, M3P.top.C1, Soil.ANF.top.C1 => r = 0.7214
summary(solc)

#THIS IS CONSTRAINED CCA => THAT'S WHAT I WANT TO USE FOR FINAL ANALYSIS
#SOIL VARIABLES ON RIGHT SIDE ARE THE CONSTRAINTS
modb <- cca(peaveg ~ FIRE + CN.top.C1 + Soil.ANF.top.C1 + pH.top.C2, data=peaenvfull)
summary(modb)
anova(modb)#p = 0.060
anova(modb, by = "term", step=200)#anova type I
anova(mod, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated
#terms will get higher (“worse”) P-values
anova(mod, by="axis", perm=1000)

library(vegan3d)
ordiplot3d(modb, type = "h")

#unconstrained
mod1 <- cca(varespec ~ ., varechem)
mod1

#using AIC to help in model selection
mod0 <- cca(varespec ~ 1, varechem)
mod <- step(mod0, scope = formula(mod1), test = "perm")
vif.cca(mod1)#variance inflation factors
#A common rule of thumb is that vif > 10 indicates that a variable is
#strongly dependent on others and does not have independent information
#On the other hand, it may not be the variable that should be removed,
#but alternatively some other variables may be removed

#Species-Environment Correlation
spenvcor(mod)
#Correlation coefficient is very sensitive to single extreme values
#eigenvalue is a more appropriate measure of similarity

#Weighted average scores are preferable
#The practical reason to prefer wa scores is that they are more robust against random error in environmental variables

lmodb <- as.mlm(modb)
## Coefficients
lmod

coef(modb)

plot(modb, type = "n",xlab="CCA 1 (44.2%)",ylab="CCA 2 (28.7%)")
points(modb, cex = 10*hatvalues(lmodb), pch=16, xpd = TRUE,col="lightblue")
text(modb, display = "bp", col = "blue",scaling=1)#to sites; scaling=2 is to species
text(modb,display="sites",col="red")

solc <- bioenv(wisconsin(peaveg) ~ FIRE + AGB + PMN.top.C1 + CN.top.C1 + AvFe.top.C1 + pH.top.C2, peaenvfull)
solc#fire,CN.top.C1, M3P.top.C1, Soil.ANF.top.C1 => r = 0.7214
summary(solc)

modc <- cca(peaveg ~ FIRE+ pH.top.C2 + CN.top.C2, data=peaenvfull)
summary(modc)
anova(modc)#p = 0.08

lmodc <- as.mlm(modc)
## Coefficients
lmodc
coef(modc)

plot(modc, type = "n",xlab="CCA 1 (44.2%)",ylab="CCA 2 (28.7%)")
points(modc, cex = 10*hatvalues(lmodc), pch=16, xpd = TRUE,col="lightblue")
text(modc, display = "bp", col = "blue")
text(modc,display="sites",col="red")

modd <- cca(peaveg ~ FIRE + Soil.ANF.top.C1, data=peaenvfull)#pretty good
summary(modd)
anova(modd)#p = 0.019

mod$terms
lmodd <- as.mlm(modd)
## Coefficients
lmodd

coef(modd)
## Influential observations
influence.measures(lmodd)#Functions refit results of constrained ordination (cca, rda, capscale) 

#as a multiple response linear model (lm)
lmod$coefficients

plot(modd, type = "n")
points(modd, cex = 10*hatvalues(lmodd), pch=16, xpd = TRUE,col="blue")
text(modd, display = "bp", col = "blue")
text(modd,display="sites",col="red")
sites=c("I1","I2", "I3", "I5", "I6", "I7")

mode <- cca(peaveg ~ FIRE+AGB+Soil.ANF.top.C1, data=peaenvfull)#pretty good
summary(mode)
anova(mode)#p = 0.026

modf <- cca(peaveg ~ FIRE+Soil.ANF.top.C1+AvFe.top.C1+CN.top.C1+M3P.top.C1, data=peaenvfull)#pretty good
summary(modf)
anova(modf)#p = 0.026

lmodf <- as.mlm(modf)
## Coefficients
lmodf
coef(lmodf)

plot(modf, type = "n")
points(modf, cex = 10*hatvalues(lmodf), pch=16, xpd = TRUE,col="blue")
text(modf, display = "bp", col = "blue")
text(modf,display="sites",col="red")

mod2 <- cca(peaveg ~ AvFe.top.C1+CN.top.C1+FIRE+AGB+pH.H2O.top.C1 , data=peaenvfull)#pretty good
summary(mod2)
anova(mod2)#p=0.48

lmod2 <- as.mlm(mod2)
## Coefficients
lmod2
coef(lmod2)

plot(mod2, type = "n",xlab="CCA 1 (54%)",ylab="CCA 2 (30.3%)")
points(mod2, cex = 10*hatvalues(lmod2), pch=16, xpd = TRUE)
text(mod2, display = "bp", col = "blue")
text(mod2,display="sites",col="red")
summary(mod2)

names(peaenvfull)
str(peaenvfull)
sol2 <- bioenv(wisconsin(peaveg) ~ AGB + CN.top.C1 + M3P.top.C1 + TN.top.C1 + PMN.top.C1 + FIRE, peaenvfull)
sol2
summary(sol2)#FISHER AND FIRE ARE THE BEST, FOLLOWED WITH AGB

names(peaenv)
peaveg
peaenv$FIRE=as.integer(peaenv$FIRE)

mod2a <- cca(peaveg~FIRE+AGB+TN.top.C1+PMN.top.C1, data=peaenv)#THE BEST SO FAR
summary(mod2a)
anova(mod2a)
anova(mod2a,mod2)#p=0.05

mod2a.1 <- cca(peaveg~FIRE+AGB+TN.top.C1+PMN.top.C1, data=peaenv)#THE BEST SO FAR
summary(mod2a)
anova(mod2a)

lmod2a <- as.mlm(mod2a)
## Coefficients
lmod2a

coef(mod2a)
## Influential observations
influence.measures(lmod2a)

plot(mod2a, type = "n",xlab="CCA 1 (42.4%)",ylab="CCA 2 (30.7%)")
points(mod2a, cex = 10*hatvalues(lmod), pch=16, xpd = TRUE,col="lightblue")
text(mod2a, display = "bp", col = "black",cex=1.4)
text(mod2a,display="sites",col="red")
mtext("p=0.05", side=3,cex=1.0,adj=0, line=0.5)
mtext("I7", side=3,cex=1.4,adj=0.25, line=-1.5,col="black")
mtext("I1", side=3,cex=1.4,adj=0.32, line=-27,col="black")
mtext("I6", side=3,cex=1.6,adj=0.68, line=-16.8,col="black")
mtext("I5", side=3,cex=1.6,adj=0.7, line=-15,col="black")
mtext("I3", side=3,cex=1.6,adj=0.62, line=-17.5,col="black")
mtext("I2", side=3,cex=1.6,adj=0.66, line=-20,col="black")

scl <- 3 ## scaling = 5
colvec <- c("red2", "green4", "mediumblue")
str(scrs, max = 1)
scrs <- scores(mod, display = c("sites", "species"), scaling = scl)
xlim <- with(scrs, range(species[,1], sites[,1]))
ylim <- with(scrs, range(species[,2], sites[,2]))


plot(mod2a, type = "n",xlab="CCA 1 (45%)",ylab="CCA 2 (33%)",scaling=scl)
points(mod2a, cex = 10*hatvalues(lmod), pch=16, xpd = TRUE,col="lightblue")
text(mod2a, display = "bp", col = "black",cex=1.4)
text(mod2a,display="species",col="red")
mtext("p=0.028", side=3,cex=1.0,adj=0, line=0.5)
mtext("I7", side=3,cex=1.4,adj=0.25, line=-1.5,col="black")
mtext("I1", side=3,cex=1.4,adj=0.32, line=-27,col="black")
mtext("I6", side=3,cex=1.6,adj=0.68, line=-16.8,col="black")
mtext("I5", side=3,cex=1.6,adj=0.7, line=-15,col="black")
mtext("I3", side=3,cex=1.6,adj=0.62, line=-17.5,col="black")
mtext("I2", side=3,cex=1.6,adj=0.66, line=-20,col="black")

summary(mod2a)

mod2b <- cca(peaveg~FIRE+AGB+TN.top.C1+CN.top.C1, data=peaenv)
summary(mod2b)
anova(mod2b)#p=0.06

lmod2b <- as.mlm(mod2b)
## Coefficients
lmod2b

coef(mod2b)
## Influential observations
influence.measures(lmod2b)#Functions refit results of constrained ordination (cca, rda, capscale) 
#as a multiple response linear model (lm)
lmod2b$coefficients

plot(mod2b, type = "n",xlab="CCA 1 (65.6%)",ylab="CCA 2 (34.4%)")
points(mod2b, cex = 10*hatvalues(lmod2b), pch=16, xpd = TRUE,col="lightblue")
text(mod2b, display = "bp", col = "black",cex=1.4)
text(mod2b,display="sites",col="red")
mtext("p=0.061", side=3,cex=1.0,adj=0, line=0.5)
mtext("I7", side=3,cex=1.4,adj=0.27, line=-11,col="black")
mtext("I1", side=3,cex=1.4,adj=0.23, line=-20.9,col="black")
mtext("I6", side=3,cex=1.6,adj=0.68, line=-4.2,col="black")
mtext("I5", side=3,cex=1.6,adj=0.73, line=-13,col="black")
mtext("I2", side=3,cex=1.6,adj=0.65, line=-21,col="black")
mtext("I3", side=3,cex=1.6,adj=0.62, line=-26.8,col="black")
summary(mod2b)

names(peaenvfull)
sol3 <- bioenv(wisconsin(peaveg) ~ AGB + Soil.ANF.top.C1 + PMN.top.C1 + M3P.top.C1+ CN.top.C1 + Fire.Legacy, peaenvfull)
sol3
summary(sol3)#CN top C1, AGB and Fire legacy

sol4 <- bioenv(wisconsin(peaveg) ~ AGB + Soil.ANF.top.C1+ PMN.top.C1 + Fisher.alpha.diversity + CN.top.C1 + FIRE, peaenvfull)
sol4
summary(sol4)#CN top C1, AGB and Fire legacy

names(peaenvfull)
mod3 <- cca(peaveg~AGB+ANF.0.10.C1+CN.0.10.C1+Fire Legacy, data=peaenvfull)
summary(mod3)
anova(mod3)#p=0.044

lmod3 <- as.mlm(mod3)
## Coefficients
summary(lmod3)

coef(mod3)
hatvalues(lmod3)
## Influential observations
influence.measures(lmod3)#Functions refit results of constrained ordination (cca, rda, capscale) 

#as a multiple response linear model (lm)
lmod$coefficients

plot(mod3, type = "n",xlab="CCA 1 (37.7%)",ylab="CCA 2 (27.5%)")
points(mod3, cex = 10*hatvalues(lmod3), pch=16, xpd = TRUE,col="lightblue")
text(mod3, display = "bp", col = "black",cex=1.4)
text(mod3,display="sites",col="red")
mtext("p=0.044", side=3,cex=1.0,adj=0, line=0.5)
mtext("I1", side=3,cex=1.4,adj=0.5, line=-27,col="black")
mtext("I7", side=3,cex=1.4,adj=0.12, line=-1.6,col="darkred")
mtext("I6", side=3,cex=1.6,adj=0.82, line=-10,col="red")
mtext("I5", side=3,cex=1.6,adj=0.81, line=-6.2,col="purple")
mtext("I3", side=3,cex=1.6,adj=0.72, line=-10.7,col="red")
mtext("I2", side=3,cex=1.6,adj=0.77, line=-10.5,col="red")

mod4 <- cca(peaveg~AGB+PMN.top.C1+CN.top.C1+Soil.ANF.top.C1, data=peaenvfull)
summary(mod4)
anova(mod4)#p=0.058

mod4a <- cca(peaveg~AGB+NP.top.C1+Soil.ANF.top.C1+Fire.Legacy, data=peaenvfull)
summary(mod4a)
anova(mod4a)#p=0.031

lmod4a <- as.mlm(mod4a)
## Coefficients
lmod4a

plot(mod4a, type = "n",xlab="CCA 1 (46%)",ylab="CCA 2 (28.8%)")
points(mod4a, cex = 10*hatvalues(lmod4a), pch=16, xpd = TRUE,col="lightblue")
text(mod4a, display = "bp", col = "black",cex=1.4)
text(mod4a,display="sites",col="red")
mtext("p=0.031", side=3,cex=1.0,adj=0, line=0.5)
mtext("I1", side=3,cex=1.4,adj=0.52, line=-27,col="black")
mtext("I7", side=3,cex=1.4,adj=0.17, line=-1.6,col="darkred")
mtext("I6", side=3,cex=1.6,adj=0.84, line=-10,col="red")
mtext("I5", side=3,cex=1.6,adj=0.81, line=-5,col="purple")
mtext("I3", side=3,cex=1.6,adj=0.74, line=-11,col="red")
mtext("I2", side=3,cex=1.6,adj=0.79, line=-11,col="red")


#PCA ENV VARIABLES

## No constraints: PCA
mod0 <- rda(peaveg ~ 1, peaenvfull)
mod0
plot(mod0)

mod <- ordistep(mod0, scope=formula(mod1))
mod
plot(mod)
points(mod, pch=16, xpd = TRUE,col="lightblue")

## Permutation test for all variables
anova(mod)

## Permutation test of "type III" effects, or significance when a term
## is added to the model after all other terms
anova(mod, by = "margin")

data(dune.env)
head(dune.env)

## All environmental variables: Full model
mod1 <- rda(peaveg ~ ., peaenvfull)
mod1
plot(mod1)

#TRY THIS ALSO
# The method is very slow for large number of possible subsets.
# Therefore only 6 variables in this example.
data(varespec)
str(varespec)
head(varespec)
data(varechem)
names(varechem)
head(varechem)
sol <- bioenv(wisconsin(varespec) ~ log(N) + P + K + Ca + pH + Al, varechem)
sol
summary(sol)

#Trying Mantel
data(varespec)
data(varechem)
veg.dist <- vegdist(varespec) # Bray-Curtis
env.dist <- vegdist(scale(varechem), "euclid")
mantel(veg.dist, env.dist)
mantel(veg.dist, env.dist, method="spear")