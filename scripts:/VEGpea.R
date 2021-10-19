#Vegetation Data

peaveg<-read.csv(file.choose())#PEA VEG MATRIX
attach(peaveg)
dim(peaveg)

peaveg2<-read.csv(file.choose())#PEA sp matrix dbh 10 updated
attach(peaveg2)
dim(peaveg2)
str(peaveg2)
peaveg2[2:33]=as.numeric(peaveg2[2:33])
str(peaveg2)
peaveg2

peasp2<-data.matrix(peaveg2)#use this one for updated dbh10
str(peasp2)
peasp2[2:33]=as.numeric(peasp2[2:33])
str(peasp2)
peasp2

peasp3<-data.matrix(peaveg2[2:33])#use this one for updated dbh10
str(peasp3)
peasp3[1:32]=as.numeric(peasp3[1:32])
str(peasp3)
peasp3

peaveg3<-read.csv(file.choose())#PEA_family_matrix
attach(peaveg3)
dim(peaveg3)
str(peaveg3)
peafamily<-data.matrix(peaveg3)
str(peafamily)
peafamily[1:5, 1:32]=as.numeric(peafamily[1:5, 1:32])
str(peafamily)

pea_ba<-read.csv(file.choose())#BA_sp_matrix
attach(pea_ba)
dim(pea_ba)
str(pea_ba)
basal_area<-pea_ba[,2:52]
names(basal_area)
hist(basal_area$Tachigali.bracteosa)
hist(rec$Empnig)

pea_all<-read.csv(file.choose())#PEA_ALL
attach(pea_all)
dim(pea_all)
str(pea_all)
names(pea_all)

pea_env<-read.csv(file.choose())#Environmental Data
attach(pea_env)
dim(pea_env)
names(pea_env)

pea_soil<-read.csv(file.choose())#pea_env_varpart
attach(pea_soil)
dim(pea_soil)
names(pea_soil)

peaenvfull2<-read.csv(file.choose())#PEA ENV DATA FULL_updated
attach(peaenvfull2)
dim(peaenvfull2)
names(peaenvfull2)

peaenvfull3<-read.csv(file.choose())#PEA ENV DATA FULL_updated_noI7
attach(peaenvfull3)
dim(peaenvfull3)
names(peaenvfull3)

peaenvfull3a<-read.csv(file.choose())#PEA_env_data_means_noI7
attach(peaenvfull3a)
dim(peaenvfull3a)
names(peaenvfull3a)

peaenvfire<-read.csv(file.choose())#PEA ENV DATA FULL_fire_updated_means
attach(peaenvfire)
str(peaenvfire)
names(peaenvfire)
levels(peaenvfire$Site)



cor(peaenvfire[,4:31], use="complete.obs", method="spearman")
library(Hmisc)
rcorr(as.matrix(peaenvfire[,5:18]), type="spearman")

peaenvfire2<-read.csv(file.choose())#PEA ENV DATA FULL_fire_updated_means
attach(peaenvfire2)
dim(peaenvfire2)
names(peaenvfire2)

cor(peaenvfire2[,4:31], use="complete.obs", method="spearman")
library(Hmisc)
rcorr(as.matrix(peaenvfire2[,5:31]), type="spearman")

cor(peaenvfull3a, use="complete.obs", method="spearman")
library(Hmisc)
rcorr(as.matrix(peaenvfull3a), type="spearman")

#Ordination tutorial, to use with my data later

library(vegan)
library(lattice)
library(permute)
library(MASS)
data(varespec)
names(varespec)

#Non-metric multidimensional scaling (NMDS) can be performed using isoMDS function
#in the MASS package => IT IS A NONLINEAR METHOD 
#This function needs dissimilarities as input.
#Function vegdist in vegan contains dissimilarities which are found good
#in community ecology. The default is Bray-Curtis dissimilarity, nowadays
#often known as Steinhaus dissimilarity, or in Finland as Sørensen index.

str(peaveg2)
vare.dis <- vegdist(peasp2)#dissimilarities
vare.dis
vare.dis_updated <- vegdist(peasp3, method="bray")#dissimilarities
vare.dis_updated
pea.mds0 <- isoMDS(peaveg2)
pea.mds0
plot(pea.mds0)

vare.dis <- vegdist(peaveg2[c(1:53)],method="bray")#dissimilarities
vare.dis
pea.mds03 <- metaMDS(vare.dis_updated)#species level
pea.mds03
plot(pea.mds03)#species level
pea.mds_updated <- metaMDS(vare.dis_updated,trymax=900)#species level based on dissimilarities
pea.mds_updated
plot(pea.mds_updated)#species level

dis_mds_updated <- vegdist(decostand(peasp3,"norm"), "bray")
dis_mds_updated
pea.mds_up2 <- metaMDS(dis_mds_updated, trymax=900)#species level based on dissimilarities
pea.mds_up2 #try using this one today
plot(pea.mds_up2)

vare.disfam <- vegdist(peaveg3,method="bray")#dissimilarities
vare.disfam
pea.mdsfam <- metaMDS(vare.disfam)#species level
pea.mdsfam
plot(pea.mdsfam)#species level

plot(pea.mdsfam,type = "p",cex=1.2,display=c("sites","species"),choices=c(1,2),scaling = "sites")
text(pea.mdsfam, display = c("sites", "species"),forests,cex=1.8, color="blue",
     choices = c(1,2))
plot(ef_finalfam,p.max = 0.15, col = "blue")

ef_finalfam <- envfit(vare.disfam, peaenvfull3a, permu = 999)#using decorana
ef_finalfam#moisture

ef_finalfam2 <- envfit(dis_mdsBA, peasoil3b, permu = 999)#using decorana
ef_finalfam2#total C


vare.disBA <- vegdist(basal_area,method="bray")#dissimilarities
vare.disBA
env.disBA <- vegdist(basal_area,method="bray",k=2,trymax=500)#dissimilarity index
env.disBA
pea.mdsBA <- metaMDS(vare.disBA,trymax=900)#species level
pea.mdsBA
plot(pea.mdsBA)#species level based on BA

dis_mdsBA <- vegdist(decostand(basal_area,"norm"), "bray")
dis_mdsBA
pea.mdsBA <- metaMDS(dis_mdsBA,trymax=900)#species level based on dissimilarities
pea.mdsBA
plot(pea.mdsBA)
plot(ef_finalBA2,pmax=0.15)
plot(pea.mdsBA,type = "p",cex=1.2,display=c("sites","species"),choices=c(1,2),scaling = "sites")
text(pea.mdsBA, display = c("sites", "species"),forests,cex=1.8, color="blue",
     choices = c(1,2))
plot(ef_finalBA2,p.max = 0.15, col = "blue")

ef_finalBA <- envfit(dis_mdsBA, peasoil3b, permu = 999)#using decorana
ef_finalBA#total C

ef_finalBA2 <- envfit(dis_mdsBA, peaenvfull3a, permu = 999)#using decorana
ef_finalBA2#total C

dis_mds03 <- vegdist(decostand(peaveg2[c(1:53)],"norm"), "bray")
dis_mds03
pea.mds03 <- metaMDS(dis_mds03)#species level based on dissimilarities
pea.mds03


plot(pea.mds03)
MDS1 = pea.mds03$points[,1]
MDS2 = pea.mds03$points[,2]
NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, Site = forests)
NMDS

ggplot(NMDS, aes(x=MDS1, y=MDS2, col=Site, shape=Site,cex=4,cex.lab=2)) +
  geom_point() +
  stat_ellipse() +
  theme_bw() +
  labs(title = "NMDS Plot Forests Species Level")

names(peasoil3b)
#today
ef_final03 <- envfit(pea.mds_up2, peaenvfull3, permu = 999)#using decorana
ef_final03

ef_final03a <- envfit(pea.mds_up2,peasoil3b, permu = 999)#using decorana
ef_final03a

fit_mds03 <- envfit(pea.mds03~.,data=peasoil3b, perm = 0, display = "lc", scaling = "sites")
fit_mds03
plot(pea.mds03)

#today final plot
plot(ef_final03,  p.max = 0.1,col = "red")#based on veg distance
plot(pea.mds_up2,type = "p",cex=1.2,display=c("sites","species"),choices=c(1,2),scaling = "sites")
text(pea.mds_up2, display = c("sites", "species"),forests,cex=1.8, color="blue",
     choices = c(1,2))
plot(ef,p.max = 0.1, col = "blue")


data(varechem)
names(peasoil3b)
rankindex(scale(peasoil3b), basal_area, c("euc","man","bray","jac","kul"))

#Stress S is a statistic of goodness of fit, and
#it is a function of and non-linear monotone transformation of observed
#dissimilarities θ(d) and ordination distances ˜d.

#Nmds maps observed community dissimilarities nonlinearly onto ordination
#space and it can handle nonlinear species responses of any shape.
#We can inspect the mapping using function Shepard in MASS package, or
#a simple wrapper stressplot in vegan:

stressplot(vare.mds0, vare.dis)

#trying with peaveg

pea.dis <- vegdist(peaveg2)
pea.dis
pea.mds0 <- isoMDS(pea.dis)
pea.mds0

peaenvfull3$LongitudeDecimal

names(peaenvfull3)
peaenvfull3$LongitudeDecimal
env.dis <- vegdist(peaveg3,method="bray",k=2,trymax=500)#dissimilarity index
env.dis

NMDS<-metaMDS(env.dis)
MDS1 = NMDS$points[,1]
MDS2 = NMDS$points[,2]
NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, Site = forests)
NMDS

ggplot(NMDS, aes(x=MDS1, y=MDS2, col=Site, cex=1.6)) +
  geom_point() +
  stat_ellipse() +
  theme_bw() +
  labs(title = "NMDS Plot Soil")

data(mite)
data("mite.pcnm")
data(mite.env)
str(mite.env)
str(mite.pcnm)

str(peaveg3)
str(pea_env)
str(pea_soil)
pea_env$Stem.Density=as.numeric(pea_env$Stem.Density)
str(pea_env)
pea_env$Species.richness=as.numeric(pea_env$Species.richness)
str(pea_soil)

mod_pea <- varpart(peaveg3, ~ ., pea_soil, data=pea_env, transfo="hel")
mod_pea
showvarparts(2, bg = c("hotpink","skyblue"))
plot(mod_pea, bg = c("hotpink","skyblue"))
mod2 <- varpart(decostand(peaveg3, "hel"),pea_soil, pea_env)
showvarparts(2, bg = c("hotpink","skyblue"))
plot(mod2, bg = c("hotpink","skyblue"))

aFrac <- rda(decostand(peaveg3, "hel"), pea_env, pea_soil)
anova(aFrac, step=200, perm.max=200)
RsquareAdj(aFrac)

mod_b <- varpart(peaveg3, ~ TP.B+TC.B+AGB,
               pea_env, data=pea_soil, transfo="hel")
mod_b
showvarparts(2, bg = c("hotpink","skyblue"))
plot(mod_b, bg = c("hotpink","skyblue"))

varpea <- varpart(vegdist(peaveg3), ~ .,peaenvfull3, transfo="hel")
summary(varpea)


stressplot(pea.mds0, pea.dis)

par(mfrow = c(1,1))
plot(pea.mds03, type = "t")
plot(scores(pea.mds03))
points(pea.mds03,display="sites")

#THAT'S THE ONE
data(dune)
names(dune.env)
str(dune.env)
str(peaenvfire)
peaenvfull2$Fire.Legacy=as.integer(peaenvfull2$Fire.Legacy)
peaenvfire$Fire.Scar=as.factor(peaenvfire$Fire.Scar)

pea.mds <- metaMDS(peaveg2,k=2,trymax=500)#k is number of reduced dimensions
summary(pea.mds)
pea.mds
pea.mds03#this one is the metaMDS of the dissimilarity between forests at species level

plot(pea.mds03,type = "p",cex=1.2,display=c("sites","species"),choices=c(1,2),scaling = "sites")
text(pea.mds03, display = c("sites", "species"),forests,cex=1.8, color="blue",
     choices = c(1,2))
plot(ef,p.max = 0.1, col = "blue")

fit_mds03 <- envfit(pea.mds03 ~ .,data=peaenvfull3, perm = 0, display = "lc", scaling = "sites")
fit_mds03
plot(fit,  p.max = 0.05,col = "red")

ord.fit2$vectors
bc<-vegdist(peaveg2, method="bray", binary=FALSE) 
bc

plot(pea.mds,type = "p",cex=1.2,display=c("sites","species"),choices=c(1,2),scaling = "sites")
text(pea.mds, display = c("sites", "species"),forests,cex=1.8, color="blue",
          choices = c(1,2))
plot(ef,p.max = 0.1, col = "blue")
ord.fit2$vectors
bc<-vegdist(peaveg2, method="bray", binary=FALSE) 
bc

#TRYING THIS NOW - BACK TO THIS SOON
bc.mds<-metaMDS(bc,k=2,try=500)
bc.mds
plot(bc.mds, choices = c(1, 2), type="n") #plots the ordination axes
points(bc.mds, display = c("sites", "species"))#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
text(bc.mds, display = c("sites", "species"),cex=1.6)

#shorter dataset
names(peaenvfull3)
peafinal<-peaenvfull3[c(1,4:5,9,12,15,18:19,22,31:32)]

ord.fit2 <- envfit(bc.mds ~ ., data=peafinal, perm=999)
ord.fit2
plot(ord.fit2,p.max = 0.1)

anosim_location = anosim(bc, forests)
anosim_location # take a look at results
summary(anosim_location)
plot(anosim_location)

str(peaenvfull3)
ordisurf(bc.mds ~ TC, data=peaenvfull3, method = "REML", select = TRUE)
summary(ordisurf)

plot(pea.mds$points)
MDS_xy <- data.frame(pea.mds$species)
MDS_xy

bci.mds<-metaMDS(peaveg2, distance = "bray", k = 2, trymax = 20, autotransform =TRUE, noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE) #makes the object bci.mds using Bray-Curtis ordination
bci.mds
plot(bci.mds, choices = c(1, 2), type="n") #plots the ordination axes
points(bci.mds, display = c("sites", "species"))#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
text(bci.mds, display = c("sites", "species"),cex=1.6)

plot(ord.fit)
ordispider(pea.mds,Fire.Legacy, col="skyblue")
ordisurf(pea.mds, NP.10.30.C2, add=TRUE)
ordihull(pea.mds, Fire.Legacy, col=1:4, lwd=3)
ordiellipse(pea.mds, Fire.Legacy, col=1:4, draw="polygon")
ordispider(pea.mds, Fire.Legacy, col=1:4, label = TRUE)

names(peaenvfull3)
ord.fit <- envfit(pea.mds ~ pH.H2O.NB+TC.B+PMN.B+AvFe.B, data=peaenvfull3, perm=999)
ord.fit

ord.fit2 <- envfit(pea.mds ~ peaenvfull3, data=peaenvfull3, perm=999)
ord.fit2
plot(ord.fit2)

pea.mds3 <- metaMDS(peaveg3,k=2,trymax=500)#k is number of reduced dimensions
summary(pea.mds3)
pea.mds3
plot(pea.mds3,type = "p",cex=1.2,display=c("sites","species"),choices=c(1,2),scaling = "sites")
text(pea.mds3, display = c("sites", "species"),forests,cex=1.8, color="blue",
     choices = c(1,2))
plot(ef,p.max = 0.1, col = "blue")
ord.fit2$vectors

ord.fit3 <- envfit(pea.mds3 ~ peaenvfull3, data=peaenvfull3, perm=999)
ord.fit3
plot(ord.fit3)

bc3<-vegdist(peaveg3, method="bray", binary=FALSE) 
bc3

#REPEAT ef with less variables, only the most important ones (higher R2)
names(peaenvfire)
str(peaenvfire)
str(peaenvfull3)
names(peaenvfull3)
peaenvfull3$codes=c("I1","I2","I3","I5","I6")
peasoil <- peaenvfull2[ , 2:57]
peasoil2 <- peaenvfire[c(3,5:18)]
names(peasoil2)
peasoil3 <- peaenvfull3[c(1:22,31:32,37)]#based on findings of geochemMDS2
names(peasoil3)
peasoil3a <- peaenvfull3[c(1:22,25:26,31:32,37)]#based on findings of geochemMDS2
names(peasoil3a)

library(vegan)
ord <- metaMDS(iris[, 1:4])
plot(ord)
plot(envfit_final,pmax=0.15)
ggord(ord, iris$Species)
ord <- metaMDS(dis,trymax=500)
ggord(ord, iris$Species)

#here is euclidean, but maybe try with bray too
str(peaenvfull3)
peafinal$Fire.Legacy=as.numeric(peafinal$Fire.Legacy)
str(peafinal)

geochemMDS <- metaMDS(peafinal, distance="euclidean",trymax=500, autotransform=TRUE)
geochemMDS
summary(geochemMDS)

names(peasoil3)
soil.dist <- vegdist(decostand(peaenvfull3[, 1:24], "norm"), "euclidean")
soil.dist
names(peaenvfull3)
peaenvfull3$sites=c("I1","I2","I3","I5","I6")

ord_soil <- metaMDS(soil.dist, distance='euclidean', k=3, trymax=50, autotransform=TRUE, wasscores=TRUE, noshare=FALSE)
geochemMDS3
ord_soil <- metaMDS(soil.dist)
ord_soil$species
ord_soil$points
plot(ord_soil,display="sites")
NMDS.1 = ord_soil$points[,1]
NMDS.2 =ord_soil$points[,2]
NMDSab = data.frame(NMDS.1 = NMDS.1, NMDS.2 = NMDS.2, Site = forests)
NMDSab
ggplot(NMDSab, aes(x=MDSa, y=MDSb, col=Site, shape=Site,cex=4,cex.lab=2)) +
  geom_point() +
  stat_ellipse() +
  theme_bw() +
  labs(title = "NMDS Plot Soils")
library(vegan)
ord <- metaMDS(iris[, 1:4])


variableScores3 <- ord_soil$species
variableScores3
sampleScores3 <- ord_soil$points
plot(sampleScores3[,1], sampleScores3[,2], xlab='NMDS 1', ylab='NMDS 2', type='n', asp=1, las=1)
points(sampleScores3, pch=16, cex=0.8, col=c(rep("green",2),rep("blue",2),rep("black",2),rep("red",2),rep("purple",2),rep("darkgray",2)))
text(ord_soil, display = c("sites"),peaenvfull3[,37],cex=1.6, col=c(rep("green",2),rep("blue",2),rep("black",2),rep("red",2),rep("purple",2),rep("darkgray",2)))
arrows(0, 0, variableScores3[,1], variableScores3[,2], length=0.1, angle=20)
textNudge <- 1.2
text(variableScores3[,1]*textNudge, variableScores3[,2]*textNudge, rownames(variableScores3), cex=1.0)
variableScores3
ordiplot(geochemMDS3,type="n")
orditorp(geochemMDS3,display="species",col="red",air=0.01)
orditorp(geochemMDS3,display="sites",col=c(rep("green",2),rep("blue",2),rep("black",2),rep("red",2),rep("purple",2),rep("yellow",2)),
         air=0.01,cex=1.25)


anosim_locationSOIL = anosim(soil.dist, forests)
anosim_locationSOIL # take a look at results
summary(anosim_locationSOIL)
plot(anosim_locationSOIL)

summary(geochemMDS)
anova(geochemMDS)
names(geochemMDS)
variableScores <- geochemMDS$species
variableScores
sampleScores <- geochemMDS$points
plot(sampleScores[,1], sampleScores[,2], xlab='NMDS 1', ylab='NMDS 2', type='n', asp=1, las=1)
points(sampleScores, pch=16, cex=1.4, col='red')
text(geochemMDS, display = c("sites"),peasoil3$sites,cex=1.8, col="blue")
arrows(0, 0, variableScores[,1], variableScores[,2], length=0.05, angle=20)
textNudge <- 1.2
text(variableScores[,1]*textNudge, variableScores[,2]*textNudge, rownames(variableScores), cex=0.7)
variableScores
ordihull(geochemMDS, group=peasoil2$C.top.C1, show="0")

geochemMDS2 <- metaMDS(peasoil2, distance='euclidean', k=3, trymax=50, autotransform=TRUE, wasscores=TRUE, noshare=FALSE)
geochemMDS2
names(geochemMDS2)
variableScores2 <- geochemMDS2$species
sampleScores2 <- geochemMDS2$points
plot(sampleScores2[,1], sampleScores2[,2], xlab='NMDS 1', ylab='NMDS 2', main="Ordination sites and clusters based on soil variables",type='n', asp=1, las=1)
points(sampleScores2, pch=16, cex=1.4, col='red')
text(geochemMDS2, display = c("sites"),peaenvfire[,1],cex=1.6, col="blue")
arrows(0, 0, variableScores2[,1], variableScores2[,2], length=0.1, angle=20)
textNudge <- 1.2
text(variableScores2[,1]*textNudge, variableScores2[,2]*textNudge, rownames(variableScores2), cex=1.2)
variableScores2

names(peasoil2)
peasoil2_red<-peasoil2[c(1:3,5:8,10:12,14)]
names(peasoil2_red)
str(peasoil3)
geochemMDS3 <- metaMDS(peasoil2_red, distance='euclidean', k=3, trymax=50, autotransform=TRUE, wasscores=TRUE, noshare=FALSE)
geochemMDS3
names(geochemMDS3)
variableScores3 <- geochemMDS3$species
variableScores3
sampleScores3 <- geochemMDS3$points
plot(sampleScores3[,1], sampleScores3[,2], xlab='NMS 1', ylab='NMS 2', type='n', asp=1, las=1)
points(sampleScores3, pch=16, cex=0.2, col=c(rep("green",2),rep("blue",2),rep("black",2),rep("red",2),rep("purple",2),rep("darkgray",2)))
text(geochemMDS3, display = c("sites"),peaenvfire[,1],cex=1.6, col=c(rep("green",2),rep("blue",2),rep("black",2),rep("red",2),rep("purple",2),rep("darkgray",2)))
arrows(0, 0, variableScores3[,1], variableScores3[,2], length=0.1, angle=20)
textNudge <- 1.2
text(variableScores3[,1]*textNudge, variableScores3[,2]*textNudge, rownames(variableScores3), cex=1.0)
variableScores3
ordiplot(geochemMDS3,type="n")
orditorp(geochemMDS3,display="species",col="red",air=0.01)
orditorp(geochemMDS3,display="sites",col=c(rep("green",2),rep("blue",2),rep("black",2),rep("red",2),rep("purple",2),rep("yellow",2)),
         air=0.01,cex=1.25)

perm <- how(nperm = 199)
names(peasoil3)

#VOLTAR AQUI
dim(peasp2)
names(peaenvfull3)
names(peaveg3)

dis_mdsBA

names(peasoil3)
str(peasoil3)
fire=as.factor(peasoil3[,1])
fire
fertility=peasoil3[,c(4,15,20,22)]
peasoil3$Fire.Legacy=as.numeric(peasoil3$Fire.Legacy)
adonis (dis_mdsBA ~ TC+Fire.Legacy+NP.B, data = peasoil3, permutations = 999)

adonis (dis_mdsBA ~ CN, data = peasoil3, permutations = 999)

adonis (dis_mdsBA ~ TC*PMN.B, data = peasoil3, permutations = 999)
solBA <- bioenv(wisconsin(basal_area) ~ TC + CN + PMN + M3P.NB + TP.NB, peasoil3)
solBA
summary(solBA)

peafogo<-peasoil3[,c(1,4,9,14)]
veg.distBA <- vegdist(basal_area) # Bray-Curtis
env.distBA <- vegdist(scale(peafogo), "euclid")
names(peasoil3)
mantel(veg.distBA ~ env.distBA,mrank=TRUE,nperm = 1000,nboot = 500, pboot = 0.9, cboot = 0.90)
plot(mgram(veg.distBA, env.distBA, nclass=8))
mantel(veg.distBA, env.distBA, method="spear")

adonis (peasp2 ~ TC+Fire.Legacy+TP.B, data = peasoil3, permutations = 999)
adonis (dis_mdsBA ~ TC+Fire.Legacy+PMN.B, data = peasoil3, permutations = 999, by="margin")
#TC significant and explains 65.5% variation
adonis (dis_mdsBA ~ TC+Fire.Legacy+PMN.B, data = peasoil3b, permutations = 999, by=NULL)

adonis (dis_mdsBA ~ TC+PMN.B, data = peasoil3, permutations = 999,by="margin")
adonis (dis_mdsBA ~ TC+PMN.B+TP.NB, data = peasoil3b, permutations = 999,by="margin")
#The best so far

names(peasoil3b)
adonis (dis_mdsBA ~ TC+Fire.Legacy+TP.NB, data = peasoil3b, permutations = 999,by="margin")
#The best so far


cadonis (peasp2 ~ TC+NP.B+PMN.B, method="bray",data = peasoil3, permutations = 999)
adonis (peaveg3 ~ CN.B+ANF.NB+AvFe.B, method="bray",data = peaenvfull3, permutations = 999)
adonis (dis, ~ Fire.Legacy+TC+AvFe.B, data = peaenvfull3, permutations = 999)

adonis (peaveg2[1:53] ~ Fire.Legacy+TC, data = peasoil3b, permutations = 999)



peasoil.nmds <- metaMDS(peaenvfull3,trymax=20)
peasoil.nmds#GOOD FIT
summary(peasoil.nmds)
scores(peasoil.nmds,"sites")
plot(peasoil.nmds, display = "sites",cex=1.4)
plot(peasoil.nmds, type = "p",cex=1.4,display=c("sites"),choices=c(1,2))
text(peasoil.nmds, display = c("sites"),forests,cex=1.6, col="blue")
ordisurf(peasoil.nmds, NP.10.30.C2, add=TRUE)

summary(peaveg.dca)
identify(peaveg.dca, "sites", labels=forests,col="blue",cex=1.7)
plot(ord.fit, p.max = 0.05,col="blue")


?metaMDS

#Function rankindex in vegan can be used to study which of the indices
#best separates communities along known gradients using rank correlation
#as default.
data(peaenvfull3)
rankindex(scale(peaenvfull3), peaveg2, c("euc","man","bray","jac","kul"))
#Bray-Curtis or Jaccard are the best

dis <- vegdist(decostand(peasp2, "norm"), "bray")
dis
plot(dis)
meta.dis<-metaMDS(dis)

dis3 <- vegdist(decostand(peaveg3, "norm"), "bray")
dis3
plot(dis3)

ef_final <- envfit(dis3, pea_env, permu = 999)#using decorana
scores(ef_final,"vectors")
ef_final$vectors
plot(dis3)
plot(ef_final,p.max = 0.1, col = "blue")
labels(ef)

names(peasoil3a)
peasoil3b<-peasoil3a[c(1,4,7:17,20:26)]
plot(meta.dis,type = "p",cex=1.2,display=c("sites","species"),choices=c(1,2),scaling = "sites")
points(meta.dis, display = "species", col = "blue")
text(meta.dis, display = c("sites", "species"),forests,cex=1.8, color="blue",
     choices = c(1,2))
plot(ef_final2,p.max = 0.15, col = "blue")

plot(meta.dis,display=c("sites","species"),cex=1.2)
plot(meta.dis, type = "p",cex=1.4,display=c("sites"),choices=c(1,2))
identify(pea.mds3, "species")
text(meta.dis, display = c("sites","species"),forests,cex=1.6, col="blue")
plot(ef_final2,p.max = 0.12, col = "blue")

str(peaveg2)
stems2 <- colSums(peaveg2[c(1:53)])
stems2

dim(stems2)
sel2 <- orditorp(meta.dis, dis="species", priority=stems2, pcol = "red", pch="+",cex=1.3)
ordilabel(meta.dis, dis="species",priority = stems2)
plot(ef3,p.max = 0.14, col = "blue",cex=1.6)
labels(ef3)

ef_final2 <- envfit(meta.dis, peasoil3b, permu = 999)#using decorana
scores(ef_final2,"vectors")
ef_final2$vectors#Total C and NP were significant
plot(ord,display = c("sites","species"))
plot(ef_final2,p.max = 0.15, col = "blue")
labels(ef)
#It's good to standardize the data
#Wisconsin double standardization often
#improves the gradient detection ability of dissimilarity indices; this can
#be performed using command wisconsin in vegan

#eigenvector methods.

#Principal components analysis (pca) and correspondence analysis (ca)
#are the most important eigenvector methods in community ordination.

#Pca is based on Euclidean distances, ca is based on Chi-square distances

#functions for PCA : prcomp or princomp, or rda in vegan package

#Running PCA for peaveg

peaveg.pca <- rda(peaveg)#with covariances => “explain” only the
#abundant species with high variances
peaveg.pca
x=(102433/135191)
x
#PCA 1 explains 75.8% of variance
y=(22638/135191)
y
#PCA 2 explains 16.7%

#in the output, Inertia = Variance

plot(peaveg.pca)
sum(apply(peaveg, 2, var))
biplot(peaveg.pca, scaling = -1)
#abundant and scarce species will be approximately as far away from the origin.

peaveg.pca2 <- rda(peaveg, scale = TRUE)#now inertia is correlation
#explain all species
peaveg.pca2#total inertia is equal to the number of variables (species)
biplot(peaveg.pca2,scaling=-1)

#CCA : Correspondence analysis is based on Chi-squared distance, and the inertia is
#the Chi-squared statistic of a data matrix standardized to unit total
#Correspondence analysis is a weighted averaging method

pea.ca <- cca(peaveg)
pea.ca
plot(pea.ca)
plot(pea.ca, scaling = 1,display="sites",xlab="CCA 1 (0.46)",ylab="CCA 2 (0.30)")#display the site scores as weighted averages of species scores
#When we takeweighted averages, the range of averages shrinks from the original values.

plot(pea.ca, scaling = 3)#symmetric scaling

chisq.test(peaveg3/sum(peaveg3))#here p-values are misleading

#detrended correspondence analysis (dca)
#better than pca and cca

peaveg.dca <- decorana(peaveg,iweigh=0,ira=0)
summary(peaveg.dca)#axis lenghts is gradient lenght

b<-plot(peaveg.dca,display="sites",pch = 21, col = "red",cex=1.6,xlab="DCA 1(45%)",ylab="DCA 2 (10%)")
text(peaveg.dca,display="sites",forests,col="black",cex=1.8)
points(peaveg.dca, display = "sp", col = "blue")
identify(b, "sp")
plot(ef, p.max = 0.05,col="red")
ordilabel(peaveg.dca, dis="sp", priority = stems)
sel <- orditorp(peaveg.dca, dis="sp", lab="species", priority=stems, pcol = "red", pch="+",cex=1.1)
ordilabel(peaveg.dca, dis="sp", lab=shnam, priority = stems)

plot(peaveg.dca, display = "species", type = "n")
points(peaveg.dca, display="species", col = head)

#to abbreviate species names

shnam <- make.cepnames(names(peaveg))
shnam[1:5]

mod<-cca(peaveg)
summary(mod)

pl <- plot(mod, type="n",xlab="CCA 1 (45%)",ylab="CCA 2 (30%)")
pl
points(mod, pch=16, xpd = TRUE,col="black",cex=1.6)
points(peaveg.dca, display = "sp", col = "red")
identify(pl, "sp")
identify(pl, "sp", labels=shnam)
identify(pl, "sites", labels=forests,col="blue",cex=1.7)
forests<- c("I-1","I-2","I-3","I-5","I-6")

stems <- colSums(peaveg)
plot(peaveg.dca, dis="sp", type="n")
points(peaveg.dca, pch=16, xpd = TRUE,col="black",cex=1.6)
identify(pl, "sites", labels=forests,col="blue",cex=1.7)

#giving higher priority to the most abundant species
sel <- orditorp(mod, dis="sp", lab=shnam, priority=stems, pcol = "red", pch="+",cex=1.1)
ordilabel(mod, dis="sp", lab=shnam, priority = stems)

sel <- orditorp(peaveg.dca, dis="sp", priority=stems, pcol = "red", pch="+",cex=1.1)
ordilabel(mod, dis="sp", lab=shnam, priority = stems)

#environmental variables to explain ordination

?envfit
names(peaenvfull2)
ef <- envfit(pea.mds, peaenvfull3, permu = 999)#using decorana
scores(ef,"vectors")
ef$vectors
plot(pea.mds)
plot(ef,p.max = 0.1, col = "blue")
labels(ef)

label(labels(ef)[1])<-"FIRE"

species<-pea.mds3$species
species
ef3 <- envfit(pea.mds3, peaenvfull3, permu = 999)#using decorana
scores(ef3,"vectors")
ef3$vectors
plot(pea.mds3,display=c("sites","species"),cex=1.2)
plot(peasoil.nmds, type = "p",cex=1.4,display=c("sites"),choices=c(1,2))
identify(pea.mds3, "species")
text(pea.mds3, display = c("sites","species"),forests,cex=1.6, col="blue")
plot(ef3,p.max = 0.1, col = "blue")
labels(ef3)

plot(pea.mds3, choices = c(1, 2), type="n") #plots the ordination axes
points(pea.mds3, display = "sites")#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
points(pea.mds3, display = "species")
text(pea.mds3, display = "sites", forests,cex=1.8,col="blue")
text(pea.mds3, display = "species", cex=1.2)

names(peaveg3)
str(peaveg3)
stems <- colSums(peaveg3)
str(peaveg3)
plot(pea.mds3, dis="species", type="n")
points(pea.mds3, pch=16, xpd = TRUE,col="black",cex=1.6)
identify(pl, "sites", labels=forests,col="blue",cex=1.7)

#giving higher priority to the most abundant species
sel <- orditorp(pea.mds3, display="species", priority=stems, pcol = "red", pch="+",cex=1.3)
ordilabel(pea.mds3, dis="species",priority = stems)
plot(ef3,p.max = 0.14, col = "blue",cex=1.6)
labels(ef3)

sel <- orditorp(peaveg.dca, dis="sp", priority=stems, pcol = "red", pch="+",cex=1.1)
ordilabel(mod, dis="sp", lab=shnam, priority = stems)
dim(stems)


#REPEAT ef with less variables, only the most important ones (higher R2)
scores(ef3,"vectors")
plot(pea.mds3, display = "sites",cex=1.4)
summary(peaveg.dca)
identify(pea.mds3, "sites", labels=forests,col="blue",cex=1.7)
plot.envfit(ef, p.max = 0.05,col="blue")

names(peaenvfull2)
ord <- cca(peaveg ~ TN.10.30.C2 + M3P.10.30.C1+ TN.top.C1 +moisture.10.30.C1+AvFe_AvP.top.C1, peaenvfull2)
summary(ord)
ordispider(ord)
vif.cca(ord)
alias(ord)
plot(ord, type="p",xlab="CCA 1 (41.8%)",ylab="CCA 2 (27.6%)")
text(ord, "sites", labels=forests,col="blue",cex=1.7)

ord2 <- cca(peaveg ~ TN.top.C1 + M3P.10.30.C1+ CP.top.C2 +moisture.10.30.C1+AvFe_AvP.top.C1, peaenvfull2)
summary(ord2)
ordispider(ord)
vif.cca(ord2)
alias(ord2)
plot(ord2, type="p",xlab="CCA 1 (41.7%)",ylab="CCA 2 (27.6%)")
text(ord, "sites", labels=forests,col="blue",cex=1.7)

ord3 <- cca(peaveg ~ TN.top.C1 + M3P.10.30.C1 +moisture.10.30.C1+AvFe_AvP.top.C1, peaenvfull2)
summary(ord3)
goodness(ord3,summ=TRUE)
ordispider(ord)
inertcomp(ord3, display = c("species", "sites"),
          statistic = c("explained", "distance"), proportional = FALSE)

vif.cca(ord3)
alias(ord2)
plot(ord3, type="p",xlab="CCA 1 (44.6%)",ylab="CCA 2 (29.1%)",col="black",scaling=2)
text(ord3, "sites", labels=forests,col="black",cex=1.7)
mtext("p=0.007", side=3,cex=1.2,adj=0.02, line=0.3,col="black")
text("p=0.007")


ord4 <- cca(peaveg ~ TN.top.C1 + M3P.10.30.C1 +Fire.Legacy+AvFe_AvP.top.C1, peaenvfull2)
summary(ord4)
anova(ord4)#p=0.001389 **
goodness(ord4)
vif.cca(ord4)
anova(ord4, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated

goodness(ord4, display = c("species", "sites"))
plot(ord4, type="p",xlab="CCA 1 (45.5%)",ylab="CCA 2 (29.5%)")
text(ord4, "sites", labels=forests,col="black",cex=1.7)
mtext("p=0.0014", side=3,cex=1.0,adj=0.02, line=0.3,col="black")

vare.cca <- cca(varespec ~ Al + P*(K + Baresoil), data=varechem)
vare.cca
names(peaenvfull2)

#partial CCA => controlling for Fire Legacy
ord5 <- cca(peaveg ~ TN.top.C1 + M3P.10.30.C1 +Condition(Fire.Legacy)+AvFe_AvP.top.C1, peaenvfull2)
summary(ord5)#89% explained by CCA1 and CCA2 combined
anova(ord5)#p=0.0097 **
goodness(ord5)
vif.cca(ord5)
anova(ord5, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated
goodness(ord4, display = c("species", "sites"))
plot(ord5, type="p",xlab="CCA 1 (55.3%)",ylab="CCA 2 (33.8%)",scaling=2)
text(ord5, "sites", labels=forests,col="black",cex=1.7)
mtext("p=0.038", side=3,cex=1.0,adj=0.02, line=0.3,col="black")

vegandocs("decision")

ord5a <- cca(peaveg ~ NP.10.30.C2+ TN.top.C1 + M3P.10.30.C1 +Condition(Fire.Legacy)+AvFe_AvP.top.C1, peaenvfull2)
summary(ord5a)#89% explained by CCA1 and CCA2 combined
anova(ord5a)#p=0.0097 **
goodness(ord5)
vif.cca(ord5)
anova(ord5, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated


ord5.1 <- cca(peaveg ~ TN.top.C2 + M3P.top.C2 +Condition(Fire.Legacy)+AvFe_AvP.top.C2, peaenvfull2)
summary(ord5.1)
anova(ord5.1)#not significant
vif.cca(ord5.1)
anova(ord5.1, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated

ord5.2 <- cca(peaveg ~ TN.top.C2 + M3P.10.30.C2 +Fire.Legacy+AvFe_AvP.top.C2, peaenvfull2)
summary(ord5.2)#75.4% explained by CCA1 and CCA2 combined
anova(ord5.2)#p=0.53ns
goodness(ord5.2)
vif.cca(ord5.2)
anova(ord5.2, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated

ord5.3 <- cca(peaveg ~ C.top.C2 + NP.10.30.C2 +Fire.Legacy+PMN.top.C2, peaenvfull2)
summary(ord5.3)#68% explained by CCA1 and CCA2 combined
anova(ord5.3)#p=0.088ns
goodness(ord5.3)
vif.cca(ord5.3)
anova(ord5.3, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated
plot(ord5.3, type="p",xlab="CCA 1 (45.6%)",ylab="CCA 2 (28.8%)",scaling=2)
text(ord5.3, "sites", labels=forests,col="black",cex=1.7)
mtext("p=0.09", side=3,cex=1.0,adj=0.02, line=0.3,col="black")


ord5.4 <- cca(peaveg ~ C.top.C2 + NP.10.30.C2 +Condition(Fire.Legacy)+PMN.top.C2, peaenvfull2)
summary(ord5.4)#91% explained by CCA1 and CCA2 combined
anova(ord5.4)#p=0.27ns
goodness(ord5.4)
vif.cca(ord5.4)
anova(ord5.4, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated
plot(ord5.4, type="p",xlab="CCA 1 (57.2%)",ylab="CCA 2 (33.8%)",scaling=2)
text(ord5.4, "sites", labels=forests,col="black",cex=1.7)
mtext("p=0.27", side=3,cex=1.0,adj=0.02, line=0.3,col="black")


ord6 <- cca(peaveg ~ TN.top.C1  +Condition(Fire.Legacy)+pH.H2O.10.30.C1, peaenvfull2)
summary(ord6)
anova(ord6)
vif.cca(ord6)
anova(ord6, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated


fit <- envfit(ord5, peaenvfull2, perm = 999, display = "wa")
fit
pl2<-plot(fit, p.max = 0.05, col = "red")
pl2
identify(pl2, "sites",col="blue",cex=1.7)

#'scaling' must be set similarly in envfit and in ordination plot
pl2<-plot(pea.mds03, type = "p", scaling = "sites")
pl2
identify(pl2, "sites",labels=forests,col="brown",cex=1.5)

fit <- envfit(ord3, peaenvfull2, perm = 0, display = "lc", scaling = "sites")
fit
plot(fit,  p.max = 0.05,col = "red")

names(peaenvfull2)
colnames(peaenvfull)[35] <- "pH_10.30_NB"
colnames(peaenvfull2)[1] <- "FIRE"
colnames(peaenvfull)[18] <- "PMN_0.10_B"
colnames(peaenvfull)[23] <- "avP_10.30_NB"

#using significant vectors: FIRE, PMN TOP C2, M3P.10.30.C1,TN.10.30.C2,PH.H2O.10.30.C1,
#The first two columns give direction cosines of the vectors, and r2 gives
#the squared correlation coefficient

#it implies a linear relationship between ordination and environment: direction and
#strength are all you need to know. This may not always be appropriate

#Function ordisurf fits surfaces of environmental variables to ordinations
#It uses generalized additive models in function gam of package mgcv
#Function gam uses thinplate splines in two dimensions, and automatically
#selects the degree of smoothing by generalized cross-validation

#If the response really is linear and vectors are appropriate, the fitted surface
#is a plane whose gradient is parallel to the arrow, and the fitted
#contours are equally spaced parallel lines perpendicular to the arrow.

#We make vector fitting for selected
#variables and add fitted surfaces in the same plot.

str(peaenvfull2)
peaenvfull2$FIRE=as.integer(peaenvfull$FIRE)

#DO NMDS!!!

ef <- envfit(pea.mds ~ PMN_0.10_B+avP_10.30_NB+pH_10.30_NB+FIRE, peaenvfull2)
#PMN is linear, avP is nonlinear, pH is linear
plot(pea.mds, display = "sites")
text(pea.mds,display="sites",col="black",cex=1.8)
plot(ord3)
tmp <- with(peaenvfull2, ordisurf(pea.mds, PMN_0.10_B, add = TRUE,knots=5))
with(peaenvfull2, ordisurf(ord3, TN.top.C1, add = TRUE, col = "green4",knots=5))
with(peaenvfull2, ordisurf(ord3, AvFe_AvP.top.C1, add = TRUE, col = "gray4",knots=5))
with(peaenvfull2, ordisurf(ord3, moisture.10.30.C1, add = TRUE, col = "purple4",knots=5))
with(peaenvfull2, ordisurf(ord3, M3P.10.30.C1, add = TRUE, col = "purple4",knots=5))


tmp
plot(tmp)

fitted(ef)

#Example for use of categorical variables

library(dplyr)
data(dune)
str(dune)
data(dune.env)
str(dune.env)

dune.ca <- cca(dune)
ef <- envfit(dune.ca, dune.env, permutations = 999)
ef

#Correspondence analysis is a weighted ordination method, and vegan
#functions envfit and ordisurf will do weighted fitting, unless the user
#specifies equal weights.

plot(ord3, display = "sites", type = "p")
with(dune.env, ordiellipse(dune.ca, Management, kind = "se", conf = 0.95))
with(dune.env, ordispider(dune.ca, Management, col = "blue", label= TRUE))
with(dune.env, ordihull(dune.ca, Management, col="blue", lty=2))
str(peaANF)
levels(peaANF$fire.scar)
peaANF <- mutate(peaANF,
                   oFIRE = ordered(fire.legacy, levels = c('2','3','4','5')))

levels(peaANF$fire.scar)
peaANF <- mutate(peaANF,
                 oFSCAR = ordered(fire.scar, levels = c('1','2')))

#CONSTRAINED ORDINATION

#In constrained ordination we do not want to display all or even most of the compositional variation, but only the variation that can be explained
#by the used environmental variables, or constraints. 

#The constrained ordination is non-symmetric: we have “independent” variables or constraints and we
#have “dependent” variables or the community.

#Constrained ordination rather is related to multivariate linear models

#The recommended way of defining a constrained model is to use model formula.

#THIS IS CONSTRAINED CCA => THAT'S WHAT I WANT TO USE FOR FINAL ANALYSIS
#SOIL VARIABLES ON RIGHT SIDE ARE THE CONSTRAINTS
modb <- cca(peaveg ~ FIRE + CN.top.C1 + Soil.ANF.top.C1 + pH.top.C2, data=peaenvfull2)
summary(modb)
anova(ord3)#p = 0.060
anova(ord3, by = "term", step=200)#anova type I
anova(ord3, by = "margin", perm=500)#anova type III => independent of the order of the terms, but correlated
vif.cca(ord3)
#terms will get higher (“worse”) P-values
anova(ord3, by="axis", perm=1000)

library(vegan3d)
ordiplot3d(ord3, type = "h")

#unconstrained
mod1 <- cca(varespec ~ ., varechem)
mod1

data(varespec)
data(varechem)

#using AIC to help in model selection
mod0 <- cca(varespec ~ 1, varechem)
mod <- step(mod0, scope = formula(mod1), test = "perm")
vif.cca(mod1)#variance inflation factors
#A common rule of thumb is that vif > 10 indicates that a variable is
#strongly dependent on others and does not have independent information
#On the other hand, it may not be the variable that should be removed,
#but alternatively some other variables may be removed

#Species-Environment Correlation
spenvcor(ord3)
#Correlation coefficient is very sensitive to single extreme values
#eigenvalue is a more appropriate measure of similarity

#Weighted average scores are preferable
#The practical reason to prefer wa scores is that they are more robust against random error in environmental variables
#All ecological observations have random error, and therefore it is better to use scores that are resistant to this variation
#The wa scores are based on community composition, but so that they are as similar as
#possible to the constraints.

#BIPLOTS
#The Arrows are based on (weighted) correlation of lc scores and environmental
#variables. They are scaled to unit length in the constrained ordination of full rank

#In vegan the biplot arrows are always scaled similarly irrespective of
#scaling of sites or species. With default scaling = 2, the biplot arrows
#have optimal relation to sites, but with scaling = 1 they rather are related to species

pred <- calibrate(ord4)
head(pred)

#using gam to check the linearity hypothesis of the biplot method

plot(ord4, display = c("bp", "wa", "lc"))
ef <- with(peaenvfull2, ordisurf(ord4, TN.top.C1, display = "lc", add = TRUE))
plot(ef)
#Function performs weighted fitting, and the model
#should be consistent with the one used in arrow fitting

#Conditioned or partial models
#The effect of some environmental variables can be removed from the ordination
#before constraining with other variables. The analysis is said to
#be conditioned on variables, or in other words, it is partial after removing
#variation caused by some variables

#what would be the effect of designed Management after
#removing the natural variation caused by Moisture

dune.cca <- cca(dune ~ Management + Condition(Moisture), dune.env)
plot(dune.cca)
dune.cca
summary(dune.cca)
#need to contrast with model fit with Management only and compare constrained inertia
#to see which is higher

anova(dune.cca, perm.max = 2000)
anova(cca(dune ~ Management, dune.env))
with(dune.env, anova(dune.cca, strata = Moisture))

#Dissimilarities and Environment

#recommended method in vegan is adonis which
#implements a multivariate analysis of variances using distance matrices

#adonis: Multivariate ANOVA based on dissimilarities

betadiver(help=TRUE)
betad <- betadiver(peaveg3, "w")
betad
beta_mds<-metaMDS(betad,k=2,try=500)
beta_mds
plot(beta_mds)
summary(betad)
plot(betad)

betad <- betadiver(peaveg2, "w")#Wittaker
betad

sol4 <- bioenv(betad, peaenvfull3,method="spearman",index="bray",metric="euclidean",upto=4)
summary(sol4)
sol5<-bioenvdist(sol4,which="best")
sol5

str(peaveg2)
betad2 <- betadiver(peaveg2, "w")#Wittaker
betad2

sol6 <- bioenv(betad2, peaenvfull3,method="spearman",index="bray",metric="euclidean",upto=4)
summary(sol6)
sol7<-bioenvdist(sol6,which="best")
sol7

names(peaenvfull3)
vec1 <- bioenv(wisconsin(peaveg3) ~ TP.B, peaenvfull3)
vec1
summary(vec1)

vec2 <- bioenv(wisconsin(peaveg3) ~ AGB, peaenvfull3)
vec2
summary(vec2)

vec3 <- bioenv(wisconsin(peaveg3) ~ ANF.NB+TP.B, peaenvfull3)
vec3
summary(vec3)


summary(res)
summary(sol4)
summary(betad)
plot(betad)
range(betad-vegdist(peaveg2,binary=TRUE))

names(peaenvfull2)

#function adonis is analogous to multivariate analysis of variance
m1<-adonis(betad ~ Fire.Legacy,peaenvfull2, perm=2000,method="bray",by="margin")
m1
Family1<-adonis(betad2 ~ TP.B, peaenvfull3, perm = 2000,method="bray")#significant
Family1#R2 = 0.487, p=0.04

Family2<-adonis(betad2 ~AvFe_AvP.NB, peaenvfull3, perm = 2000,method="bray")#significant
Family2#R2 = 0.378, p=0.14

TC2<-adonis(betad2 ~ TC.B+pH.H2O.B, peaenvfull3, perm = 2000,method="bray")#significant
TC2

TC3<-adonis(betad2 ~ TC.B+AvFe.B, peaenvfull3, perm = 2000,method="bray")#significant
TC3#73% explained

TC4<-adonis(betad2 ~ TC.B+TP.NB, peaenvfull3, perm = 2000,method="bray")#significant
TC4#72% explained

adonis(betad ~ M3P.top.C1, peaenvfull2, perm = 2000)#significant, 35.5% of variation

adonis(betad ~ C.top.C1, peaenvfull2, perm = 2000)#significant

adonis(betad ~ AvFe_AvP.top.C1, peaenvfull2, perm = 2000)#significant

adonis(betad ~ pH.H2O.10.30.C1, peaenvfull3, perm = 2000)#very significant, 46.7%

adonis2(betad ~ pH.H2O.10.30.C1, peaenvfull2, perm = 999,by="margin")#very significant, best so far

adonis(betad ~ NP.10.30.C2, peaenvfull3, perm = 2000)#significant, 47.3% of variation

adonis(betad ~ NP.10.30.C2+pH.H2O.10.30.C1, peaenvfull2, perm = 2000)#significant

#ECODIST PACKAGE proofing of correlations between soil and vegetation
library(ecodist)
data(graze)
str(graze)
speciesdata <- graze[, 3:7]
str(speciesdata)
str(peaveg3)
data.matrix(peaveg2)
str(peaveg2)
peaveg2=as.numeric(peaveg2)
str(peaveg2)
peaveg2


soilfam<-cor2m(peaenvfull3[1:36], peafamily,trim = TRUE, alpha = 0.05)
soilfam
options(max.print=1000000)

soilfam2<-cor2m(peaenvfull3[1:36], peafamily,trim = TRUE, alpha = 0.1)
soilfam2

distance(peafamily, method = "bray-curtis")#family level

soilsp<-cor2m(peaenvfull3[1:36], peasp2,trim = TRUE, alpha = 0.05)
soilsp
options(max.print=1000000)

soilsp2<-cor2m(peaenvfull3[1:36], peasp2,trim = TRUE, alpha = 0.1)
soilsp2

sp.d <- distance(peaveg2,method="bray-curtis")
sp.d

sp.nmds <- nmds(sp.d, nits=20, mindim=1, maxdim=4)
sp.nmds
sp.nmin <- min(sp.nmds, dims=2)
sp.nmin#stress=0.04565772
plot(sp.nmds)
sp.rot <- princomp(sp.nmin)$scores# rotate the configuration to maximize variance
# rotation preserves distance apart in ordination space
cor(dist(sp.nmin), dist(sp.rot))

peasp2[1,]
sp.vf <- vf(sp.nmin,peaveg2, nperm=1000)#vf is vector fitting
sp.vf
sp.vfrot <- princomp(sp.rot)$scores# rotate the configuration to maximize variance
# rotation preserves distance apart in ordination space
cor(dist(sp.nmin), dist(sp.rot))
str(peaveg2)
str(peasp2)
names(peaveg2)
peaveg2[1,]
data(iris)
str(iris)

plot(sp.nmin, main="NMDS")
par(mfrow = c(1,1))
plot(sp.vf,pval=0.1)

soil.d <- distance(peaenvfull3[1:36],method="euclidean")
soil.d
soil.nmds <- nmds(soil.d, nits=20, mindim=1, maxdim=4)
soil.nmds
soil.nmin <- min(soil.nmds, dims=2)
soil.nmin#stress=0.06638653
names(soil.nmin)
plot(soil.nmds)
soil.rot <- princomp(soil.nmin)$scores
# rotation preserves distance apart in ordination space
cor(dist(soil.nmin), dist(soil.rot))

soil.vf <- vf(soil.nmin,peaenvfull3[1:36], nperm=1000)#vf is vector fitting
soil.vf

#FINAL NMDS PLOT AT SPECIES LEVEL
peaveg2$sitelabels=c("I1","I2","I3","I5","I6")
peaveg2$sitelabels
par(mfrow = c(1,1))

plot(sp.nmin, main="NMDS Community species by soil",type="p",cex=1.2)#the best two dimensions for species-level
plot(soil.vf,pval=0.1)#plotting soil vectors with p>0.1
plot(soil.vf,pval=0.05)#plotting soil vectors with p>0.1
text(sp.nmin, display = c("sites", "species"),cex=1.8, color="blue",
     choices = c(1,2))

#FINAL NMDS PLOT AT FAMILY LEVEL
fam.d <- distance(peaveg3,method="bray-curtis")
fam.d

fam.nmds <- nmds(fam.d, nits=20, mindim=1, maxdim=4)
fam.nmds
fam.nmin <- min(fam.nmds, dims=2)
fam.nmin#stress=0.04384329
plot(fam.nmds)
fam.rot <- princomp(fam.nmin)$scores# rotate the configuration to maximize variance
# rotation preserves distance apart in ordination space
cor(dist(fam.nmin), dist(fam.rot))

peasp2[1,]
#fitting family vectors onto family ordination
fam.vf <- vf(fam.nmin,peaveg3, nperm=1000)#vf is vector fitting
fam.vf
fam.vfrot <- princomp(fam.vf)$scores# rotate the configuration to maximize variance
# rotation preserves distance apart in ordination space
cor(dist(fam.vf), dist(fam.vfrot))
str(peaveg3)
str(peasp2)
names(peaveg2)
peaveg2[1,]
data(iris)
str(iris)

par(mfrow = c(1,1))
plot(fam.nmin, main="NMDS")
plot(fam.vf,pval=0.1)
text(fam.nmin, display = c("sites", "species"),cex=1.8, color="blue",
     choices = c(1,2))

soil.d <- distance(peaenvfull3[1:36],method="euclidean")
soil.d
soil.nmds <- nmds(soil.d, nits=20, mindim=1, maxdim=4)
soil.nmds
soil.nmin <- min(soil.nmds, dims=2)
soil.nmin#stress=0.06638653
names(soil.nmin)
plot(soil.nmds)
soil.rot <- princomp(soil.nmin)$scores
# rotation preserves distance apart in ordination space
cor(dist(soil.nmin), dist(soil.rot))

soil.vf <- vf(soil.nmin,peaenvfull3[1:36], nperm=1000)#vf is vector fitting
soil.vf
soil.vfrot<-princomp(soil.vf)$scores
cor(dist(soil.vf), dist(soil.vfrot))

plot(soil.nmin, main="NMDS Sites by soil properties",type="p",cex=1.2)#the best two dimensions for species-level
text(soil.nmin, display = c("sites", "species"),cex=1.8, color="blue",
     choices = c(1,2))

#soil BURNED vs UNBURNED sites
peaenvfire
peafire<-peaenvfire[c(2:3,5:18)]
dim(peafire)
fire.d <- distance(peafire,method="bray-curtis")
fire.d
fire.nmds <- nmds(fire.d, nits=500, mindim=1, maxdim=4)
fire.nmds
fire.nmin <- min(fire.nmds, dims=2)
fire.nmin#stress=0.08241508
names(fire.nmin)
plot(fire.nmds)
fire.rot <- princomp(fire.nmin)$scores
# rotation preserves distance apart in ordination space
cor(dist(fire.nmin), dist(fire.rot))

fire.vf <- vf(fire.nmin,peafire, nperm=1000)#vf is vector fitting
fire.vf
fire.vfrot<-princomp(fire.vf)$scores
cor(dist(fire.vf), dist(fire.vfrot))

par(mfrow = c(1,1))
plot(fire.nmin, main="NMDS Site Fire Presence by Soil Variables",type="p",cex=1.2)#the best two dimensions for species-level
text(fire.nmin, display = c("sites", "species"),cex=1.8, color="blue",
     choices = c(1,2))
plot(fire.vf,pval=0.1)#plotting soil vectors with p>0.1

#FINAL NMDS PLOT AT FAMILY LEVEL
peaveg2$sitelabels=c("I1","I2","I3","I5","I6")
peaveg2$sitelabels
par(mfrow = c(1,1))

plot(fam.nmin, main="NMDS Community species by soil",type="p",cex=1.2)#the best two dimensions for species-level
plot(fam.rot, main="NMDS Community species by soil",type="p",cex=1.2)#the best two dimensions for species-level
text(fam.rot, display = c("sites", "species"),cex=1.8, color="blue",
     choices = c(1,2))
plot(fam.vf,pval=0.1)
plot(fam.vfrot,pval=0.1)
plot(soil.vf,pval=0.1)#plotting soil vectors with p>0.1
plot(soil.vf,pval=0.05)#plotting soil vectors with p>0.1
text(soil.vf, display = c("sites", "species"),cex=1.8, color="blue",
     choices = c(1,2))
text(fam.nmin, display = c("sites", "species"),cex=1.8, color="blue",
     choices = c(1,2))



#PLOTTING THE ROTATED ONES
plot(sp.nmin, col=as.numeric(iris$Species), pch=as.numeric(iris$Species), main="NMDS")
plot(sp.vf)
plot(sp.rot, main="Rotated NMDS")
plot(sp.vfrot)

#pea_all dataset
library(ggplot2)
library(ggpubr)
library(magrittr)
names(pea_all)
head(pea_all)

aggregate(pea_all$Basal.Area..m2.,by=list(SITE=pea_all$Plot.Code, fam=pea_all$Family), sum,
          na.rm=TRUE)
aggregate(pea_all$Basal.Area..m2.,by=list(SITE=pea_all$Plot.Code), sum,
          na.rm=TRUE)
fab_pea1=(6.317748338/19.10778)*100
fab_pea1

fab_pea2=(1.596013895/21.98321)*100
fab_pea2

fab_pea3=(1.112560482/36.22848)*100
fab_pea3

fab_pea5=(2.838346501/28.70479)*100
fab_pea5

fab_pea6=(0.947173692/15.33413)*100
fab_pea6

fabaceae <-pea_all[pea_all$Family == "Fabaceae",]
fabaceae
str(fabaceae)
str(pea_all)
head(fabaceae)
abundancefab=(331/5419)*100
abundancefab
levels(fabaceae$Plot.Code)=c("I1","I2","I3","I5","I6")


p_all <- ggboxplot(fabaceae, x = "Plot.Code", y = "Basal.Area..m2.",
                 palette ="npg", color="Genus",
                 add = "jitter", fill="lightgray",width=1.0,ylab="Tree Basal Area (m2 ha-1)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),order=c("I3","I1","I2","I5","I6"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_all
ggpar(p_all,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))



p3<-facet(p2 + theme_bw(), facet.by = "Depth",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.scar=c("NB","B")))

ggpar(p3,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Vegetation",font.legend = c(20,"bold"))

p_all2 <- ggboxplot(fabaceae, x = "Plot.Code", y = "Height..m.",
                   palette ="npg", color="Genus",
                   add = "jitter", fill="lightgray",width=1.0,ylab="Tree height (m)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),order=c("I3","I1","I2","I5","I6"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_all2
ggpar(p_all2,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

p_all3 <- ggscatter(pea_all, x = "D..m.", y = "Height..m.",add="reg.line",
                    palette ="npg",color="Plot.Code",
                  ylab="Tree height (m)",cex=20,short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_all3
ggpar(p_all3,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Diameter (m)",font.legend = c(20,"bold"))

p_fab <- ggscatter(fabaceae, x = "D..m.", y = "Height..m.",conf.int=TRUE,add.params=list(color="blue",fill="ligthgray"),
                    palette ="npg",color="Plot.Code",
                    ylab="Tree height (m)",cex=20,short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_fab+stat_cor(method = "spearman", label.x = 0.15, label.y = 17)

p_all_1 <- ggscatter(pea_all, x = "D..m.", y = "Height..m.",conf.int=TRUE,add.params=list(color="blue",fill="ligthgray"),
                   palette ="npg",color="Plot.Code",ylim=c(0,25),
                   ylab="Tree height (m)",cex=20,short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_all_1+stat_cor(method = "spearman", label.x = 0.15, label.y = 17)

ggscatter(fabaceae, x = "Basal.Area..m2.", y = "Height..m.",
          add = "reg.line", color="Plot.Code",                                # Add regression line
          conf.int = TRUE, ylim=c(0,20),                               # Add confidence interval
          add.params = list(color = "blue",
                            fill = "lightgray"))
+stat_cor(method = "pearson", label.x = 0.15, label.y = 15)
ggpar(p_fab,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Diameter (m)",font.legend = c(20,"bold"))

p_fab2 <- ggscatter(fabaceae, x = "Basal.Area..m2.", y = "Height..m.",
                   palette ="npg",color="Plot.Code",
                   ylab="Tree height (m)",cex=20,short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_fab2
ggpar(p_fab2,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Diameter (m)",font.legend = c(20,"bold"))


p_all4 <- ggscatter(pea_all, x = "Basal.Area..m2.", y = "Height..m.",
                    palette ="npg",color="Plot.Code",
                    ylab="Tree height (m)",cex=20,short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_all4
ggpar(p_all4,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Tree basal area (m2)",font.legend = c(20,"bold"))

p_all5 <- ggscatter(fabaceae, x = "Basal.Area..m2.", y = "Height..m.",
                    palette ="npg",color="Plot.Code",shape="Genus",
                    ylab="Tree height (m)",cex=20,short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_all5
ggpar(p_all5,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Tree basal area (m2)",font.legend = c(20,"bold"))

vochy <-pea_all[pea_all$Family == "Vochysiaceae",]
vochy

names(vochy)
p_all_v <- ggboxplot(pea_all, x = "Plot.Code", y = "Basal.Area..m2.",
                   palette ="npg",
                   add = "jitter", fill="lightgray",width=1.0,ylab="Basal Area (m2 ha-1)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),order=c("PEA-03","PEA-01","PEA-02","PEA-05","PEA-06"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_all_v
ggpar(p_all_v,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

