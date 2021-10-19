#vegetation analysis final
#ms PEA chapter2

library(vegan)
library(lattice)
library(permute)
library(MASS)

peaveg2<-read.csv(file.choose())#PEA sp matrix dbh 10 updated
attach(peaveg2)
dim(peaveg2)
str(peaveg2)
names(peaveg2)

peasp3<-data.matrix(peaveg2[2:44])#use this one for updated dbh10
str(peasp3)
peasp3[1:43]=as.numeric(peasp3[1:43])
str(peasp3)
peasp3

vegfb<-data.matrix(peaveg2[,2:44])#use this one for updated dbh30
str(vegfb)
head(vegfb)
vegfb[1:43]=as.numeric(vegfb[1:43])
str(vegfb)
rownames(vegfb) <- peaveg2$Stand

pea_ba<-read.csv(file.choose())#BA_sp_matrix_updated_DBH_10
attach(pea_ba)
dim(pea_ba)
str(pea_ba)
basal_area<-pea_ba[,2:44]
names(basal_area)
hist(basal_area$Tachigali.bracteosa)

peaenvfull3<-read.csv(file.choose())#PEA ENV DATA FULL_updated_noI7
attach(peaenvfull3)
dim(peaenvfull3)
names(peaenvfull3)

peasoil3a <- peaenvfull3[c(1:4,7:22,31:32)]#based on findings of geochemMDS2
names(peasoil3a)
peasoil3_B<-peasoil3a[c(1,4:24)]

peaenvfull3a<-read.csv(file.choose())#PEA_env_data_averages_noI7
attach(peaenvfull3a)
dim(peaenvfull3a)
names(peaenvfull3a)

#use peasp3 and peasoil3a for final NMDS

#Non-metric multidimensional scaling (NMDS) can be performed using isoMDS function
#in the MASS package => IT IS A NONLINEAR METHOD 
#This function needs dissimilarities as input.

data(BCI)
class(BCI)
class(peasp3)
peasp4<-as.data.frame(peasp3)
peasp4
H<-diversity(peasp4, index = "shannon")
H
S <- specnumber(peasp4) ## rowSums(BCI > 0) does the same...
S
J <- H/log(S)
J
vare.dis_updated <- vegdist(peasp3, method="bray")#dissimilarities
vare.dis_updated

pea.mds_updated <- metaMDS(vare.dis_updated,trymax=900)#species level based on dissimilarities
pea.mds_updated
summary(pea.mds_updated)
plot(pea.mds_updated)#species level

new_mds <- vegdist(decostand(peasp3,"norm"), "bray")
new_mds

pea.mds_up2 <- metaMDS(new_mds,trymax=1000)#species level based on dissimilarities
pea.mds_up2 #try using this one today
plot(pea.mds_up2)

ef_final03 <- envfit(pea.mds_up2, peaenvfull3, permu = 999)#using decorana
ef_final03

ef_final03a <- envfit(pea.mds_up2,peasoil3a, permu = 999)#using decorana
ef_final03a

plot(pea.mds_up2,type = "p",cex=1.2,display=c("sites","species"),choices=c(1,2),scaling = "sites")
text(pea.mds_up2, display = c("sites", "species"),forests,cex=1.8, color="blue",
     choices = c(1,2))
plot(ef_final03a,  p.max = 0.1,col = "blue")#based on veg distance

#Adonis
names(peasoil3_B)
adonis (dis_mds_updated  ~ Fire.Legacy+TC.B+TP.B, data = peasoil3, permutations = 999,by="margin")

#final ms
adonis (dis_mdsBA ~ TC+Fire.Legacy+PMN.B, data = peasoil3, permutations = 999, by="margin")

##########################
#For Basal Area

vare.dis_ba <- vegdist(basal_area, method="bray")#dissimilarities
vare.dis_ba

dis_mds_ba2 <- vegdist(decostand(basal_area,"norm"), "bray")
dis_mds_ba2

pea.mds_ba <- metaMDS(dis_mds_ba2,trymax=900)#species level BA based on dissimilarities
pea.mds_ba
summary(pea.mds_ba)
plot(pea.mds_ba)#species level BA

ef_BA <- envfit(pea.mds_ba,peasoil3_B, permu = 999)#using decorana
ef_BA
par(mfrow = c(1,1))
plot(pea.mds_ba, type = "t")
plot(scores(pea.mds_ba))
points(pea.mds_ba,display="sites")

ef_ba <- envfit(pea.mds_ba,peasoil3b, permu = 999)#using decorana
ef_ba

#final ms
names(peasoil3_B)
adonis (dis_mds_ba2 ~ TC+Fire.Legacy+PMN.B, data = peasoil3, permutations = 999, by="margin")


#FINAL PLOT FOR BASAL AREA AND SOIL
plot(pea.mds_ba,type = "p",cex=1.2,display=c("sites","species"),choices=c(1,2),scaling = "sites")
text(pea.mds_ba, display = c("sites", "species"),forests,cex=1.8, color="blue",
     choices = c(1,2))
plot(ef_BA,p.max = 0.15, col = "blue")

plot(bc.mds, choices = c(1, 2), type="n") #plots the ordination axes
points(bc.mds, display = c("sites", "species"))#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
text(bc.mds, display = c("sites", "species"),cex=1.6)

bci.mds<-metaMDS(peaveg2, distance = "bray", k = 2, trymax = 20, autotransform =TRUE, noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE) #makes the object bci.mds using Bray-Curtis ordination
bci.mds
plot(bci.mds, choices = c(1, 2), type="n") #plots the ordination axes
points(bci.mds, display = c("sites", "species"))#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
text(bci.mds, display = c("sites", "species"),cex=1.6)

