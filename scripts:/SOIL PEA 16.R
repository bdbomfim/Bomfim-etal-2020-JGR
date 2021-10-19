#CHAPTER 3 SOIL PEA 16

pea2<-read.csv(file.choose())#ALL SOIL PEA 16_updated_noI7
attach(pea2)
names(pea2)
str(pea2)#n=182

grasses <-pea2[pea2$Vegetation == "grassland",]
grasses

Forests <-pea2[pea2$Vegetation == "forest",]
levels(Forests$Site)
str(Forests)

peaANF<-read.csv(file.choose())#MEAN SOILS PEA
attach(peaANF)
names(peaANF)
str(peaANF)
pea2$Fire.Scar=as.factor(pea2$Fire.Scar)
pea2$Fire.legacy=as.factor(pea2$Fire.legacy)
levels(pea2$Fire.Presence)

levels(pea2$Fire.Presence)

pea2 <- mutate(pea2,
                   oFP = ordered(Fire.Presence, levels = c('NB','B')))
pea2 <- mutate(pea2,
               oSite = ordered(Site, levels = c('I3','I1','I2','I5','I6')))

names(Forests)
levels(pea2$Site)
aggregate(pea2$mean.ANF,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$mean.ANF,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth),sd,na.rm=TRUE)
aggregate(pea2$CN,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$CN,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$PMN,by=list(SITE=pea2$Site, cluster=pea2$Cluster,depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$PMN,by=list(SITE=pea2$Site, cluster=pea2$Cluster,depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$NP,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$NP,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$pH_H2O,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$pH_H2O,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea$lab.moisture_.,by=list(SITE=pea$Site, cluster=pea$Cluster, depth=pea$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$M3Fe_M3P,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$M3Fe_M3P,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$M3Fe_microg_g.dw,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$TP.g_kg,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$TP.g_kg,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$CP,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$CP,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea$Tfe_g_kg,by=list(SITE=pea$Site, cluster=pea$Cluster, depth=pea$Depth), mean,
          na.rm=TRUE)
aggregate(pea$Fe.P,by=list(SITE=pea$Site, cluster=pea$Cluster, depth=pea$Depth), mean,
          na.rm=TRUE)
aggregate(Forests$TC_g_kg,by=list(FP=Forests$Fire.Presence, D=Forests$Depth), mean,
          na.rm=TRUE)
aggregate(Forests$TN_g_kg,by=list(FP=Forests$Fire.Presence, D=Forests$Depth), mean,
          na.rm=TRUE)
aggregate(Forests$CN,by=list(FP=Forests$Fire.Presence, D=Forests$Depth), mean,
          na.rm=TRUE)
aggregate(Forests$M3P,by=list(FP=Forests$Fire.Presence, D=Forests$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$TC_g_kg,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), FUN="se",
          na.rm=TRUE)
aggregate(pea2$M3P,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$TC_g_kg,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$TC_g_kg,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$TN_g_kg,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$TN_g_kg,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$pH_KCl,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$pH_KCl,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$Tfe_g_kg,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$Tfe_g_kg,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$Fe.P,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), mean,
          na.rm=TRUE)
aggregate(pea2$Fe.P,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sd,
          na.rm=TRUE)
aggregate(pea2$Fe.P,by=list(SITE=pea2$Site, cluster=pea2$Cluster, depth=pea2$Depth), sum,
          na.rm=TRUE)
names(pea)

summary(pea)

library(pastecs)
library(ggplot2)
library(dplyr)
library(psych)
library(sjstats)
library(ggpubr)
library(magrittr)

library(vegan)
library(MASS)

library(mgcv)
library(nlme)

stat.desc(pea)
describe(pea)
describeBy(pea, Site)


Forests <- mutate(Forests,
               oSite = ordered(Site, levels = c('I3','I1','I2','I5','I6')))

Forest6<-Forests[Forests$Site == "I6",]
Forest6

Forest3<-Forests[Forests$Site == "I3",]
str(Forest3)

Forest5<-Forests[Forests$Site == "I5",]
Forest6

Forest2<-Forests[Forests$Site == "I2",]
str(Forest3)

Forest1<-Forests[Forests$Site == "I1",]
str(Forest3)

High<-Forests[Forests$Fire.Class == "high",]
High

#target these pH_H2O	CN	NP	PMN	TP g_kg	Fe/P	M3Fe_M3P	CP as predictors for ANF


shapiro.test(pea2$mean.ANF)
pea2$logANF=log10(pea2$mean.ANF)
shapiro.test(pea2$logANF)#NORMAL

shapiro.test(pea2$PMN)
pea2$log_pmn=log10(pea2$PMN)
shapiro.test(pea2$log_pmn)#NORMAL

shapiro.test(pea2$CN)#NORMAL

pea2$logavP=log10(pea2$M3P)
shapiro.test(pea2$logavP)

shapiro.test(Forests$mean.ANF)
Forests$log_anf=log10(Forests$mean.ANF)
shapiro.test(Forests$log_anf)#NORMA

shapiro.test(pea2$TN_g_kg)
pea2$logN=log10(pea2$TN_g_kg)
shapiro.test(pea2$logN)#NORMAL

#Forests variables 
shapiro.test(Forests$TN_g_kg)
shapiro.test(Forests$TC_g_kg)#NORMAL
shapiro.test(Forests$CN)
shapiro.test(Forests$NP)
Forests$logNP=log10(Forests$NP)
shapiro.test(Forests$logNP)#NORMAL

shapiro.test(Forests$log_pmn)
Forests$logPMN=log10(Forests$PMN)
shapiro.test(Forests$logPMN)#NORMAL

shapiro.test(Forests$Tfe_g_kg)#NORMAL - helps explain PMN

shapiro.test(Forests$M3Fe_microg_g.dw)
Forests$logM3Fe=log10(Forests$M3Fe_microg_g.dw)
shapiro.test(Forests$logM3Fe)#NORMAL

shapiro.test(Forests$M3P)
Forests$logM3P=log10(Forests$M3P)
shapiro.test(Forests$logM3P)#NORMAL

shapiro.test(Forests$Fe.P)
Forests$logFe.P=log10(Forests$Fe.P)
shapiro.test(Forests$logFe.P)#NORMAL

shapiro.test(Forests$TP.g_kg)

shapiro.test(Forests$lab.moisture_.)
Forests$logmoisture=log10(Forests$lab.moisture_.)
shapiro.test(Forests$logmoisture)#NORMAL

shapiro.test(Forests$Cfe)
Forests$logCfe=log10(Forests$Cfe)
shapiro.test(Forests$logCfe)#NORMAL

shapiro.test(Forests$CP)
Forests$logCP=log10(Forests$CP)
shapiro.test(Forests$logCP)#NORMAL

shapiro.test(Forests$PMN.M3P)
Forests$logPMNAVP=log10(Forests$PMN.M3P)
shapiro.test(Forests$logPMNAVP)#NORMAL

#GAM MODELING TO EXPLORE DATA AND RELATIONSHIPS BETWEEN VARIABLES
names(Forests)
Y <- cbind(Forests$logANF,Forests$logN,Forests$PMN)
fit <- manova(Y ~ Forests$Site*Forests$Fire.Scar)
summary(fit, test="Pillai")

#ex: N ~ DOW + SEX + Year + s( DOY, bs = "cc" ) + s( AgeCut), s(AgeCut, by = SEX ) + s(AgeCut, by = DOW)

gamc<-gam(TC_g_kg~Site*Fire.Presence*Depth,data=Forests)
summary(gamc)
anova(gamc)
plot.gam(gamc,pages=1)
plot.gam(gamc, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Site", xlabs="Partial effect of Site on Soil C", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamc, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Fire presence", xlabs="Partial effect of Fire presence on Soil C", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamc, select=3, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
gam.check(gamc)

names(Forests)
levels(Forests$Fire.Presence)
pc <- ggboxplot(Forests, x = "Site", y = "TC_g_kg",
                color = "Fire.frequency",palette ="npg",
                add = "jitter", fill="lightgray", shape="Fire.Presence",width=1.0,ylab="Soil total C (g/kg)",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G7","I7","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pc

pc1<-facet(pc + theme_bw(), facet.by = "Depth",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Depth=c("0-10","10-30")))

ggpar(pc1,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

levels(Forests$Fire.Presence)
pc.1 <- ggboxplot(Forests, x = "Site", y = "TC_g_kg",
                color = "Fire.frequency",palette ="npg",
                add = "jitter", fill="lightgray",width=1.0,ylab="Soil total C (g/kg)",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G7","I7","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pc.1

pc1.1<-facet(pc.1 + theme_bw(), facet.by = "Fire.Presence",
           short.panel.labs = FALSE,   # Allow long labels in panels
           panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","NB")))

ggpar(pc1.1,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

pc2 <- ggboxplot(Forests, x = "Site", y = "TC_g_kg",
                color = "Depth",palette ="npg",
                add = "jitter", fill="lightgray",width=1.0,ylab="Total soil C (g/kg)",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G7","I7","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pc2

pc3<-facet(pc2 + theme_bw(), facet.by = "Fire.Presence",
           short.panel.labs = FALSE,   # Allow long labels in panels
           panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Depth=c("0-10","10-30")))

ggpar(pc3,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

pP <- ggboxplot(Forests, x = "Site", y = "M3P",
                 color = "Fire.frequency",palette ="npg",
                 add = "jitter", fill="lightgray",width=1.0,ylab="Soil Available P (microg/g)",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G7","I7","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pP

pP1<-facet(pP + theme_bw(), facet.by = "Fire.Presence",
           short.panel.labs = FALSE,   # Allow long labels in panels
           panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Depth=c("0-10","10-30")))

ggpar(pP1,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))


ggpar(pcn,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Fire presence",font.legend = c(20,"bold"))

pcn5 <- ggboxplot(Forests, x = "Site", y = "CN",
                color = "Fire.frequency",palette ="npg",
                add = "jitter", fill="lightgray",width=1.0,ylab="Soil total CN",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G7","I7","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Fire.Presence=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pcn5

pcn6<-facet(pcn5 + theme_bw(), facet.by = "Fire.Presence",
           short.panel.labs = FALSE,   # Allow long labels in panels
           panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","NB")))

ggpar(pcn6,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

names(Forests)
pN <- ggboxplot(Forests, x = "Site", y = "TN_g_kg",
                  color = "Fire.frequency",palette ="npg",
                  add = "jitter", fill="lightgray",width=1.0,ylab="Total N (g/kg)",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G7","I7","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Fire.Presence=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pN

pN1<-facet(pN + theme_minimal(), facet.by = "Fire.Presence",
            short.panel.labs = FALSE,   # Allow long labels in panels
            panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","NB")))

ggpar(pN,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))


pcn <- ggboxplot(Forests, x = "Site", y = "CN",
                color = "Fire.frequency",palette ="npg",
                add = "jitter", fill="lightgray",width=1.0,ylab="Total CN",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G7","I7","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pcn
ggpar(pcn,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))


str(Forests)
Forests$Fire.frequency=as.factor(Forests$Fire.frequency)
pcn1<-facet(pcn + theme_bw(), facet.by = "Depth",
           short.panel.labs = FALSE,   # Allow long labels in panels
           panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Depth=c("0-10","10-30")))

ggpar(pcn1,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

pcn2 <- ggboxplot(Forests, x = "Site", y = "CN",
                 color = "Fire.Class",palette ="npg",
                 add = "jitter", fill="lightgray",width=1.0,ylab="Total CN",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G7","I7","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Fire.Presence=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pcn2

pcn3<-facet(pcn2 + theme_bw(), facet.by = "Fire.Presence",
            short.panel.labs = FALSE,   # Allow long labels in panels
            panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","NB")))

ggpar(pcn3,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

pcn4 <- ggboxplot(Forests, x = "Site", y = "CN",
                  color = "Fire.Class",palette ="npg",
                  add = "jitter", fill="Depth",width=1.0,ylab="Total CN",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G7","I7","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Fire.Presence=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pcn4

pcn5<-facet(pcn4 + theme_bw(), facet.by = "Fire.Presence",
            short.panel.labs = FALSE,   # Allow long labels in panels
            panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","NB")))

ggpar(pcn5,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))


gamN<-lm(logN~Site*Fire.Presence,data=Forests)
summary(gamN)
anova(gamN)
TukeyHSD(gamN,"Site:Fire.Presence",ordered=TRUE)

leastsquareN =lsmeans(gamN,
                     pairwise ~ Site*Fire.Presence, 
                     adjust="tukey") 

Tukey.HSDN<-cld(leastsquareN,
               alpha=0.1,
               Letters=letters,      ### Use lower-case letters for .group
               adjust="tukey") 
Tukey.HSDN

gamC<-aov(TC_g_kg~Site*Fire.Presence,data=Forests)
summary(gamC)
anova(gamC)
TukeyHSD(gamC,"Site:Fire.Presence",ordered=TRUE)

leastsquareC =lsmeans(gamC,
                      pairwise ~ Site*Fire.Presence, 
                      adjust="tukey") 

Tukey.HSDC<-cld(leastsquareC,
                alpha=0.1,
                Letters=letters,      ### Use lower-case letters for .group
                adjust="tukey") 
Tukey.HSDC

gam_CN<-aov(CN~Site*Fire.Presence,data=Forests)
summary(gam_CN)
anova(gam_CN)
TukeyHSD(gam_CN,"Site:Fire.Presence",ordered=TRUE)

leastsquareCN =lsmeans(gam_CN,
                      pairwise ~ Site*Fire.Presence, 
                      adjust="tukey") 

Tukey.HSDCN<-cld(leastsquareCN,
                alpha=0.1,
                Letters=letters,      ### Use lower-case letters for .group
                adjust="tukey") 
Tukey.HSDCN

plot.gam(gamN,pages=1,shade=TRUE)
par(mfrow = c(2,2))
plot.gam(gamN, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Site", xlabs="Partial effect of Site on Soil C", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamN, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Fire presence", xlabs="Partial effect of Fire presence on Soil C", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamN, select=3, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
gam.check(gamc)

gamd6<-gam(logANF~s(logN),data=Forest6)
summary(gamd6)

gamd.1<-gam(logANF~Site+s(logN,by=oSite),data=Forests)
summary(gamd.1)
plot.gam(gamd6,pages=1,shade=TRUE)
plot.gam(gamd6,scheme=2,select=1,labcex=1.2,main="I-6",too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total N (log g/kg)",ylab="s(logN,1) * I-1",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)
mtext("(e)", side=3,cex=1.4,adj=0, line=0.5)
mtext("R2adj = 0.61, p <0.001", side=3,cex=1.0,adj=1, line=0.3)

gamd5<-gam(logANF~s(logN),data=Forest5)
summary(gamd5)
plot.gam(gamd5,scheme=2,select=1,labcex=1.2,main="I-1",too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total N (log g/kg)",ylab="s(logN,1) * I-1",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)
mtext("(b)", side=3,cex=1.4,adj=0, line=0.5)
mtext("p = 0.78", side=3,cex=1.0,adj=1, line=0.3)

names(Forests)
gamCN<-gam(logANF~Fire.Presence+s(CN,by=Fire.Presence),data=High)
summary(gamCN)
plot.gam(gamCN,pages=1,shade=TRUE,xlab="total CN",cex.lab=1.4,cex.axis=1.2)

plot(CN~Fire.frequency,data=Forests)
CN.a<-aov(CN~Fire.frequency,data=Forests)
summary(CN.a)

gamCNa<-gam(logANF~Fire.Presence+s(CN,by=Fire.Presence),data=Forests)
summary(gamCNa)
plot.gam(gamCNa,pages=1,shade=TRUE,xlab="total CN",cex.lab=1.4,cex.axis=1.2)


gamd.2<-gam(logN~Site+s(logANF,by=oSite),data=Forests)
summary(gamd.2)
plot.gam(gamd.2,pages=1,shade=TRUE)
plot.gam(gamd.2,scheme=2,select=1,labcex=1.2,main="I-1",too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total N (log g/kg)",ylab="s(logN,1) * I-1",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

gam1<-gam(logANF~Site+Fire.Presence+s(logN,by=Site)+s(log_pmn,by=Fire.Presence),data=Forests)
summary(gam1)#62.3%,r2adj=0.46
gam.check(gam1)
plot.gam(gam1,pages=1)

gam1a<-gam(logANF~Fire.Presence+s(log_pmn,logN,by=Fire.Presence),data=Forests)
summary(gam1a)#55.3%,r2adj=0.46
gam.check(gam1a)
plot.gam(gam1a,scheme=2,pages=1,too.far=10)

names(Forests)
gam1b<-gam(logANF~Fire.Presence+s(TC_g_kg,logN,by=Fire.Presence),data=Forests)
summary(gam1b)#55.3%,r2adj=0.39
gam.check(gam1b)
plot.gam(gam1b,scheme=2,pages=1,too.far=10)
par(mfrow = c(2,1))
plot.gam(gam1b,scheme=2,select=1,labcex=1.2,main="Burned",too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total C (g/kg)",ylab="Total N (log g/kg)",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)
mtext("(a)", side=3,cex=1.4,adj=0, line=0.5)
mtext("p = 0.0032", side=3,cex=1.0,adj=1, line=0.3)

plot.gam(gam1b,scheme=2,select=2,labcex=1.2,too.far=10,main="Unburned",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total C (g/kg)",ylab="Total N (log g/kg)",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)
mtext("(b)", side=3,cex=1.4,adj=0, line=0.5)
mtext("p = 0.048", side=3,cex=1.0,adj=1, line=0.3)


gam1c<-gam(logANF~Fire.Presence+s(CN,by=Fire.Presence),data=Forests)
summary(gam1c)#55.3%,r2adj=0.46
gam.check(gam1c)
plot.gam(gam1c,scheme=2,pages=1,too.far=10,shade=TRUE)
plot.gam(gam1c,scheme=2,select=1,labcex=1.2,main="Burned",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total C (g/kg)",ylab="Total N (log g/kg)",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)
mtext("(a)", side=3,cex=1.4,adj=0, line=0.5)
mtext("p < 0.0001", side=3,cex=1.0,adj=1, line=0.3)
AIC(gam1b,gam1c,gam1D,gam1e,gam1f)

names(Forests)
shapiro.test(Forests$lab.moisture_.)
Forests$logmoist=log10(Forests$lab.moisture_.)
shapiro.test(Forests$logmoist)

#THE BEST MODEL
levels(Forests$Depth)
Topsoil <-Forests[Forests$Depth == "0--10",]
names(Topsoil)

Bottom <-Forests[Forests$Depth == "10--30",]
Bottom

str(Forests)

gam1D<-gam(logANF~Fire.Presence+s(CN,logavP,by=Fire.Presence),data=Forests)
summary(gam1D)#58% deviance explained
plot.gam(gam1D,scheme=2,pages=1,too.far=10,shade=TRUE)

gam1D_depth<-gam(logANF~s(CN)+s(logavP),data=Topsoil)
summary(gam1D_depth)#61.1%
plot.gam(gam1D_depth,pages=1)

gam_anf<-aov(logANF~Site*Fire.Presence,data=Topsoil)
summary(gam_anf)
Anova(gam_anf,type="III")
TukeyHSD(gam_anf,"Fire.Presence",ordered=TRUE)#p=0.069
TukeyHSD(gam_anf,"Site",ordered=TRUE)#p=ns

lmCN<-lm(CN~Site*Fire.Presence*Fire.frequency,data=Forests)
summary(lmCN)
plot(CN~Fire.frequency*Site,data=Forests)

lmavP<-lm(logavP~Site*Fire.Presence*Fire.frequency,data=Forests)
summary(lmavP)
plot(M3P~Fire.frequency,data=Forests)

gam_anf2<-aov(logANF~Site*Fire.Presence,data=Bottom)
summary(gam_anf2)
Anova(gam_anf2,type="III")
TukeyHSD(gam_anf2,"Fire.Presence",ordered=TRUE)#p=ns
TukeyHSD(gam_anf2,"Site:Fire.Presence",ordered=TRUE)#p=ns


gamx<-gam(logANF~Fire.Presence+s(CN,log_pmn,by=Fire.Presence),data=Forests)
summary(gamx)#51.4%
plot.gam(gamx,pages=1,scheme=2,shade=TRUE,too.far=10)

AIC(gam1D,gam1Da,gamx)

gamM<-gam(logANF~Fire.Presence+s(CN,logmoist,by=Fire.Presence)+s(logM3P),data=Forests)
summary(gamM)#51.4%
plot.gam(gamM,pages=1,scheme=2,shade=TRUE,too.far=10)


lmP<-lm(CN~log_pmn,data=Forests)
summary(lmP)
plot(logM3P~log_pmn,data=Forests)

par(mfrow = c(1,2))
plot.gam(gam1D,scheme=2,select=1,labcex=1.2,main="Burned",too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total CN",ylab="Available P (log microg/g)",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)
mtext("(a)", side=3,cex=1.4,adj=0, line=0.5)
mtext("p < 0.001", side=3,cex=1.4,adj=1, line=0.3)

plot.gam(gam1D,scheme=2,select=2,labcex=1.2,too.far=10,main="Unburned",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total CN",ylab="Available P (log microg/g)",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)
mtext("(b)", side=3,cex=1.4,adj=0, line=0.5)
mtext("p = 0.02", side=3,cex=1.4,adj=1, line=0.3)


gam2<-gam(CN~Fire.Presence*Site,data=Forests)
summary(gam2)#73.6%, interaction significant
anova(gam2)
plot.gam(gam2.1,scheme=2,pages=1,labcex=1.2,too.far=10,main="Unburned",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total CN",ylab="Available P (log microg/g)",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

gam2.1<-gam(CN~Fire.Presence*Site*Depth,data=Forests)
summary(gam2.1)
anova(gam2.1)
AIC(gam2,gam2.1)

gam2.1a<-gam(CN~Fire.Presence*Depth*Fire.Class,data=Forests)
summary(gam2.1a)
AIC(gam2,gam2.1,gam2.1a)

gam2a<-gam(logM3P~Fire.Presence*Site,data=Forests)
summary(gam2a)#56.8%
plot.gam(gam2a,scheme=2,pages=1,labcex=1.2,too.far=10,main="Unburned",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total CN",ylab="Available P (log microg/g)",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

gamP<-gam(logM3P~Fire.Presence+s(CN,by=Fire.Presence),data=Forests)
summary(gamP)#26.2%
gam.check(gamP)
plot.gam(gamP,scheme=2,pages=1,labcex=1.2,too.far=10,main="Unburned",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total CN",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

plot.gam(gamP,scheme=2,select=1,labcex=1.2,main="Burned",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total CN",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)
mtext("(a)", side=3,cex=1.4,adj=0, line=0.5)
mtext("p = 0.016", side=3,cex=1.4,adj=1, line=0.3)

plot.gam(gamP,scheme=2,select=2,labcex=1.2,too.far=10,main="Unburned",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total CN",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)
mtext("(b)", side=3,cex=1.4,adj=0, line=0.5)
mtext("p = 0.015", side=3,cex=1.4,adj=1, line=0.3)

gamP2<-gam(logM3P~Fire.Presence+Depth+s(CN,by=Fire.Presence)+s(CN,by=Depth),data=Forests)
summary(gamP2)#39.4%
plot.gam(gamP2,pages=1,scheme=2,too.far=10)
gam.check(gamP)

gamP3<-gam(logM3P~Depth+Fire.Presence*Site,data=Forests)
summary(gamP3)#50.7%
gam.check(gamP3)
plot.gam(gamP3,scheme=2,pages=1,labcex=1.2,too.far=10,main="Unburned",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total CN",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

gamP4<-gam(logM3P~s(CN)+Site,data=Forests)
summary(gamP4)#50.7%
gam.check(gamP3)
plot.gam(gamP4,scheme=2,pages=1,labcex=1.2,too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total CN",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

gamP5<-gam(logM3P~Fire.Presence+s(TC_g_kg,by=Fire.Presence),data=Forests)
summary(gamP5)
plot.gam(gamP5,scheme=2,pages=1,labcex=1.2,too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total C",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

gamP6<-gam(logM3P~Site+Fire.Presence+Depth,data=Forests)
summary(gamP6)
plot.gam(gamP6,scheme=2,pages=1,labcex=1.2,too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="Total C",shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

avP<-lm(logavP~Site*Fire.Presence,data=Forests)
summary(avP)
TukeyHSD(avP,"Site:Fire.Presence",ordered=TRUE)

leastsquareP =lsmeans(avP,
                      pairwise ~ Site*Fire.Presence, 
                      adjust="tukey") 

Tukey.HSDP<-cld(leastsquareP,
                alpha=0.1,
                Letters=letters,      ### Use lower-case letters for .group
                adjust="tukey") 
Tukey.HSDP

avP2<-lm(logM3P~Site*Fire.Presence*Depth,data=Forests)
summary(avP2)

leastsquare =lsmeans(avP2,
                     pairwise ~ Site*Fire.Presence*Depth, 
                     adjust="tukey") 

Tukey.HSD<-cld(leastsquare,
               alpha=0.1,
               Letters=letters,      ### Use lower-case letters for .group
               adjust="tukey") 
Tukey.HSD

CN2<-lm(CN~Site*Fire.Presence,data=Forests)
summary(CN2)

CN3<-lm(CN~Site*Fire.Presence*Depth,data=Forests)
summary(CN3)

leastsquare =lsmeans(CN2,
                     pairwise ~ Site*Fire.Presence, 
                     adjust="tukey") 

Tukey.HSD<-cld(leastsquare,
               alpha=0.1,
               Letters=letters,      ### Use lower-case letters for .group
               adjust="tukey") 
Tukey.HSD

N<-lm(logN~Site*Fire.Presence*Depth,data=Forests)
summary(N)

leastsquare =lsmeans(N,
                     pairwise ~ Site*Fire.Presence*Depth, 
                     adjust="tukey") 

Tukey.HSD<-cld(leastsquare,
               alpha=0.1,
               Letters=letters,      ### Use lower-case letters for .group
               adjust="tukey") 
Tukey.HSD

gam1e<-gam(logANF~Site+s(CN,by=Site),data=Forests)
summary(gam1e)#59.2%
plot.gam(gam1e,scheme=2,pages=1,labcex=1.2,too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T,shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

names(pea2)
gam1f<-gam(logANF~s(logavP,CN),data=pea2)
summary(gam1f)#59.2%
plot.gam(gam1f,scheme=2,pages=1,labcex=1.2,too.far=10,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T,shade=TRUE,shade.col='gray90',rug=TRUE,contour.col="black",nCol=50, adj=0.5)

par(mfrow=c(1,2))
gam1.1<-gam(logN~Depth+s(logANF,log_pmn,by=Depth),data=Forests)
summary(gam1.1)#52.4%
plot.gam(gam1.1, select=1, scheme=2,too.far=10,cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, scheme=2,too.far=10,cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1,pages=1)
gam.check(gam1.1)

gam1.2<-gam(logN~Fire.Presence+s(logANF,log_pmn,by=Fire.Presence),data=Forests)
summary(gam1.2)#40.7%
plot.gam(gam1.2, select=1, scheme=2,too.far=10,cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.2, select=2, scheme=2,too.far=10,cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1,pages=1)

AIC(gam1.1,gam1.2)

gamx<-gam(logANF~Fire.Scar*Depth,data=Forests)
summary(gamx)

gam1.2<-gam(logN~Site*Fire.Scar+Fire.Scar+s(log_pmn,by=Fire.Scar),data=Forests)
summary(gam1.2)#57.4%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.2,pages=1)

#FITTING PER FOREST
#FOREST 6 IS SIGNIFICANT
gam1.2a<-gam(logN~s(logANF),data=Forest6)
summary(gam1.2a)#78.5%
plot.gam(gam1.2a,rug=FALSE,pages=1,shade=TRUE,cex.main=1.6,main="I-6",xlab="ANF (log ng N gdw-1 h-1)",cex.lab=1.4,cex.axis=1.2)
mtext("(e)", side=3,cex=1.4,adj=0, line=0.5)
mtext("adj R2 = 0.74, p <0.0001", side=3,cex=1.2,adj=1, line=0.3)
gam.check(gam1.2a)
identify(gam1.2a, "Site", labels=n,col="blue",cex=1.7)
names(Forests)

lmN<-aov(logN~Site*Fire.Presence,data=Forests)
summary(lmN)
TukeyHSD(lmN,"Site:Fire.Presence",ordered=TRUE)

gam1.2b<-gam(logN~s(logANF),data=Forest5)
summary(gam1.2b)#78.5%
plot.gam(gam1.2b,rug=FALSE,pages=1,shade=TRUE,cex.main=1.6,main="I-5",xlab="ANF (log ng N gdw-1 h-1)",cex.lab=1.4,cex.axis=1.2)
mtext("(d)", side=3,cex=1.4,adj=0, line=0.5)
mtext("adj R2 = 0.19, p = 0.87 ", side=3,cex=1.2,adj=1, line=0.3)
gam.check(gam1.2a)
identify(gam1.2a, "Site", labels=n,col="blue",cex=1.7)
names(Forests)

gam1.2c<-gam(logN~s(logANF),data=Forest2)
summary(gam1.2c)#78.5%
plot.gam(gam1.2c,rug=FALSE,pages=1,shade=TRUE,cex.main=1.6,main="I-2",xlab="ANF (log ng N gdw-1 h-1)",cex.lab=1.4,cex.axis=1.2)
mtext("(c)", side=3,cex=1.4,adj=0, line=0.5)
mtext("adj R2 = 0.05, p = 0.43 ", side=3,cex=1.2,adj=1, line=0.3)
gam.check(gam1.2a)
identify(gam1.2a, "Site", labels=n,col="blue",cex=1.7)
names(Forests)

gam1.2d<-gam(logN~s(logANF),data=Forest1)
summary(gam1.2d)#78.5%
plot.gam(gam1.2d,rug=FALSE,pages=1,shade=TRUE,cex.main=1.6,main="I-1",xlab="ANF (log ng N gdw-1 h-1)",cex.lab=1.4,cex.axis=1.2)
mtext("(b)", side=3,cex=1.4,adj=0, line=0.5)
mtext("adj R2 = 0.04, p = 0.47 ", side=3,cex=1.2,adj=1, line=0.3)
gam.check(gam1.2a)
identify(gam1.2a, "Site", labels=n,col="blue",cex=1.7)
names(Forests)

gam1.2e<-gam(logN~s(logANF),data=Forest3)
summary(gam1.2e)#78.5%
plot.gam(gam1.2e,rug=FALSE,pages=1,shade=TRUE,cex.main=1.6,main="I-3",xlab="ANF (log ng N gdw-1 h-1)",cex.lab=1.4,cex.axis=1.2)
mtext("(a)", side=3,cex=1.4,adj=0, line=0.5)
mtext("adj R2 = 0.63, p = 0.02 ", side=3,cex=1.2,adj=1, line=0.3)
gam.check(gam1.2a)
identify(gam1.2a, "Site", labels=n,col="blue",cex=1.7)
names(Forests)

gam_6<-gam(logANF~s(CN),data=Forest6)
summary(gam_6)#82.5%
plot.gam(gam_6,rug=FALSE,pages=1,shade=TRUE,xlab="CN",cex.lab=1.4,cex.axis=1.2)

gam_6a<-gam(logANF~s(TC_g_kg),data=Forest6)
summary(gam_6a)#82.5%
plot.gam(gam_6a,rug=FALSE,pages=1,shade=TRUE,xlab="CN",cex.lab=1.4,cex.axis=1.2)

#TRYING FIRE CLASS
str(High)
gamclass<-gam(logANF~Depth+s(logN,by=Depth),data=High)
summary(gamclass)#35.8%
plot.gam(gamclass,rug=FALSE,pages=1,shade=TRUE,xlab="ANF (log ng N gdw-1 h-1)",cex.lab=1.4,cex.axis=1.2)

gam1.2b<-gam(logN~Fire.Presence+s(logANF),data=Forest3)
summary(gam1.2b)#75.2%
plot.gam(gam1.2b,rug=FALSE,pages=1,shade=TRUE,xlab="ANF (log ng N gdw-1 h-1)",cex.lab=1.4,cex.axis=1.2)
mtext("(I-3)", side=3,cex=1.4,adj=0, line=0.5)
mtext("adj R2 = 0.63, p = 0.02", side=3,cex=1.2,adj=1, line=0.3)
gam.check(gam1.2a)
identify(gam1.2a, "Site", labels=n,col="blue",cex=1.7)
names(Forests)

names(Forests)
plot(logANF~logN,data=Forests)
abline(lm(logANF~logN,data=Forests))
summary(lm(logANF~logN,data=Forests))
plot(logANF~CN,data=Forests)

lmanf<-gam(logANF~s(logN),data=Forests)
plot(lmanf,shade=TRUE)
summary(lmanf)

gamN2<-gam(logANF~s(logN),data=Forests)
summary(gamN2)#significant
plot.gam(gamN2, shade=TRUE,pages=1,se=T)

gamN3<-gam(logN~s(logANF),data=Forests)
summary(gamN3)#significant
plot.gam(gamN3, shade=TRUE,pages=1,se=T)
mtext("p=0.06", side=3,cex=1.2,adj=0.02, line=0.3,col="black")

names(Forests)
gamN4<-gam(TC_g_kg~s(logANF),data=Forests)
summary(gamN4)#significant
plot.gam(gamN4, shade=TRUE,pages=1,se=T)
mtext("p=0.04", side=3,cex=1.2,adj=0.02, line=0.3,col="black")


gam1.2b<-gam(logN~Site+s(logANF,by=Site)+s(log_pmn),data=Forests)
summary(gam1.2b)#68.6%
plot.gam(gam1.2b,pages=1)
gam.check(gam1.2b,pages=1)

gam1.2b<-gam(logN~Fire.Scar+s(logANF,log_pmn,by=Fire.Scar),data=Forests)
summary(gam1.2b)#50.9%
plot.gam(gam1.2b,pages=1,scheme=2)
gam.check(gam1.2b,pages=1)

gam1.2c<-gam(logN~Site+Depth+s(logANF,by=Site)+s(logANF,by=Depth),data=Forests)
summary(gam1.2c)#68.6%
plot.gam(gam1.2b,pages=1)
gam.check(gam1.2b,pages=1)

gamN<-gam(logN~Site*Depth*Fire.Scar,data=Forests)
summary(gamN)
plot.gam(gamN,pages=1)
plot.gam(gamN, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Site", xlabs="Forest Site", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamN, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamN, select=3, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)

library(ggplot2)
library(ggpubr)
library(magrittr)

names(Forests)
Forests$Fire.frequency=as.factor(Forests$Fire.frequency)
str(Forests)
levels(Forests$Fire.Presence)

pN <- ggboxplot(Forests, x = "Site", y = "TN_g_kg",
                color = "Fire.frequency",shape="Fire.Presence",palette ="npg",fill="lightgray",
                add = "jitter", width=1.0,ylab="Total soil N (g/kg)",short.panel.labs = FALSE,order=c("I3","I1","I2","I5", "I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Fire.Presence=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pN

pN2<-facet(pN + theme_bw(), facet.by = "Fire.Presence",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","NB")))

ggpar(pN,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))


gama<-gam(logANF~Fire.Scar+s(log_pmn,logN,by=Fire.Scar),data=Forests)
summary(gama)#47.4%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gama,pages=1,scheme=2)#cool graphs

Forests$Fire.legacy=as.factor(Forests$Fire.legacy)
gamb<-gam(logANF~Fire.legacy+s(log_pmn,by=Fire.legacy),data=Forests)
summary(gamb)#44.4%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamb,pages=1,scheme=2)

gamb<-gam(logANF~Site+s(log_pmn,by=Site),data=Forests)
summary(gamb)#46.9%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamb,pages=1)

names(Forests)
gamc<-gam(logANF~Fire.Scar+s(log_pmn,logNP,by=Fire.Scar),data=Forests)
summary(gamc)#40.9%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamc,pages=1,scheme=2)

gamd<-gam(logANF~Fire.Scar+s(log_pmn,by=Fire.Scar),data=Forests)
summary(gamd)#40.9%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamd,pages=1,scheme=2)

game<-gam(logN~Fire.Scar+Depth+s(logANF,by=Fire.Scar)+s(logANF,by=Depth),data=Forests)
summary(game)#36.1%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(game,pages=1,scheme=2)

gamf<-gam(logN~Depth*Fire.Scar+s(logANF,by=Depth)+s(logANF,by=Fire.Scar),data=Forests)
summary(gamf)#36.5%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamf,pages=1,scheme=2,shade=TRUE)#INTERESTING!

gamg<-gam(logN~Fire.Scar+s(logANF,log_pmn,by=Fire.Scar),data=Forests)
summary(gamg)#35.5%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamg,pages=1,scheme=2,shade=TRUE)#INTERESTING!

shapiro.test(Forests$TC_g_kg)#normal
gamh<-gam(logANF~Fire.Scar+s(TC_g_kg,by=Fire.Scar),data=Forests)
summary(gamh)#17.8%
plot.gam(gam1.1, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="", xlabs="", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gam1.1, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
plot.gam(gamh,pages=1,scheme=2,shade=TRUE)#INTERESTING!

gami<-gam(logANF~Fire.legacy+s(TC_g_kg,by=Fire.legacy),data=Forests)
summary(gami)#25.1%
plot.gam(gami,pages=1,scheme=2,shade=TRUE)#INTERESTING!

gamj<-gam(logANF~Site+s(TC_g_kg,by=Site),data=Forests)
summary(gamj)#29%
plot.gam(gamj,pages=1,scheme=2,shade=TRUE)#INTERESTING!

gamN<-gam(logN~Fire.legacy*Fire.Scar*Depth,data=Forests)
summary(gamN)#62.6%
plot(gamN,pages=1)
plot.gam(gamN, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)

gamN2<-gam(logN~Site*Fire.Scar*Depth,data=Forests)
summary(gamN2)#67.3%
plot(gamN,pages=1)
plot.gam(gamN2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)


gam1a<-gam(logANF~Vegetation+s(logN,by=Vegetation),data=pea)
summary(gam1a)#25.1%
plot.gam(gam1a,pages=1)

gam1.1<-gam(logANF~Site+s(logN,by=Site),data=Forests)
summary(gam1.1)#19.6% and only significance was in site most burnt
plot.gam(gam1.1,pages=1,shade=TRUE)

gam1.2<-gam(log_anf~Site+s(CN,by=Site),data=Forests)
summary(gam1.2)
plot.gam(gam1.2,pages=1,shade=TRUE)#57.6%

gam1.2a<-gam(logANF~Site+s(CN,by=Site),data=pea)
summary(gam1.2a)
plot.gam(gam1.2a,pages=1,shade=TRUE)#37.1%

gam1.3<-gam(log_anf~Fire.legacy+s(CN,by=Fire.legacy),data=Forests)
summary(gam1.3)#55.7%
plot.gam(gam1.3,pages=1,shade=TRUE)

gam1.3a<-gam(log_anf~Depth+Fire.legacy+s(CN,by=Fire.legacy)+s(CN,by=Depth),data=Forests)
summary(gam1.3a)#57.5%
plot.gam(gam1.3a,pages=1,shade=TRUE)

gam1.3b<-gam(log_anf~Cluster+Fire.legacy+s(CN,by=Fire.legacy)+s(CN,by=Cluster),data=Forests)
summary(gam1.3b)#55.7%
plot.gam(gam1.3b,pages=1,shade=TRUE)

gam1.3c<-gam(log_anf~Fire.legacy+Cluster+s(logNP,by=Fire.legacy)+s(CN,by=Cluster),data=Forests)
summary(gam1.3c)#57.1% good model!
plot.gam(gam1.3c,pages=1,shade=TRUE)

gam1.3d<-gam(log_anf~Fire.legacy+Cluster+s(logPMN,by=Fire.legacy)+s(logNP,by=Cluster),data=Forests)
summary(gam1.3d)#47.2%
plot.gam(gam1.3d,pages=1,shade=TRUE)

gam1.3e<-gam(log_anf~Fire.legacy+Depth+s(logPMN,by=Fire.legacy)+s(logPMN,by=Depth),data=Forests)
summary(gam1.3e)#44.7%
plot.gam(gam1.3e,pages=1,shade=TRUE)

gam1.3.e<-gam(logANF~Fire.legacy+Vegetation+s(log_pmn,by=Fire.legacy)+s(log_pmn,by=Vegetation),data=pea)
summary(gam1.3.e)#25.5%
plot.gam(gam1.3.e,pages=1,shade=TRUE)

gam1.3f<-gam(log_anf~Fire.legacy+Cluster+s(logM3Fe,by=Fire.legacy)+s(logM3Fe,by=Cluster),data=Forests)
summary(gam1.3f)#18.7%
plot.gam(gam1.3f,pages=1,shade=TRUE)

gam1.3g<-gam(log_anf~Fire.legacy+Cluster+s(logFe.P,by=Fire.legacy)+s(logFe.P,by=Cluster),data=Forests)
summary(gam1.3g)#13.2% 
plot.gam(gam1.3g,pages=1,shade=TRUE)

gam1.3h<-gam(log_anf~Fire.legacy+Cluster+s(TP.g_kg,by=Fire.legacy)+s(TP.g_kg,by=Cluster),data=Forests)
summary(gam1.3h)#19.2% 
plot.gam(gam1.3h,pages=1,shade=TRUE)

gam2<-gam(log_anf~Fire.legacy+Depth+s(CN,by=Fire.legacy)+s(logPMN,by=Depth),data=Forests)
summary(gam2)#54.4% best model so far, PMN affected by Depth
plot.gam(gam2,pages=1,shade=TRUE)

gam2.1<-gam(log_anf~Fire.legacy+Depth+s(logPMN,by=Fire.legacy)+s(CN,by=Depth),data=Forests)
summary(gam2.1)#47.8% best model so far, CN not affected by Depth
plot.gam(gam2.1,pages=1,shade=TRUE)

gam2a<-gam(log_anf~s(CN)+s(logPMN),data=Forests)
summary(gam2a)#9.63%
plot.gam(gam2a,pages=1,shade=TRUE)

gam2b<-gam(log_anf~Fire.legacy+s(CN,logPMN),data=Forests)
summary(gam2b)#44.4%
plot.gam(gam2b,shade=TRUE)

gam3<-gam(log_anf~Fire.legacy+Cluster+s(CN,by=Fire.legacy)+s(logPMN,by=Cluster),data=Forests)
summary(gam3)#47.3%
plot.gam(gam3,pages=1,shade=TRUE)

gam4<-gam(log_anf~Fire.legacy+Cluster+Depth+s(CN,by=Fire.legacy)+s(logPMN,by=Cluster)+s(logPMN,by=Depth),data=Forests)
summary(gam4)#55.1%
plot.gam(gam4,pages=1,shade=TRUE)

gam5<-gam(log_anf~Fire.legacy+Cluster+Depth+s(logPMNAVP,by=Fire.legacy)+s(CN,by=Cluster)+s(logPMN,by=Depth),data=Forests)
summary(gam5)#60.9%
plot.gam(gam5,pages=1,shade=TRUE)

gam6<-gam(log_anf~Fire.legacy+s(logPMNAVP,by=Fire.legacy),data=Forests)
summary(gam6)#28%
plot.gam(gam6,pages=1,shade=TRUE)

gam7<-gam(log_anf~Fire.legacy+Cluster+s(logCP,by=Fire.legacy)+s(logPMN,by=Cluster),data=Forests)
summary(gam7)#51%
plot.gam(gam7,pages=1,shade=TRUE)

gam7a<-gam(log_anf~Fire.legacy+Cluster+s(logPMN,by=Fire.legacy)+s(logCP,by=Cluster),data=Forests)
summary(gam7a)#37.1%
plot.gam(gam7a,pages=1,shade=TRUE)

gam7b<-gam(log_anf~Fire.legacy+Cluster+s(logCP,by=Fire.legacy)+s(logmoisture,by=Cluster),data=Forests)
summary(gam7b)#39.1%
plot.gam(gam7b,pages=1,shade=TRUE)

gam7.1<-gam(log_anf~Cluster+s(logCP,PMN,by=Cluster),data=Forests)
summary(gam7.1)#39.1% significant in cluster 2
plot.gam(gam7.1,pages=1,scheme=2,shade=TRUE)

gam8<-gam(log_anf~Fire.legacy+s(TC_g_kg,by=Fire.legacy),data=Forests)
summary(gam8)#15.9% C significant for legacy of 5 years
plot.gam(gam8,pages=1,scheme=2,shade=TRUE)

gam8.1<-gam(log_anf~Fire.legacy+Cluster+s(TC_g_kg,by=Fire.legacy)+s(TC_g_kg,by=Cluster),data=Forests)
summary(gam8.1)#30.2%
plot.gam(gam8.1,pages=1,scheme=2,shade=TRUE)

gam9<-gam(log_anf~Fire.legacy+s(logCfe,by=Fire.legacy),data=Forests)
summary(gam9)#15.5% significant for legacy of 5 years
plot.gam(gam9,pages=1,scheme=2,shade=TRUE)

gam9<-gam(log_anf~Fire.legacy+s(logCfe,by=Fire.legacy),data=Forests)
summary(gam9)#15.5% significant for legacy of 5 years
plot.gam(gam9,pages=1,scheme=2,shade=TRUE)


#Trying the best model with whole dataset pea => first need to transform variables

GAM1<-gam(logANF~Fire.legacy+s(CN,log_pmn,by=Fire.legacy),data=pea)
summary(GAM1)#26.3%
plot.gam(GAM1,shade=TRUE, scheme=2, pages=1)

GAM2<-gam(logANF~Fire.legacy+s(CN,log_pmn,by=Vegetation),data=pea)
summary(GAM2)#26.3%
plot.gam(GAM2,shade=TRUE, scheme=2, pages=1,too.far=5)

#Doing ordination to see sites against soil properties

library(nlme)
library(mgcv)

names(pea2)
options(max.print=1000000)
corr_matrix_pea<-(corr.test(pea2[8:28],use = "pairwise",method="spearman",ci=TRUE,alpha=.05))
corr_matrix_pea

peaanf.pca <- rda(peaANF[6:14])#with covariances => “explain” only the
summary(peaanf.pca)

#check if rep is significant => no

#using dataset with mean values per cluster, site, depth

#peaANF
names(peaANF)

grass <-peaANF[peaANF$Vegetation == "grassland",]
grass

forests <-peaANF[peaANF$Vegetation == "forest",]
forests

corr_matrix<-(corr.test(peaANF[6:14],use = "pairwise",method="pearson",ci=TRUE,alpha=.05))
corr_matrix

corr_matrix2<-(corr.test(forests[6:14],use = "pairwise",method="pearson",ci=TRUE,alpha=.05))
corr_matrix2

par(mfrow=c(1,1))
peasoil.mds<-metaMDS(forests[6:14], distance = "bray", k = 2, trymax = 20, autotransform =TRUE, noshare = 0.1, expand = TRUE, trace = 1, plot = FALSE) #makes the object bci.mds using Bray-Curtis ordination
summary(peasoil.mds)
plot(peasoil.mds, type="n") #plots the ordination axes
points(peasoil.mds, display = "sites", col="black",cex=1.6)#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
text(peasoil.mds,display="sites",col="red")
points(peasoil.mds, display = "sites", col="lightblue",cex=1.4)#displays both sites and species on the same plot.  Try choosing just “sites” to reduce clutter
identify(bci.mds4, "sites", labels=sites,col="blue",cex=1.7)
sites<- c("G1","G2","G3","G5","G6","I7","I1","I2","I3","I5","I6","I7")
sites<-peaANF[1]
n

gamall<-gam(mean_ANF~fire.scar+s(M3P,by=fire.scar),data=peaANF)
summary(gamall)#19.9% and significant in scar 1 = unburnt
plot.gam(gamall,shade=TRUE, scheme=2, pages=1)

gamalla<-gam(mean_ANF~fire.scar+s(M3P,by=fire.scar),data=peaANF)
summary(gamalla)#19.9% and significant in scar 1 = unburnt
plot.gam(gamalla,shade=TRUE, scheme=2, pages=1)

gamall<-gam(mean_ANF~SITE+s(M3P,by=),data=peaANF)
summary(gamallA)#19.9% and significant in scar 1 = unburnt
plot.gam(gamall,shade=TRUE, scheme=2, pages=1)

gamall2<-gam(mean_ANF~fire.scar+s(PMN,by=fire.scar),data=peaANF)
summary(gamall2)#30.3% and significant in both fire scars
plot.gam(gamall2,shade=TRUE, scheme=2, pages=1)

gamall3<-gam(mean_ANF~fire.scar+s(pH_H2O,by=fire.scar),data=peaANF)
summary(gamall3)#49.2% and significant in both fire scars
plot.gam(gamall3,shade=TRUE, scheme=2, pages=1)

gamall3.1<-gam(PMN~fire.scar+s(pH_H2O,by=fire.scar),data=peaANF)
summary(gamall3.1)#46% and significant in both fire scars
plot.gam(gamall3.1,shade=TRUE, scheme=2, pages=1)

gamall3.2<-gam(pH_H2O~fire.scar*SITE,data=peaANF)
summary(gamall3.2)#73.6% and significant in both fire scars
anova(gamall3.2)#site is significant

gamall3.3<-gam(PMN~fire.legacy+s(pH_H2O,by=fire.legacy),data=peaANF)
summary(gamall3.3)#46% and significant in both fire scars
plot.gam(gamall3.3,shade=TRUE, scheme=2, pages=1)

gamall4<-gam(mean_ANF~fire.legacy+s(pH_H2O,by=fire.legacy),data=peaANF)
summary(gamall4)#49.2% and significant in both fire scars
plot.gam(gamall4,shade=TRUE, scheme=2, pages=1)

gamph<-gam(logph~SITE*fire.scar, data=peaANF)
summary(gamph)#74.5% of variance explained
plot(gamph, pages=1,residuals=TRUE)
shapiro.test(resid(gamph))#NORMAL

gamph2<-gam(logPH~fire.legacy*fire.scar, data=forests)
summary(gamph2)#50% of variance explained
plot.gam(gamph2, pages=1, all.terms=TRUE, shade=TRUE,seWithMean=TRUE)
shapiro.test(resid(gamph2))#NORMAL

gamNP<-gam(logNP~SITE*fire.scar, data=peaANF)
summary(gamNP)#89.8%
shapiro.test(resid(gamNP))#NORMAL
plot.gam(gamNP, pages=1, all.terms=TRUE, shade=TRUE,seWithMean=TRUE)

str(peaANF)
gamNP2<-gam(logNP~fire.legacy+s(logph,by=fire.legacy), data=peaANF)
summary(gamNP2)#72%
plot.gam(gamNP2, pages=1, all.terms=TRUE, shade=TRUE,seWithMean=TRUE)

shapiro.test(resid(gamNP2))#NORMAL

peaANF$logNP=log10(peaANF$NP)
shapiro.test(peaANF$logNP)

peaANF$logph=log10(peaANF$pH_H2O)
shapiro.test(peaANF$logph)

forests$logPH=log10(forests$pH_H2O)
shapiro.test(forests$logPH)

#MOISTURE
names(Forests)
shapiro.test(Forests$lab.moisture_.)
Forests$logmois=log10(Forests$lab.moisture_.)
shapiro.test(Forests$logmois)#NORMAL

plot(logANF~lab.moisture_.,data=Forests)
gam_mois<-gam(logANF~Fire.Presence+s(logmois,by=Fire.Presence),data=Forests)
summary(gam_mois)
plot.gam(gam_mois,pages=1,shade=TRUE)

gam_C<-gam(TC_g_kg~Fire.Presence+s(logmois,by=Fire.Presence),data=Forests)
summary(gam_C)
plot.gam(gam_C,pages=1,shade=TRUE)

#SOIL VARIABLES IMPORTANT FOR VEGETATION
names(Forests)
correlation<-Forests[c(10:11,13,18:19,22,24,26,27)]
names(correlation)

corr_matrix_forests<-(corr.test(correlation,use = "pairwise",method="spearman",ci=TRUE,alpha=.05))
corr_matrix_forests

#graphs
names(Forests)

soil_all5 <- ggscatter(Forests, x = "CN", y = "M3P",
                       palette ="npg",color="Fire.frequency",shape="Fire.Presence",
                       ylab="Tree height (m)",cex=20,short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
soil_all5
ggpar(soil_all5,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Tree basal area (m2)",font.legend = c(20,"bold"))

